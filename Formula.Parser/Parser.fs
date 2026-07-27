//-----------------------------------------------------------------------
// <copyright file="Parser.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module Parser =
    open System

    open FParsec
    open Formula.Parser.Ast

    let push item stack = item :: stack

    let pop stack =
        match stack with
        | [] -> None, stack
        | item :: newStack -> Some item, newStack

    let peek stack =
        match stack with
        | [] -> None
        | item :: newStack -> Some item
    
    let str s = pstring s
    let ws = spaces
    let str_ws s = str s .>> ws

    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    let keyword s = attempt (pstring s .>> notFollowedBy (satisfy isIdentifierChar)) .>> ws

    let pUnicodeHex =
        (manyMinMaxSatisfy 4 4 isHex <?> "a Unicode scalar value")
    let toCharOrSurrogatePair p =
        p |> withSkippedString (fun codePoint _ -> Int32.Parse(codePoint, System.Globalization.NumberStyles.HexNumber) |> Char.ConvertFromUtf32)
    
    let pBackslashEscape =
        anyOf "'\"\\0abfnrtv"
        |>> function
            | ''' -> "'"
            | '"' -> "\""
            | '\\' -> "\\"
            | '0' -> "\x00"
            | 'a' -> "\a"
            | 'b' -> "\b"
            | 'f' -> "\f"
            | 'n' -> "\n"
            | 'r' -> "\r"
            | 't' -> "\t"
            | 'v' -> "\v"
            | _ -> invalidOp "Mismatched escape sequence parser pattern."
    
    let pUnicodeEscape = (pchar 'u' >>. (pUnicodeHex |> toCharOrSurrogatePair))
    
    let pEscapedChar = pstring "\\" >>. (pBackslashEscape <|> pUnicodeEscape)
    let isBasicStrChar c = c <> '\\' && c <> '"' && c > '\u001f' && c <> '\u007f'
    let pBasicStrChars = manySatisfy isBasicStrChar
    
    let adjustPosition offset (pos: Position) =
        Position(pos.StreamName, pos.Index + int64 offset,
                 pos.Line, pos.Column + int64 offset)

    let getInfixOperator str prec assoc mapping =
        InfixOperator(str, getPosition .>> ws, prec, assoc, (),
                               fun opPos leftTerm rightTerm ->
                                   mapping
                                       ((adjustPosition -str.Length opPos), opPos)
                                       leftTerm rightTerm)

    let getPrefixOperator str prec isAssoc mapping =
        PrefixOperator(str, getPosition .>> ws, prec, isAssoc, (),
                               fun opPos term ->
                                   mapping
                                       ((adjustPosition -str.Length opPos), opPos)
                                       term)

    let getOperatorItem op p =
        { Item = op; StartPosition = fst p; EndPosition = snd p } :> IPositionedAstItem<'a>

    let getInfixAst (kind: (IAstItem<'a> * IAstItem<'b> -> 'a0)) x y p =
        { Item = kind(x :> IPositionedAstItem<'a>, y :> IPositionedAstItem<'b>); StartPosition = x.StartPosition; EndPosition = y.EndPosition } :> IPositionedAstItem<'a0>
    
    let getInfixOperatorAst (kind: (IAstItem<'a> * IAstItem<'b> * IAstItem<'c> -> 'a0)) x y op p =
        let opItem = getOperatorItem op p
        { Item = kind(x :> IPositionedAstItem<'a>, opItem, y :> IPositionedAstItem<'c>); StartPosition = x.StartPosition; EndPosition = y.EndPosition } :> IPositionedAstItem<'a0>

    let getPrefixAst (kind: (IAstItem<'a> -> 'a0)) x p =
        { Item = kind(x :> IPositionedAstItem<'a>); StartPosition = fst p; EndPosition = x.EndPosition } :> IPositionedAstItem<'a0>
    
    let pnumber = pfloat |>> Number
    let pboolean = (keyword "true" >>% Boolean(true)) <|> (keyword "false" >>% Boolean(false))
    let ptext =
        stringsSepBy pBasicStrChars pEscapedChar |> between (str_ws "\"") (str_ws "\"") <?> "text" |>> Text
    let pnothing = (keyword "null" >>% Nothing)

    let wrapPos<'a> (parser) = pipe3 getPosition parser getPosition (fun s expr e -> { Item = expr; StartPosition = s; EndPosition = e; } :> IPositionedAstItem<'a>) 

    let pconstant =
        (wrapPos pnumber |>> (fun x -> Constant(x :> IAstItem<value>)))
        <|> (wrapPos pboolean |>> (fun x -> Constant(x :> IAstItem<value>)))
        <|> (wrapPos ptext |>> (fun x -> Constant(x :> IAstItem<value>)))
        <|> (wrapPos pnothing |>> (fun x -> Constant(x :> IAstItem<value>)))

    let psimpleidentifier =
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" |>> Identifier

    let pescapedidentifier =
        between (str_ws "[") (str_ws "]") (many1Satisfy ((<>) ']')) <?> "identifier" |>> Identifier

    let pidentifier = (wrapPos psimpleidentifier) <|> (wrapPos pescapedidentifier)

    let pwildcardidentifier =
        wrapPos (between (str_ws "{") (str_ws "}") (many1Satisfy ((<>) '}')) <?> "wildcard" |>> WildcardIdentifier)

    let pexpr, pexprImpl = createParserForwardedToRef()

    let rangePart =
        (opt ((str_ws ":")
              //>>? ((isInRootOfFunctionParameter <|> failFatally "Ranges are not supported outside of function parameters and must be used directly as parameters without other operations.")
                   >>. pexpr ))//)
    
    let errorRangePart =
        (opt ((str_ws ":")
              >>? ((failFatally "Ranges are not supported outside of function parameters and must be used directly as parameters without other operations.")
                   >>. pexpr )))
    
    let indexNoRange =
        (str_ws "|") >>? pexpr .>>.? errorRangePart .>>? (str_ws "|")
    
    let indexOrRange =
        (str_ws "|") >>? pexpr .>>.? rangePart .>>? (str_ws "|")
    
    let funcParam = 
        pipe2 pidentifier indexOrRange
            (fun id indexOrRange ->
                match indexOrRange with
                | (index, optRange) ->
                    match optRange with
                    | Some range -> Variable(id :> IAstItem<identifier>, Some(((index) :> IAstItem<expr>, (range) :> IAstItem<expr>)), None)
                    | None -> Variable(id :> IAstItem<identifier>, None, Some(((index) :> IAstItem<expr>)))
                )

    let wildcardFuncParam =
        pipe2 pwildcardidentifier indexOrRange
            (fun id indexOrRange ->
                match indexOrRange with
                | (index, optRange) ->
                    match optRange with
                    | Some range -> Variable(id :> IAstItem<identifier>, Some(((index) :> IAstItem<expr>, (range) :> IAstItem<expr>)), None)
                    | None -> Variable(id :> IAstItem<identifier>, None, Some(((index) :> IAstItem<expr>)))
                )

    let argList = sepBy ( attempt (wrapPos (funcParam)) <|> attempt (wrapPos (wildcardFuncParam)) <|> pexpr) (str_ws ",")
    
    let argListInParens =
        (str_ws "(") >>. argList .>> (str_ws ")")

    let index =
        (str_ws "|") >>? pexpr .>>? (str_ws "|")
    
    (*let funcParam = 
        pipe3 pidentifier (opt (argListInParens)) (opt (indexOrRange))
            (fun id optArgs optIndexOrRange ->
                match optArgs with
                | Some args ->
                    Function(id :> IAstItem<identifier>, args
                    |> List.map (fun x -> x :> IAstItem<expr>))
                | None ->
                    match optIndexOrRange with
                    | Some (index, optRange) ->
                        match optRange with
                        | Some range -> Variable(id :> IAstItem<identifier>, Some(((index) :> IAstItem<expr>, (range) :> IAstItem<expr>)), None)
                        | None -> Variable(id :> IAstItem<identifier>, None, Some(((index) :> IAstItem<expr>)))
                    | None ->
                        Variable(id :> IAstItem<identifier>, None, None)
                )*)
    
    let identWithOptArgs = 
        pipe3 pidentifier (opt (argListInParens)) (opt (indexNoRange))
            (fun id optArgs optIndexNoRange ->
                match optArgs with
                | Some args ->
                    Function(id :> IAstItem<identifier>, args
                    |> List.map (fun x -> x :> IAstItem<expr>))
                | None ->
                    match optIndexNoRange with
                    | Some (index, noRange) ->
                        match noRange with
                        | Some range -> invalidOp "Range parsing error - this should never be hit"
                        | None -> Variable(id :> IAstItem<identifier>, None, Some(((index) :> IAstItem<expr>)))
                    | None ->
                        Variable(id :> IAstItem<identifier>, None, None)
                )

    let wildcardWithOptIndex =
        pipe2 pwildcardidentifier (opt (indexNoRange))
            (fun id optIndexNoRange ->
                match optIndexNoRange with
                | Some (index, noRange) ->
                    match noRange with
                    | Some range -> invalidOp "Range parsing error - this should never be hit"
                    | None -> Variable(id :> IAstItem<identifier>, None, Some(((index) :> IAstItem<expr>)))
                | None ->
                    Variable(id :> IAstItem<identifier>, None, None)
                )

    let branchExpr = pipe3 (keyword "IF" >>. pexpr .>> ws)  (keyword "THEN" >>. pexpr .>> ws) (keyword "ELSE" >>. pexpr .>> ws) (fun cond a b -> Branch(cond, a, b))

    let oppa = new OperatorPrecedenceParser<IPositionedAstItem<expr>,_,_>()
    do pexprImpl := oppa.ExpressionParser
    let terma = wrapPos (branchExpr .>> ws) <|> wrapPos (pconstant .>> ws) <|> wrapPos (identWithOptArgs .>> ws) <|> wrapPos (wildcardWithOptIndex .>> ws) <|> between (str_ws "(") (str_ws ")") pexpr
    oppa.TermParser <- terma
    oppa.AddOperator(getInfixOperator "??" 1 Associativity.Left (fun p x y -> getInfixAst Coalesce x y p))
    oppa.AddOperator(getInfixOperator "||" 2 Associativity.Left (fun p x y -> getInfixOperatorAst Logical x y Or p))
    oppa.AddOperator(getInfixOperator "&&" 3 Associativity.Left (fun p x y -> getInfixOperatorAst Logical x y And p))
    oppa.AddOperator(getInfixOperator "=" 4 Associativity.Left (fun p x y -> getInfixOperatorAst Comparison x y Equal p))
    oppa.AddOperator(getInfixOperator "<>" 4 Associativity.Left (fun p x y -> getInfixOperatorAst Comparison x y NotEqual p))
    oppa.AddOperator(getInfixOperator ">" 5 Associativity.Left (fun p x y -> getInfixOperatorAst Comparison x y GreaterThan p))
    oppa.AddOperator(getInfixOperator "<" 5 Associativity.Left (fun p x y -> getInfixOperatorAst Comparison x y LessThan p))
    oppa.AddOperator(getInfixOperator ">=" 5 Associativity.Left (fun p x y -> getInfixOperatorAst Comparison x y GreaterThanEqual p))
    oppa.AddOperator(getInfixOperator "<=" 5 Associativity.Left (fun p x y -> getInfixOperatorAst Comparison x y LessThanEqual p))
    oppa.AddOperator(getInfixOperator "+" 6 Associativity.Left (fun p x y -> getInfixOperatorAst Arithmetic x y Add p))
    oppa.AddOperator(getInfixOperator "-" 6 Associativity.Left (fun p x y -> getInfixOperatorAst Arithmetic x y Subtract p))
    oppa.AddOperator(getInfixOperator "*" 7 Associativity.Left (fun p x y -> getInfixOperatorAst Arithmetic x y Multiply p))
    oppa.AddOperator(getInfixOperator "/" 7 Associativity.Left (fun p x y -> getInfixOperatorAst Arithmetic x y Divide p))
    oppa.AddOperator(getInfixOperator "%" 7 Associativity.Left (fun p x y -> getInfixOperatorAst Arithmetic x y Modulus p))
    oppa.AddOperator(getInfixOperator "^" 8 Associativity.Left (fun p x y -> getInfixOperatorAst Arithmetic x y Power p))
    oppa.AddOperator(getPrefixOperator "-" 9 true (fun p x -> getPrefixAst Negation x p))
    oppa.AddOperator(getPrefixOperator "!" 9 true (fun p x -> getPrefixAst Inversion x p))

    let formula = ws >>. pexpr .>> ws .>> eof

    let parseFormulaString str = run formula str

    let parseFormula str =
        match parseFormulaString str with
        | Success (ast, us, pos) ->
            ast
        | Failure (msg, err, us) ->
            raise (ParserException(msg, err))

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
    
    type UserState =
        { FunctionParameterStack: int list; Depth: int; RangeStack: int list }
        with
            static member Default = { FunctionParameterStack = []; Depth = 0; RangeStack = [] }
    
    let enterFunctionParameter =
        updateUserState (fun us -> { us with FunctionParameterStack = push us.Depth us.FunctionParameterStack })
    
    let exitFunctionParameter =
        updateUserState (fun us -> { us with FunctionParameterStack = snd (pop us.FunctionParameterStack) })
        
    let enterRange =
        updateUserState (fun us -> { us with RangeStack = push us.Depth us.RangeStack })
    
    let exitRange =
        updateUserState (fun us -> { us with RangeStack = snd (pop us.RangeStack) })
    
    let incrementDepth =
        updateUserState (fun us -> { us with Depth = us.Depth + 1 })
        
    let decrementDepth =
        updateUserState (fun us -> { us with Depth = us.Depth - 1 })
    
    let isInRootOfFunctionParameter =
        userStateSatisfies (fun us ->
            let currentParam = peek us.FunctionParameterStack
            match currentParam with
            | None -> false
            | Some p ->
                let currentRange = peek us.RangeStack
                match currentRange with
                | None -> failwith "Unexpected error in range stack"
                | Some r -> p = r - 1
        ) 
    
    let str s = pstring s
    let ws = spaces
    let str_ws s = str s .>> ws

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
        InfixOperator(str, incrementDepth >>? getPosition .>> ws, prec, assoc, (),
                               fun opPos leftTerm rightTerm ->
                                   mapping
                                       ((adjustPosition -str.Length opPos), opPos)
                                       leftTerm rightTerm)

    let getPrefixOperator str prec isAssoc mapping =
        PrefixOperator(str, incrementDepth >>? getPosition .>> ws, prec, isAssoc, (),
                               fun opPos term ->
                                   mapping
                                       ((adjustPosition -str.Length opPos), opPos)
                                       term)

    let getOperatorItem op p =
        { Item = op; StartPosition = fst p; EndPosition = snd p } :> IPositionedAstItem<'a>

    let getInfixOperatorAst (kind: (IAstItem<'a> * IAstItem<'b> * IAstItem<'c> -> 'a0)) x y op p =
        let opItem = getOperatorItem op p
        { Item = kind(x :> IPositionedAstItem<'a>, opItem, y :> IPositionedAstItem<'c>); StartPosition = x.StartPosition; EndPosition = y.EndPosition } :> IPositionedAstItem<'a0>

    let getPrefixOperatorAst (kind: (IAstItem<'a> -> 'a0)) x p =
        { Item = kind(x :> IPositionedAstItem<'a>); StartPosition = fst p; EndPosition = x.EndPosition } :> IPositionedAstItem<'a0>
    
    let pnumber = pfloat |>> Number
    let pboolean: Parser<value, UserState> = (str_ws "true" >>% Boolean(true)) <|> (str_ws "false" >>% Boolean(false))
    let ptext: Parser<value, UserState> =
        stringsSepBy pBasicStrChars pEscapedChar |> between (str_ws "\"") (str_ws "\"") <?> "text" |>> Text

    let wrapPos<'a> (parser: Parser<'a, UserState>) = pipe3 getPosition parser getPosition (fun s expr e -> { Item = expr; StartPosition = s; EndPosition = e; } :> IPositionedAstItem<'a>) 

    let pconstant = (wrapPos pnumber |>> (fun x -> Constant(x :> IAstItem<value>))) <|> (wrapPos pboolean |>> (fun x -> Constant(x :> IAstItem<value>))) <|> (wrapPos ptext |>> (fun x -> Constant(x :> IAstItem<value>)))

    let psimpleidentifier: Parser<identifier, UserState> =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" |>> Identifier

    let pescapedidentifier: Parser<identifier, UserState> =
        between (str_ws "[") (str_ws "]") (many1Satisfy ((<>) ']')) <?> "identifier" |>> Identifier

    let pidentifier = (wrapPos psimpleidentifier) <|> (wrapPos pescapedidentifier)

    let pexpr, pexprImpl = createParserForwardedToRef()

    let argList = sepBy ((enterFunctionParameter) >>? pexpr .>>? (exitFunctionParameter)) (str_ws ",")
    
    let argListInParens =
        (str_ws "(") >>. argList .>> (str_ws ")")

    let rangePart: Parser<IPositionedAstItem<expr> option, UserState> =
        (opt ((str_ws ":")
              >>? ((isInRootOfFunctionParameter <|> failFatally "Ranges are not supported outside of function parameters and must be used directly as parameters without other operations.")
                   >>. pexpr )))
    
    let indexOrRange =
        (str_ws "|") >>? (enterRange) >>? pexpr .>>.? rangePart .>>? (exitRange) .>>? (str_ws "|")
    
    let identWithOptArgs = 
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
                )
    
    let branchExpr = pipe3 (str_ws "IF" >>. pexpr .>> ws)  (str_ws "THEN" >>. pexpr .>> ws) (str_ws "ELSE" >>. pexpr .>> ws) (fun cond a b -> Branch(cond, a, b))

    let oppa = new OperatorPrecedenceParser<IPositionedAstItem<expr>,_,UserState>()
    do pexprImpl := oppa.ExpressionParser
    let terma = wrapPos (branchExpr .>> ws) <|> wrapPos (pconstant .>> ws) <|> wrapPos (identWithOptArgs .>> ws) <|> between (str_ws "(") (str_ws ")") pexpr
    oppa.TermParser <- incrementDepth >>? terma .>>? decrementDepth
    oppa.AddOperator(getInfixOperator "||" 1 Associativity.Left (fun p x y -> getInfixOperatorAst Logical x y Or p))
    oppa.AddOperator(getInfixOperator "&&" 2 Associativity.Left (fun p x y -> getInfixOperatorAst Logical x y And p))
    oppa.AddOperator(getInfixOperator "=" 3 Associativity.Left (fun p x y -> getInfixOperatorAst Comparison x y Equal p))
    oppa.AddOperator(getInfixOperator "<>" 3 Associativity.Left (fun p x y -> getInfixOperatorAst Comparison x y NotEqual p))
    oppa.AddOperator(getInfixOperator ">" 3 Associativity.Left (fun p x y -> getInfixOperatorAst Comparison x y GreaterThan p))
    oppa.AddOperator(getInfixOperator "<" 3 Associativity.Left (fun p x y -> getInfixOperatorAst Comparison x y LessThan p))
    oppa.AddOperator(getInfixOperator ">=" 3 Associativity.Left (fun p x y -> getInfixOperatorAst Comparison x y GreaterThanEqual p))
    oppa.AddOperator(getInfixOperator "<=" 3 Associativity.Left (fun p x y -> getInfixOperatorAst Comparison x y LessThanEqual p))
    oppa.AddOperator(getInfixOperator "+" 5 Associativity.Left (fun p x y -> getInfixOperatorAst Arithmetic x y Add p))
    oppa.AddOperator(getInfixOperator "-" 5 Associativity.Left (fun p x y -> getInfixOperatorAst Arithmetic x y Subtract p))
    oppa.AddOperator(getInfixOperator "*" 6 Associativity.Left (fun p x y -> getInfixOperatorAst Arithmetic x y Multiply p))
    oppa.AddOperator(getInfixOperator "/" 6 Associativity.Left (fun p x y -> getInfixOperatorAst Arithmetic x y Divide p))
    oppa.AddOperator(getInfixOperator "%" 6 Associativity.Left (fun p x y -> getInfixOperatorAst Arithmetic x y Modulus p))
    oppa.AddOperator(getInfixOperator "^" 7 Associativity.Left (fun p x y -> getInfixOperatorAst Arithmetic x y Power p))
    oppa.AddOperator(getPrefixOperator "-" 8 true (fun p x -> getPrefixOperatorAst Negation x p))
    oppa.AddOperator(getPrefixOperator "!" 8 true (fun p x -> getPrefixOperatorAst Inversion x p))

    let formula = ws >>. pexpr .>> ws .>> eof

    let parseFormulaString str = runParserOnString formula UserState.Default "" str

    let parseFormula str =
        match parseFormulaString str with
        | Success (ast, us, pos) ->
            ast
        | Failure (msg, err, us) ->
            raise (ParserException(msg, err))

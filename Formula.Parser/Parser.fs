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

    let str s = pstring s
    let ws = spaces
    let str_ws s = str s .>> ws

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

    let getInfixOperatorAst (kind: (IAstItem<'a> * IAstItem<'b> * IAstItem<'c> -> 'a0)) x y op p =
        let opItem = getOperatorItem op p
        { Item = kind(x :> IPositionedAstItem<'a>, opItem, y :> IPositionedAstItem<'c>); StartPosition = x.StartPosition; EndPosition = y.EndPosition } :> IPositionedAstItem<'a0>

    let getPrefixOperatorAst (kind: (IAstItem<'a> -> 'a0)) x p =
        { Item = kind(x :> IPositionedAstItem<'a>); StartPosition = fst p; EndPosition = x.EndPosition } :> IPositionedAstItem<'a0>
    
    let pnumber = pfloat |>> Number
    let pboolean: Parser<value, unit> = (str_ws "true" >>% Boolean(true)) <|> (str_ws "false" >>% Boolean(false))
    let ptext: Parser<value, unit> =
        between (str_ws "\"") (str_ws "\"") (many1Satisfy ((<>) '\"')) <?> "text" |>> Text

    let wrapPos<'a> (parser: Parser<'a, unit>) = pipe3 getPosition parser getPosition (fun s expr e -> { Item = expr; StartPosition = s; EndPosition = e; } :> IPositionedAstItem<'a>) 

    let pconstant = (wrapPos pnumber |>> (fun x -> Constant(x :> IAstItem<value>))) <|> (wrapPos pboolean |>> (fun x -> Constant(x :> IAstItem<value>))) <|> (wrapPos ptext |>> (fun x -> Constant(x :> IAstItem<value>)))

    let psimpleidentifier: Parser<identifier, unit> =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" |>> Identifier

    let pescapedidentifier: Parser<identifier, unit> =
        between (str_ws "[") (str_ws "]") (many1Satisfy ((<>) ']')) <?> "identifier" |>> Identifier

    let pidentifier = (wrapPos psimpleidentifier) <|> (wrapPos pescapedidentifier)

    let pexpr, pexprImpl = createParserForwardedToRef()

    let argList = sepBy pexpr (str_ws ",")
    let argListInParens = between (str_ws "(") (str_ws ")") argList

    let rangeVals = pexpr .>> str_ws ":" .>>. pexpr
    let range = between (str_ws "|") (str_ws "|") rangeVals

    let identWithOptArgs = 
        pipe3 pidentifier (opt (attempt range)) (opt argListInParens) 
            (fun id optRange optArgs ->
                match optArgs with
                | Some args -> Function(id :> IAstItem<identifier>, args |> List.map (fun x -> x :> IAstItem<expr>))
                | None ->
                    match optRange with
                    | Some range -> Variable(id :> IAstItem<identifier>, Some(((fst range) :> IAstItem<expr>, (snd range) :> IAstItem<expr>)))
                    | None -> Variable(id :> IAstItem<identifier>, None)
                )

    let branchExpr = pipe3 (str_ws "IF" >>. pexpr .>> ws)  (str_ws "THEN" >>. pexpr .>> ws) (str_ws "ELSE" >>. pexpr .>> ws) (fun cond a b -> Branch(cond, a, b))

    let oppa = new OperatorPrecedenceParser<IPositionedAstItem<expr>,_,_>()
    do pexprImpl := oppa.ExpressionParser
    let terma = wrapPos (branchExpr .>> ws) <|> wrapPos (pconstant .>> ws) <|> wrapPos (identWithOptArgs .>> ws) <|> between (str_ws "(") (str_ws ")") pexpr
    oppa.TermParser <- terma
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

    let parseFormulaString str = run formula str

    let parseFormula str =
        match parseFormulaString str with
        | Success (ast, a, b) ->
            ast
        | Failure (msg, a, b) ->
            raise (ParserException(msg, a))

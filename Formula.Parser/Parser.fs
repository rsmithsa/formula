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

    let pnumber = pfloat |>> Number
    let pboolean: Parser<value, unit> = (str_ws "true" >>% Boolean(true)) <|> (str_ws "false" >>% Boolean(false))
    let ptext: Parser<value, unit> =
        between (str_ws "\"") (str_ws "\"") (many1Satisfy ((<>) '\"')) <?> "text" |>> Text
    let pconstant = (pnumber |>> Constant) <|> (pboolean |>> Constant) <|> (ptext |>> Constant)

    let psimpleidentifier: Parser<identifier, unit> =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" |>> Identifier

    let pescapedidentifier: Parser<identifier, unit> =
        between (str_ws "[") (str_ws "]") (many1Satisfy ((<>) ']')) <?> "identifier" |>> Identifier

    let pidentifier = (psimpleidentifier) <|> (pescapedidentifier)

    let pexpr, pexprImpl = createParserForwardedToRef()

    let argList = sepBy pexpr (str_ws ",")
    let argListInParens = between (str_ws "(") (str_ws ")") argList

    let rangeVals = pexpr .>> str_ws ":" .>>. pexpr
    let range = between (str_ws "|") (str_ws "|") rangeVals

    let identWithOptArgs = 
        pipe3 pidentifier (opt (attempt range)) (opt argListInParens) 
            (fun id optRange optArgs ->
                match optArgs with
                | Some args -> Function(id, args)
                | None -> Variable(id, optRange))

    let branchExpr = pipe3 (str_ws "IF" >>. pexpr .>> ws)  (str_ws "THEN" >>. pexpr .>> ws) (str_ws "ELSE" >>. pexpr .>> ws) (fun cond a b -> Branch(cond, a, b))

    let oppa = new OperatorPrecedenceParser<expr,unit,unit>()
    do pexprImpl := oppa.ExpressionParser
    let terma = (branchExpr .>> ws) <|> (pconstant .>> ws) <|> (identWithOptArgs .>> ws) <|> between (str_ws "(") (str_ws ")") pexpr
    oppa.TermParser <- terma
    oppa.AddOperator(InfixOperator("||", ws, 1, Associativity.Left, fun x y -> Logical(x, Or, y)))
    oppa.AddOperator(InfixOperator("&&", ws, 2, Associativity.Left, fun x y -> Logical(x, And, y)))
    oppa.AddOperator(InfixOperator("=", ws, 3, Associativity.Left, fun x y -> Comparison(x, Equal, y)))
    oppa.AddOperator(InfixOperator("<>", ws, 3, Associativity.Left, fun x y -> Comparison(x, NotEqual, y)))
    oppa.AddOperator(InfixOperator(">", ws, 3, Associativity.Left, fun x y -> Comparison(x, GreaterThan, y)))
    oppa.AddOperator(InfixOperator("<", ws, 3, Associativity.Left, fun x y -> Comparison(x, LessThan, y)))
    oppa.AddOperator(InfixOperator(">=", ws, 3, Associativity.Left, fun x y -> Comparison(x, GreaterThanEqual, y)))
    oppa.AddOperator(InfixOperator("<=", ws, 3, Associativity.Left, fun x y -> Comparison(x, LessThanEqual, y)))
    oppa.AddOperator(InfixOperator("+", ws, 5, Associativity.Left, fun x y -> Arithmetic(x, Add, y)))
    oppa.AddOperator(InfixOperator("-", ws, 5, Associativity.Left, fun x y -> Arithmetic(x, Subtract, y)))
    oppa.AddOperator(InfixOperator("*", ws, 6, Associativity.Left, fun x y -> Arithmetic(x, Multiply, y)))
    oppa.AddOperator(InfixOperator("/", ws, 6, Associativity.Left, fun x y -> Arithmetic(x, Divide, y)))
    oppa.AddOperator(InfixOperator("%", ws, 6, Associativity.Left, fun x y -> Arithmetic(x, Modulus, y)))
    oppa.AddOperator(InfixOperator("^", ws, 7, Associativity.Left, fun x y -> Arithmetic(x, Power, y)))
    oppa.AddOperator(PrefixOperator("-", ws, 8, true, fun x -> Negation(x)))
    oppa.AddOperator(PrefixOperator("!", ws, 8, true, fun x -> Inversion(x)))

    let formula = ws >>. pexpr .>> ws .>> eof

    let parseFormulaString str = run formula str

    let parseFormula str =
        match parseFormulaString str with
        | Success (ast, a, b) ->
            ast
        | Failure (msg, a, b) ->
            raise (ParserException(msg, a))

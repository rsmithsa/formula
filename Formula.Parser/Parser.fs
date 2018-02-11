//-----------------------------------------------------------------------
// <copyright file="Parser.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module Parser =

    open FParsec
    open Formula.Parser.Ast

    let str s = pstring s
    let ws = spaces
    let str_ws s = str s .>> ws

    let pnumber = pfloat |>> Number
    let pboolean: Parser<value, unit> = (str_ws "true" >>% Boolean(true)) <|> (str_ws "false" >>% Boolean(false))
    let pconstant = (pnumber |>> (fun x -> Constant(x))) <|> (pboolean |>> (fun x -> Constant(x)))

    let pidentifier: Parser<identifier, unit> =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" |>> Identifier

    let pvariable = pidentifier |>> (fun x -> Variable(x))

    let pexpr, pexprImpl = createParserForwardedToRef()

    let argList = sepBy pexpr (str_ws ",")
    let argListInParens = between (str_ws "[") (str_ws "]") argList
    let identWithOptArgs = 
        pipe2 pidentifier (opt argListInParens) 
            (fun id optArgs ->
                match optArgs with
                | Some args -> Function(id, args)
                | None -> Variable(id))

    let oppa = new OperatorPrecedenceParser<expr,unit,unit>()
    do pexprImpl := oppa.ExpressionParser
    let terma = (pconstant .>> ws) <|> (identWithOptArgs .>> ws) <|> between (str_ws "(") (str_ws ")") pexpr
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
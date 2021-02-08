//-----------------------------------------------------------------------
// <copyright file="ParserTests.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Tests

open FParsec.CharParsers
open Microsoft.VisualStudio.TestTools.UnitTesting

open Formula.Parser.Ast
open Formula.Parser.Parser

[<TestClass>]
type ParserTests () =

    [<TestMethod>]
    member this.TestParseConstant () =
        let result = parseFormulaString "42"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Constant(Number(42.0))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
     
    [<TestMethod>]
    member this.TestParseNegationConstant () =
        let result = parseFormulaString "-42"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Negation(Constant(Number(42.0)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseAdditionConstant () =
        let result = parseFormulaString "1 + 42"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Arithmetic(Constant(Number(1.0)), Add, Constant(Number(42.0)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseSubtractionConstant () =
        let result = parseFormulaString "1 - 42"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Arithmetic(Constant(Number(1.0)), Subtract, Constant(Number(42.0)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseMultiplicationConstant () =
        let result = parseFormulaString "1 * 42"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Arithmetic(Constant(Number(1.0)), Multiply, Constant(Number(42.0)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseDivisionConstant () =
        let result = parseFormulaString "1 / 42"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Arithmetic(Constant(Number(1.0)), Divide, Constant(Number(42.0)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParsePowerConstant () =
        let result = parseFormulaString "1 ^ 42"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Arithmetic(Constant(Number(1.0)), Power, Constant(Number(42.0)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseModulusConstant () =
        let result = parseFormulaString "1 % 42"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Arithmetic(Constant(Number(1.0)), Modulus, Constant(Number(42.0)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseOrderOfOperations1 () =
        let result = parseFormulaString "1 + 42 * 2"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Arithmetic(Constant(Number(1.0)), Add, Arithmetic(Constant(Number(42.0)), Multiply, Constant(Number(2.0))))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseOrderOfOperations2 () =
        let result = parseFormulaString "(1 + 42) * 2"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Arithmetic(Arithmetic(Constant(Number(1.0)), Add, Constant(Number(42.0))), Multiply, Constant(Number(2.0)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseOrderOfOperations3 () =
        let result = parseFormulaString "(1 + 42) * 2^3"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Arithmetic(Arithmetic(Constant(Number(1.0)), Add, Constant(Number(42.0))), Multiply, Arithmetic(Constant(Number(2.0)), Power, Constant(Number(3.0))))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseOrderOfOperations4 () =
        let result = parseFormulaString "1 + 42 % 2"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Arithmetic(Constant(Number(1.0)), Add, Arithmetic(Constant(Number(42.0)), Modulus, Constant(Number(2.0))))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariable1 () =
        let result = parseFormulaString "MyVar"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Variable(Identifier("MyVar"))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariable2 () =
        let result = parseFormulaString "MyVar1"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Variable(Identifier("MyVar1"))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariable3 () =
        let result = parseFormulaString "_MyVar_1"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Variable(Identifier("_MyVar_1"))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariableOrderOfOperations1 () =
        let result = parseFormulaString "V1 + V42 * V2"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Arithmetic(Variable(Identifier("V1")), Add, Arithmetic(Variable(Identifier("V42")), Multiply, Variable(Identifier("V2"))))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariableOrderOfOperations2 () =
        let result = parseFormulaString "(V1 + V42) * V2"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Arithmetic(Arithmetic(Variable(Identifier("V1")), Add, Variable(Identifier("V42"))), Multiply, Variable(Identifier("V2")))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariableOrderOfOperations3 () =
        let result = parseFormulaString "(V1 + V42) * V2^V3"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Arithmetic(Arithmetic(Variable(Identifier("V1")), Add, Variable(Identifier("V42"))), Multiply, Arithmetic(Variable(Identifier("V2")), Power, Variable(Identifier("V3"))))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariableOrderOfOperations4 () =
        let result = parseFormulaString "V1 + V42 % V2"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Arithmetic(Variable(Identifier("V1")), Add, Arithmetic(Variable(Identifier("V42")), Modulus, Variable(Identifier("V2"))))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseFunction () =
        let result = parseFormulaString "COUNT()"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Function(Identifier("COUNT"), [])
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseFunctionWithParameters () =
        let result = parseFormulaString "COUNT(1 + 42, MyVar)"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Function(Identifier("COUNT"), [ Arithmetic(Constant(Number(1.0)), Add, Constant(Number(42.0))); Variable(Identifier("MyVar")) ])
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseFunctionOfFunction () =
        let result = parseFormulaString "COUNT(COUNT())"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Function(Identifier("COUNT"), [ Function(Identifier("COUNT"), []) ])
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseLogicalTrue () =
        let result = parseFormulaString "true"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Constant(Boolean(true))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseLogicalFalse () =
        let result = parseFormulaString "false"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Constant(Boolean(false))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseInversion () =
        let result = parseFormulaString "!false"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Inversion(Constant(Boolean(false)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseLogicalAnd () =
        let result = parseFormulaString "true && false"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Logical(Constant(Boolean(true)), And, Constant(Boolean(false)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseLogicalOr () =
        let result = parseFormulaString "true || false"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Logical(Constant(Boolean(true)), Or, Constant(Boolean(false)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseComparisonEqual () =
        let result = parseFormulaString "42 = 1"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Comparison(Constant(Number(42.0)), Equal, Constant(Number(1.0)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseComparisonNotEqual () =
        let result = parseFormulaString "42 <> 1"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Comparison(Constant(Number(42.0)), NotEqual, Constant(Number(1.0)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseComparisonGreaterThan () =
        let result = parseFormulaString "42 > 1"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Comparison(Constant(Number(42.0)), GreaterThan, Constant(Number(1.0)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseComparisonLessThan () =
        let result = parseFormulaString "42 < 1"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Comparison(Constant(Number(42.0)), LessThan, Constant(Number(1.0)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseComparisonGreaterThanEqual () =
        let result = parseFormulaString "42 >= 1"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Comparison(Constant(Number(42.0)), GreaterThanEqual, Constant(Number(1.0)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseComparisonLessThanEqual () =
        let result = parseFormulaString "42 <= 1"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Comparison(Constant(Number(42.0)), LessThanEqual, Constant(Number(1.0)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseBranch1 () =
        let result = parseFormulaString "IF true THEN 42 ELSE 1"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Branch(Constant(Boolean(true)), Constant(Number(42.0)), Constant(Number(1.0)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseBranch2 () =
        let result = parseFormulaString "IF(42<=1)THEN(42)ELSE(1)"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Branch(Comparison(Constant(Number(42.0)), LessThanEqual, Constant(Number(1.0))), Constant(Number(42.0)), Constant(Number(1.0)))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
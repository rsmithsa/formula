//-----------------------------------------------------------------------
// <copyright file="ParserTests.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Tests

open System
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
    member this.TestParseVariable () =
        let result = parseFormulaString "MyVar"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Variable(Identifier("MyVar"))
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseFunction () =
        let result = parseFormulaString "COUNT[]"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Function(Identifier("COUNT"), [])
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseFunctionWithParameters () =
        let result = parseFormulaString "COUNT[1 + 42, MyVar]"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Function(Identifier("COUNT"), [ Arithmetic(Constant(Number(1.0)), Add, Constant(Number(42.0))); Variable(Identifier("MyVar")) ])
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseFunctionOfFunction () =
        let result = parseFormulaString "COUNT[COUNT[]]"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = Function(Identifier("COUNT"), [ Function(Identifier("COUNT"), []) ])
            Assert.AreEqual(expected, ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
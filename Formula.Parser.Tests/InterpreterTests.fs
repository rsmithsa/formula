//-----------------------------------------------------------------------
// <copyright file="InterpreterTests.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Tests

open System
open FParsec.CharParsers
open Microsoft.VisualStudio.TestTools.UnitTesting

open Formula.Parser
open Formula.Parser.Ast
open Formula.Parser.Parser
open Formula.Parser.Interpreter
open Formula.Parser.Integration

[<TestClass>]
type InterpreterTests () =

    let emptyVarMap = MapVariableProvider(Map.empty) :> IVariableProvider
    let varMap = MapVariableProvider(Map.empty.Add("MyVar", 42.0)) :> IVariableProvider

    [<TestMethod>]
    member this.TestInterpretConstant () =
        let result = parseFormulaString "42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast emptyVarMap DefaultFunctionProvider.Instance
            Assert.AreEqual(42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretNegationConstant () =
        let result = parseFormulaString "-42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast emptyVarMap DefaultFunctionProvider.Instance
            Assert.AreEqual(-42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretAdditionConstant () =
        let result = parseFormulaString "1 + 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast emptyVarMap DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0 + 42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretSubtractionConstant () =
        let result = parseFormulaString "1 - 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast emptyVarMap DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0 - 42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretMultiplicationConstant () =
        let result = parseFormulaString "1 * 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast emptyVarMap DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0 * 42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretDivisionConstant () =
        let result = parseFormulaString "1 / 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast emptyVarMap DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0 / 42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretPowerConstant () =
        let result = parseFormulaString "1 ^ 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast emptyVarMap DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0 ** 42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretOrderOfOperations1 () =
        let result = parseFormulaString "1 + 42 * 2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast emptyVarMap DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0 + 42.0 * 2.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretOrderOfOperations2 () =
        let result = parseFormulaString "(1 + 42) * 2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast emptyVarMap DefaultFunctionProvider.Instance
            Assert.AreEqual((1.0 + 42.0) * 2.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretOrderOfOperations3 () =
        let result = parseFormulaString "(1 + 42) * 2^3"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast emptyVarMap DefaultFunctionProvider.Instance
            Assert.AreEqual((1.0 + 42.0) * 2.0 ** 3.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretVariable () =
        let result = parseFormulaString "MyVar"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast varMap DefaultFunctionProvider.Instance
            Assert.AreEqual(varMap.Lookup "MyVar", value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretFunction () =
        let result = parseFormulaString "COUNT[]"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast (MapVariableProvider(Map.empty)) DefaultFunctionProvider.Instance
            Assert.AreEqual((DefaultFunctionProvider.Instance.Lookup "COUNT").Execute (List.toArray []), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretFunctionWithParameters () =
        let result = parseFormulaString "COUNT[1 + 42, MyVar]"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast varMap DefaultFunctionProvider.Instance
            Assert.AreEqual((DefaultFunctionProvider.Instance.Lookup "COUNT").Execute (List.toArray [1.0 + 42.0; varMap.Lookup "MyVar"]), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretFunctionOfFunction () =
        let result = parseFormulaString "COUNT[COUNT[]]"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast (MapVariableProvider(Map.empty)) DefaultFunctionProvider.Instance
            Assert.AreEqual((DefaultFunctionProvider.Instance.Lookup "COUNT").Execute (List.toArray [(DefaultFunctionProvider.Instance.Lookup "COUNT").Execute (List.toArray [])]), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)


// <copyright file="CompilerTests.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Tests

open FParsec.CharParsers
open Microsoft.VisualStudio.TestTools.UnitTesting

open Formula.Parser
open Formula.Parser.Parser
open Formula.Parser.Compiler
open Formula.Parser.Integration

[<TestClass>]
type CompilerTests () =

    let varMap = MapVariableProvider(Map.empty
                                        .Add("MyVar", 42.0)
                                        .Add("MyVar1", 4.2)
                                        .Add("_MyVar_1", 0.42)
                                        .Add("V1", 1.0)
                                        .Add("V2", 2.0)
                                        .Add("V3", 3.0)
                                        .Add("V42", 42.0))

    [<TestMethod>]
    member this.TestCompileConstant () =
        let result = parseFormulaString "42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileNegationConstant () =
        let result = parseFormulaString "-42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(-42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileAdditionConstant () =
        let result = parseFormulaString "1 + 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(1.0 + 42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileSubtractionConstant () =
        let result = parseFormulaString "1 - 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(1.0 - 42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileMultiplicationConstant () =
        let result = parseFormulaString "1 * 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(1.0 * 42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileDivisionConstant () =
        let result = parseFormulaString "1 / 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(1.0 / 42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompilePowerConstant () =
        let result = parseFormulaString "1 ^ 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(1.0 ** 42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileModulusConstant () =
        let result = parseFormulaString "1 % 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(1.0 % 42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileOrderOfOperations1 () =
        let result = parseFormulaString "1 + 42 * 2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(1.0 + 42.0 * 2.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileOrderOfOperations2 () =
        let result = parseFormulaString "(1 + 42) * 2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual((1.0 + 42.0) * 2.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileOrderOfOperations3 () =
        let result = parseFormulaString "(1 + 42) * 2^3"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual((1.0 + 42.0) * 2.0 ** 3.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileOrderOfOperations4 () =
        let result = parseFormulaString "1 + 42 % 2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(1.0 + 42.0 % 2.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileVariable1 () =
        let result = parseFormulaString "MyVar"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            Assert.AreEqual(varMap.Lookup "MyVar", value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileVariable2 () =
        let result = parseFormulaString "MyVar1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            Assert.AreEqual(varMap.Lookup "MyVar1", value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileVariable3 () =
        let result = parseFormulaString "_MyVar_1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            Assert.AreEqual(varMap.Lookup "_MyVar_1", value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileVariableOrderOfOperations1 () =
        let result = parseFormulaString "V1 + V42 * V2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            Assert.AreEqual(varMap.Lookup "V1" + varMap.Lookup "V42" * varMap.Lookup "V2", value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileVariableOrderOfOperations2 () =
        let result = parseFormulaString "(V1 + V42) * V2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            Assert.AreEqual((varMap.Lookup "V1" + varMap.Lookup "V42") * varMap.Lookup "V2", value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileVariableOrderOfOperations3 () =
        let result = parseFormulaString "(V1 + V42) * V2^V3"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            Assert.AreEqual((varMap.Lookup "V1" + varMap.Lookup "V42") * varMap.Lookup "V2" ** varMap.Lookup "V3", value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileVariableOrderOfOperations4 () =
        let result = parseFormulaString "V1 + V42 % V2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            Assert.AreEqual(varMap.Lookup "V1" + varMap.Lookup "V42" % varMap.Lookup "V2", value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileFunction () =
        let result = parseFormulaString "COUNT()"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual((DefaultFunctionProvider.Instance.Lookup "COUNT").Execute (List.toArray []), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileFunctionWithParameters () =
        let result = parseFormulaString "COUNT(1 + 42, MyVar)"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            Assert.AreEqual((DefaultFunctionProvider.Instance.Lookup "COUNT").Execute (List.toArray [1.0 + 42.0; varMap.Lookup "MyVar"]), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileFunctionOfFunction () =
        let result = parseFormulaString "COUNT(COUNT())"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual((DefaultFunctionProvider.Instance.Lookup "COUNT").Execute (List.toArray [(DefaultFunctionProvider.Instance.Lookup "COUNT").Execute (List.toArray [])]), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileLogicalTrue () =
        let result = parseFormulaString "true"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(1.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileLogicalFalse () =
        let result = parseFormulaString "false"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(0.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileInversion () =
        let result = parseFormulaString "!false"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(1.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileLogicalAnd () =
        let result = parseFormulaString "true && false"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(0.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileLogicalOr () =
        let result = parseFormulaString "true || false"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(1.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileComparisonEqual () =
        let result = parseFormulaString "42 = 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(0.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileComparisonNotEqual () =
        let result = parseFormulaString "42 <> 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(1.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileComparisonGreaterThan () =
        let result = parseFormulaString "42 > 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(1.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileComparisonLessThan () =
        let result = parseFormulaString "42 < 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(0.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileComparisonGreaterThanEqual () =
        let result = parseFormulaString "42 >= 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(1.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileComparisonLessThanEqual () =
        let result = parseFormulaString "42 <= 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(0.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileBranch1 () =
        let result = parseFormulaString "IF true THEN 42 ELSE 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileBranch2 () =
        let result = parseFormulaString "IF(42<=1)THEN(42)ELSE(1)"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(1.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)


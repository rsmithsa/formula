﻿// <copyright file="ILCompilerTests.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Tests

open FParsec.CharParsers
open Microsoft.VisualStudio.TestTools.UnitTesting

open Formula.Parser
open Formula.Parser.Ast
open Formula.Parser.Parser
open Formula.Parser.ILCompiler
open Formula.Parser.Integration
open TestHelper

[<TestClass>]
type ILCompilerTests () =

    let varMap = MapVariableProvider(Map.empty
                                        .Add("MyVar", 42.0)
                                        .Add("MyVar1", 4.2)
                                        .Add("_MyVar_1", 0.42)
                                        .Add("My Long Variable", 0.42)
                                        .Add("My Long @$#% Variable 2", 0.42)
                                        .Add("V1", 1.0)
                                        .Add("V2", 2.0)
                                        .Add("V3", 3.0)
                                        .Add("V42", 42.0))

    [<TestMethod>]
    member this.TestCompileNumberConstant () =
        let result = parseFormulaString "42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(42.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileBooleanConstant () =
        let result = parseFormulaString "true"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(1.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileTextConstant () =
        let result = parseFormulaString "\"123\""
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(123.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileNegationConstant () =
        let result = parseFormulaString "-42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(-42.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileAdditionConstant () =
        let result = parseFormulaString "1 + 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(1.0 + 42.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileSubtractionConstant () =
        let result = parseFormulaString "1 - 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(1.0 - 42.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileMultiplicationConstant () =
        let result = parseFormulaString "1 * 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(1.0 * 42.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileDivisionConstant () =
        let result = parseFormulaString "1 / 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(1.0 / 42.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompilePowerConstant () =
        let result = parseFormulaString "1 ^ 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(1.0 ** 42.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileModulusConstant () =
        let result = parseFormulaString "1 % 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(1.0 % 42.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileOrderOfOperations1 () =
        let result = parseFormulaString "1 + 42 * 2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(1.0 + 42.0 * 2.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileOrderOfOperations2 () =
        let result = parseFormulaString "(1 + 42) * 2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some((1.0 + 42.0) * 2.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileOrderOfOperations3 () =
        let result = parseFormulaString "(1 + 42) * 2^3"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some((1.0 + 42.0) * 2.0 ** 3.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileOrderOfOperations4 () =
        let result = parseFormulaString "1 + 42 % 2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(1.0 + 42.0 % 2.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileVariable1 () =
        let result = parseFormulaString "MyVar"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Helpers.castToDouble(varMap.Lookup "MyVar"), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileVariable2 () =
        let result = parseFormulaString "MyVar1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Helpers.castToDouble(varMap.Lookup "MyVar1"), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileVariable3 () =
        let result = parseFormulaString "_MyVar_1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Helpers.castToDouble(varMap.Lookup "_MyVar_1"), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileVariable4 () =
        let result = parseFormulaString "[My Long Variable]"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Helpers.castToDouble(varMap.Lookup "My Long Variable"), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileVariable5 () =
        let result = parseFormulaString "[My Long @$#% Variable 2]"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Helpers.castToDouble(varMap.Lookup "My Long @$#% Variable 2"), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileVariableOrderOfOperations1 () =
        let result = parseFormulaString "V1 + V42 * V2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(!!> Helpers.castToDouble(varMap.Lookup "V1") + !!> Helpers.castToDouble(varMap.Lookup "V42") * !!> Helpers.castToDouble(varMap.Lookup "V2")), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileVariableOrderOfOperations2 () =
        let result = parseFormulaString "(V1 + V42) * V2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some((!!> Helpers.castToDouble(varMap.Lookup "V1") + !!> Helpers.castToDouble(varMap.Lookup "V42")) * !!> Helpers.castToDouble(varMap.Lookup "V2")), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileVariableOrderOfOperations3 () =
        let result = parseFormulaString "(V1 + V42) * V2^V3"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some((!!> Helpers.castToDouble(varMap.Lookup "V1") + !!> Helpers.castToDouble(varMap.Lookup "V42")) * !!> Helpers.castToDouble(varMap.Lookup "V2") ** !!> Helpers.castToDouble(varMap.Lookup "V3")), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileVariableOrderOfOperations4 () =
        let result = parseFormulaString "V1 + V42 % V2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(!!> Helpers.castToDouble(varMap.Lookup "V1") + !!> Helpers.castToDouble(varMap.Lookup "V42") % !!> Helpers.castToDouble(varMap.Lookup "V2")), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileFunction () =
        let result = parseFormulaString "COUNT()"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            let expected = Helpers.castToDouble((DefaultFunctionProvider.Instance.Lookup "COUNT").Execute (List.toArray []))
            Assert.AreEqual(expected, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileFunctionWithParameters () =
        let result = parseFormulaString "SUM(1 + 42, MyVar)"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            let expected = Helpers.castToDouble((DefaultFunctionProvider.Instance.Lookup "SUM").Execute (List.toArray [Number(1.0 + 42.0); varMap.Lookup "MyVar"]))
            Assert.AreEqual(expected, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileFunctionOfFunction () =
        let result = parseFormulaString "COUNT(COUNT())"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            let expected = Helpers.castToDouble((DefaultFunctionProvider.Instance.Lookup "COUNT").Execute (List.toArray [(DefaultFunctionProvider.Instance.Lookup "COUNT").Execute (List.toArray [])]))
            Assert.AreEqual(expected, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestCompileFunctionOfFunctionWithRange () =
        let result = parseFormulaString "SUM(SUM(1 + 42, MyVar|1:10|), 1)"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            let expected = Helpers.castToDouble((DefaultFunctionProvider.Instance.Lookup "SUM").Execute (List.toArray [(DefaultFunctionProvider.Instance.Lookup "SUM").Execute (Array.concat [ [| Number(1.0 + 42.0) |]; varMap.LookupRange "MyVar" (Number(1.0)) (Number(10.0))]); Number(1.0)]))
            Assert.AreEqual(expected, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileFunctionWithRange () =
        let result = parseFormulaString "SUM(1 + 42, MyVar|1:10|)"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            let expected = Helpers.castToDouble((DefaultFunctionProvider.Instance.Lookup "SUM").Execute (Array.concat [ [| Number(1.0 + 42.0) |]; varMap.LookupRange "MyVar" (Number(1.0)) (Number(10.0))]))
            Assert.AreEqual(expected, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
                     
    [<TestMethod>]
    member this.TestCompileFunctionWithIndex () =
        let result = parseFormulaString "COUNT(1 + 42, MyVar|1|)"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            let expected = Helpers.castToDouble((DefaultFunctionProvider.Instance.Lookup "COUNT").Execute ([| Number(1.0 + 42.0); varMap.LookupIndex "MyVar" (Number(1.0)) |]))
            Assert.AreEqual(expected, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileLogicalTrue () =
        let result = parseFormulaString "true"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(1.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileLogicalFalse () =
        let result = parseFormulaString "false"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(0.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileInversion () =
        let result = parseFormulaString "!false"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(1.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileLogicalAnd () =
        let result = parseFormulaString "true && false"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(0.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileLogicalOr () =
        let result = parseFormulaString "true || false"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(1.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileComparisonEqual () =
        let result = parseFormulaString "42 = 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(0.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileComparisonNotEqual () =
        let result = parseFormulaString "42 <> 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(1.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileComparisonGreaterThan () =
        let result = parseFormulaString "42 > 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(1.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileComparisonLessThan () =
        let result = parseFormulaString "42 < 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(0.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileComparisonGreaterThanEqual () =
        let result = parseFormulaString "42 >= 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(1.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileComparisonLessThanEqual () =
        let result = parseFormulaString "42 <= 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(0.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileBranch1 () =
        let result = parseFormulaString "IF true THEN 42 ELSE 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(42.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileBranch2 () =
        let result = parseFormulaString "IF(42<=1)THEN(42)ELSE(1)"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(Some(1.0), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileNothingArithmetic () =
        let result = parseFormulaString "-1 * (42 + null)"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(None, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestCompileNothingNegation () =
        let result = parseFormulaString "-null"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            Assert.AreEqual(None, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestCompileBug51Variable () =
        let result = parseFormulaString "MyVar + MyVar + MyVar + MyVar + MyVar + MyVar + MyVar + MyVar + MyVar + MyVar + MyVar + MyVar + MyVar + MyVar + MyVar + MyVar + MyVar + MyVar + MyVar + MyVar"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(varMap, DefaultFunctionProvider.Instance)
            let expected = Some(Helpers.castToDouble(varMap.Lookup "MyVar").Value * 20.0)
            Assert.AreEqual(expected, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestCompileBug51Constant () =
        let result = parseFormulaString "1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 + 18 + 19 + 20"
        match result with
        | Success (ast, userState, endPos) ->
            let value = (compileFormula ast).Invoke(MapVariableProvider.Empty, DefaultFunctionProvider.Instance)
            let expected = Some(210.0)
            Assert.AreEqual(expected, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
//-----------------------------------------------------------------------
// <copyright file="InterpreterTests.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Tests

open FParsec.CharParsers
open Microsoft.VisualStudio.TestTools.UnitTesting

open Formula.Parser
open Formula.Parser.Ast
open Formula.Parser.Parser
open Formula.Parser.Interpreter
open Formula.Parser.Integration

[<TestClass>]
type InterpreterTests () =

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
    member this.TestInterpretNumberConstant () =
        let result = parseFormulaString "42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretBooleanConstant () =
        let result = parseFormulaString "true"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretTextConstant () =
        let result = parseFormulaString "\"123\""
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(123.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretNegationConstant () =
        let result = parseFormulaString "-42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(-42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretAdditionConstant () =
        let result = parseFormulaString "1 + 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0 + 42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretSubtractionConstant () =
        let result = parseFormulaString "1 - 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0 - 42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretMultiplicationConstant () =
        let result = parseFormulaString "1 * 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0 * 42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretDivisionConstant () =
        let result = parseFormulaString "1 / 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0 / 42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretPowerConstant () =
        let result = parseFormulaString "1 ^ 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0 ** 42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretModulusConstant () =
        let result = parseFormulaString "1 % 42"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0 % 42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretOrderOfOperations1 () =
        let result = parseFormulaString "1 + 42 * 2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0 + 42.0 * 2.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretOrderOfOperations2 () =
        let result = parseFormulaString "(1 + 42) * 2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual((1.0 + 42.0) * 2.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretOrderOfOperations3 () =
        let result = parseFormulaString "(1 + 42) * 2^3"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual((1.0 + 42.0) * 2.0 ** 3.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretOrderOfOperations4 () =
        let result = parseFormulaString "1 + 42 % 2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0 + 42.0 % 2.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretVariable1 () =
        let result = parseFormulaString "MyVar"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast varMap DefaultFunctionProvider.Instance
            Assert.AreEqual(Helpers.castToDouble(varMap.Lookup "MyVar"), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretVariable2 () =
        let result = parseFormulaString "MyVar1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast varMap DefaultFunctionProvider.Instance
            Assert.AreEqual(Helpers.castToDouble(varMap.Lookup "MyVar1"), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretVariable3 () =
        let result = parseFormulaString "_MyVar_1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast varMap DefaultFunctionProvider.Instance
            Assert.AreEqual(Helpers.castToDouble(varMap.Lookup "_MyVar_1"), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretVariable4 () =
        let result = parseFormulaString "[My Long Variable]"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast varMap DefaultFunctionProvider.Instance
            Assert.AreEqual(Helpers.castToDouble(varMap.Lookup "My Long Variable"), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretVariable5 () =
        let result = parseFormulaString "[My Long @$#% Variable 2]"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast varMap DefaultFunctionProvider.Instance
            Assert.AreEqual(Helpers.castToDouble(varMap.Lookup "My Long @$#% Variable 2"), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretVariableOrderOfOperations1 () =
        let result = parseFormulaString "V1 + V42 * V2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast varMap DefaultFunctionProvider.Instance
            Assert.AreEqual(Helpers.castToDouble(varMap.Lookup "V1") + Helpers.castToDouble(varMap.Lookup "V42") * Helpers.castToDouble(varMap.Lookup "V2"), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretVariableOrderOfOperations2 () =
        let result = parseFormulaString "(V1 + V42) * V2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast varMap DefaultFunctionProvider.Instance
            Assert.AreEqual((Helpers.castToDouble(varMap.Lookup "V1") + Helpers.castToDouble(varMap.Lookup "V42")) * Helpers.castToDouble(varMap.Lookup "V2"), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretVariableOrderOfOperations3 () =
        let result = parseFormulaString "(V1 + V42) * V2^V3"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast varMap DefaultFunctionProvider.Instance
            Assert.AreEqual((Helpers.castToDouble(varMap.Lookup "V1") + Helpers.castToDouble(varMap.Lookup "V42")) * Helpers.castToDouble(varMap.Lookup "V2") ** Helpers.castToDouble(varMap.Lookup "V3"), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretVariableOrderOfOperations4 () =
        let result = parseFormulaString "V1 + V42 % V2"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast varMap DefaultFunctionProvider.Instance
            Assert.AreEqual(Helpers.castToDouble(varMap.Lookup "V1") + Helpers.castToDouble(varMap.Lookup "V42") % Helpers.castToDouble(varMap.Lookup "V2"), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretFunction () =
        let result = parseFormulaString "COUNT()"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual((DefaultFunctionProvider.Instance.Lookup "COUNT").Execute (List.toArray []), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretFunctionWithParameters () =
        let result = parseFormulaString "COUNT(1 + 42, MyVar)"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast varMap DefaultFunctionProvider.Instance
            Assert.AreEqual((DefaultFunctionProvider.Instance.Lookup "COUNT").Execute (List.toArray [Number(1.0 + 42.0); varMap.Lookup "MyVar"]), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretFunctionOfFunction () =
        let result = parseFormulaString "COUNT(COUNT())"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual((DefaultFunctionProvider.Instance.Lookup "COUNT").Execute (List.toArray [Number((DefaultFunctionProvider.Instance.Lookup "COUNT").Execute (List.toArray []))]), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretFunctionWithRange () =
        let result = parseFormulaString "COUNT(1 + 42, MyVar|1:10|)"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast varMap DefaultFunctionProvider.Instance
            Assert.AreEqual((DefaultFunctionProvider.Instance.Lookup "COUNT").Execute (Array.concat [ [| Number(1.0 + 42.0) |]; varMap.LookupRange "MyVar" (Number(1.0)) (Number(10.0))]), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestInterpretRange () =
        let result = parseFormulaString "MyVar|1:10|"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast varMap DefaultFunctionProvider.Instance
            Assert.AreEqual((DefaultFunctionProvider.Instance.Lookup "COUNT").Execute (Array.concat [ [| Number(1.0 + 42.0) |]; varMap.LookupRange "MyVar" (Number(1.0)) (Number(10.0))]), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestInterpretFunctionWithIndex () =
        let result = parseFormulaString "COUNT(1 + 42, MyVar|1|)"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast varMap DefaultFunctionProvider.Instance
            Assert.AreEqual((DefaultFunctionProvider.Instance.Lookup "COUNT").Execute ([| Number(1.0 + 42.0); varMap.LookupIndex "MyVar" (Number(1.0)) |]), value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretLogicalTrue () =
        let result = parseFormulaString "true"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretLogicalFalse () =
        let result = parseFormulaString "false"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(0.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretInversion () =
        let result = parseFormulaString "!false"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretLogicalAnd () =
        let result = parseFormulaString "true && false"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(0.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretLogicalOr () =
        let result = parseFormulaString "true || false"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretComparisonEqual () =
        let result = parseFormulaString "42 = 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(0.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretComparisonNotEqual () =
        let result = parseFormulaString "42 <> 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretComparisonGreaterThan () =
        let result = parseFormulaString "42 > 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretComparisonLessThan () =
        let result = parseFormulaString "42 < 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(0.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretComparisonGreaterThanEqual () =
        let result = parseFormulaString "42 >= 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretComparisonLessThanEqual () =
        let result = parseFormulaString "42 <= 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(0.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretBranch1 () =
        let result = parseFormulaString "IF true THEN 42 ELSE 1"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(42.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInterpretBranch2 () =
        let result = parseFormulaString "IF(42<=1)THEN(42)ELSE(1)"
        match result with
        | Success (ast, userState, endPos) ->
            let value = interpretFormula ast MapVariableProvider.Empty DefaultFunctionProvider.Instance
            Assert.AreEqual(1.0, value);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
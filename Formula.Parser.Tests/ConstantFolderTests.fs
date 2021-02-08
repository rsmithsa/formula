//-----------------------------------------------------------------------
// <copyright file="ConstantFolderTests.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Tests

open FParsec.CharParsers
open Microsoft.VisualStudio.TestTools.UnitTesting

open Formula.Parser.Ast
open Formula.Parser.Parser
open Formula.Parser.ConstantFolder

[<TestClass>]
type ConstantFolderTests () =

    [<TestMethod>]
    member this.TestFoldConstant () =
        let result = parseFormulaString "42"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Number(42.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
     
    [<TestMethod>]
    member this.TestFoldNegationConstant () =
        let result = parseFormulaString "-42"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Number(-42.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldAdditionConstant () =
        let result = parseFormulaString "1 + 42"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Number(1.0 + 42.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldSubtractionConstant () =
        let result = parseFormulaString "1 - 42"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Number(1.0 - 42.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldMultiplicationConstant () =
        let result = parseFormulaString "1 * 42"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Number(1.0 * 42.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldDivisionConstant () =
        let result = parseFormulaString "1 / 42"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Number(1.0 / 42.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldPowerConstant () =
        let result = parseFormulaString "1 ^ 42"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Number(1.0 ** 42.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldModulusConstant () =
        let result = parseFormulaString "1 % 42"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Number(1.0 % 42.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldOrderOfOperations1 () =
        let result = parseFormulaString "1 + 42 * 2"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Number(1.0 + 42.0 * 2.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldOrderOfOperations2 () =
        let result = parseFormulaString "(1 + 42) * 2"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Number((1.0 + 42.0) * 2.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldOrderOfOperations3 () =
        let result = parseFormulaString "(1 + 42) * 2^3"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Number((1.0 + 42.0) * 2.0 ** 3.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldOrderOfOperations4 () =
        let result = parseFormulaString "1 + 42 % 2"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Number(1.0 + 42.0 % 2.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariable1 () =
        let result = parseFormulaString "MyVar"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Variable(Identifier("MyVar"))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariable2 () =
        let result = parseFormulaString "MyVar1"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Variable(Identifier("MyVar1"))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariable3 () =
        let result = parseFormulaString "_MyVar_1"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Variable(Identifier("_MyVar_1"))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariableOrderOfOperations1 () =
        let result = parseFormulaString "V1 + V42 * V2"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Arithmetic(Variable(Identifier("V1")), Add, Arithmetic(Variable(Identifier("V42")), Multiply, Variable(Identifier("V2"))))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariableOrderOfOperations2 () =
        let result = parseFormulaString "(V1 + V42) * V2"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Arithmetic(Arithmetic(Variable(Identifier("V1")), Add, Variable(Identifier("V42"))), Multiply, Variable(Identifier("V2")))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariableOrderOfOperations3 () =
        let result = parseFormulaString "(V1 + V42) * V2^V3"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Arithmetic(Arithmetic(Variable(Identifier("V1")), Add, Variable(Identifier("V42"))), Multiply, Arithmetic(Variable(Identifier("V2")), Power, Variable(Identifier("V3"))))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariableOrderOfOperations4 () =
        let result = parseFormulaString "V1 + V42 % V2"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Arithmetic(Variable(Identifier("V1")), Add, Arithmetic(Variable(Identifier("V42")), Modulus, Variable(Identifier("V2"))))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldFunction () =
        let result = parseFormulaString "COUNT[]"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Function(Identifier("COUNT"), [])
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldFunctionWithParameters () =
        let result = parseFormulaString "COUNT[1 + 42, MyVar]"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Function(Identifier("COUNT"), [ Constant(Number(1.0 + 42.0)); Variable(Identifier("MyVar")) ])
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldFunctionOfFunction () =
        let result = parseFormulaString "COUNT[COUNT[]]"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Function(Identifier("COUNT"), [ Function(Identifier("COUNT"), []) ])
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldLogicalTrue () =
        let result = parseFormulaString "true"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Boolean(true))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldLogicalFalse () =
        let result = parseFormulaString "false"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Boolean(false))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldInversion () =
        let result = parseFormulaString "!false"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Boolean(not false))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldLogicalAnd () =
        let result = parseFormulaString "true && false"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Boolean(true && false))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldLogicalOr () =
        let result = parseFormulaString "true || false"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Boolean(true || false))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldComparisonEqual () =
        let result = parseFormulaString "42 = 1"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Boolean(42.0 = 1.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldComparisonNotEqual () =
        let result = parseFormulaString "42 <> 1"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Boolean(42.0 <> 1.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldComparisonGreaterThan () =
        let result = parseFormulaString "42 > 1"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Boolean(42.0 > 1.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldComparisonLessThan () =
        let result = parseFormulaString "42 < 1"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Boolean(42.0 < 1.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldComparisonGreaterThanEqual () =
        let result = parseFormulaString "42 >= 1"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Boolean(42.0 >= 1.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldComparisonLessThanEqual () =
        let result = parseFormulaString "42 <= 1"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Boolean(42.0 <= 1.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldBranch1 () =
        let result = parseFormulaString "IF true THEN 42 ELSE 1"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Number(42.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldBranch2 () =
        let result = parseFormulaString "IF(42<=1)THEN(42)ELSE(1)"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = Constant(Number(1.0))
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
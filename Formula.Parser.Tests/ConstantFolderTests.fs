//-----------------------------------------------------------------------
// <copyright file="ConstantFolderTests.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Tests

open FParsec.CharParsers
open Microsoft.VisualStudio.TestTools.UnitTesting

open Formula.Parser
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
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Constant({ Item = Number(42.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
     
    [<TestMethod>]
    member this.TestFoldNegationConstant () =
        let result = parseFormulaString "-42"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Number(-42.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldNegationVariable () =
        let result = parseFormulaString "-ABC"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Negation({ Item = Variable({ Item = Identifier("ABC") }, None, None) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldAdditionConstant () =
        let result = parseFormulaString "1 + 42"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Number(1.0 + 42.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldSubtractionConstant () =
        let result = parseFormulaString "1 - 42"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Number(1.0 - 42.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldMultiplicationConstant () =
        let result = parseFormulaString "1 * 42"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Number(1.0 * 42.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldDivisionConstant () =
        let result = parseFormulaString "1 / 42"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Number(1.0 / 42.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldPowerConstant () =
        let result = parseFormulaString "1 ^ 42"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Number(1.0 ** 42.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldModulusConstant () =
        let result = parseFormulaString "1 % 42"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Number(1.0 % 42.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldOrderOfOperations1 () =
        let result = parseFormulaString "1 + 42 * 2"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Number(1.0 + 42.0 * 2.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldOrderOfOperations2 () =
        let result = parseFormulaString "(1 + 42) * 2"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Number((1.0 + 42.0) * 2.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldOrderOfOperations3 () =
        let result = parseFormulaString "(1 + 42) * 2^3"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Number((1.0 + 42.0) * 2.0 ** 3.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldOrderOfOperations4 () =
        let result = parseFormulaString "1 + 42 % 2"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Number(1.0 + 42.0 % 2.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariable1 () =
        let result = parseFormulaString "MyVar"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Variable({ Item = Identifier("MyVar") }, None, None) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariable2 () =
        let result = parseFormulaString "MyVar1"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Variable({ Item = Identifier("MyVar1") }, None, None) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariable3 () =
        let result = parseFormulaString "_MyVar_1"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Variable({ Item = Identifier("_MyVar_1") }, None, None) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariable4 () =
        let result = parseFormulaString "[My Long Variable]"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Variable({ Item = Identifier("My Long Variable") }, None, None) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariable5 () =
        let result = parseFormulaString "[My Long @$#% Variable 2]"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Variable({ Item = Identifier("My Long @$#% Variable 2") }, None, None) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariableRange1 () =
        let result = parseFormulaString "COUNT(MyVar|1+2:3*4|)"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Function({ Item = Identifier("COUNT") }, [ { Item = Variable({ Item = Identifier("MyVar") }, Some({ Item = Constant({ Item = Number(3.0) }) } :> IAstItem<expr>, { Item = Constant({ Item = Number(12.0) }) } :> IAstItem<expr>), None) } ]) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariableRange2 () =
        let result = parseFormulaString "COUNT(MyVar|true && false:\"2020/01/01\"|)"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Function({ Item = Identifier("COUNT") }, [ { Item = Variable({ Item = Identifier("MyVar") }, Some({ Item = Constant({ Item = Boolean(false) }) } :> IAstItem<expr>, { Item = Constant({ Item = Text("2020/01/01") }) } :> IAstItem<expr>), None) } ]) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariableRange3 () =
        let result = parseFormulaString "COUNT([My Long Variable]| 1 * 0 : 2 / 1 |)"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Function({ Item = Identifier("COUNT") }, [ { Item = Variable({ Item = Identifier("My Long Variable") }, Some({ Item = Constant({ Item = Number(0.0) }) } :> IAstItem<expr>, { Item = Constant({ Item = Number(2.0) }) } :> IAstItem<expr>), None) } ]) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariableRange4 () =
        let result = parseFormulaString "COUNT([My Long Variable]|\"Test\" : false||true|)"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Function({ Item = Identifier("COUNT") }, [ { Item = Variable({ Item = Identifier("My Long Variable") }, Some({ Item = Constant({ Item = Text("Test") }) } :> IAstItem<expr>, { Item = Constant({ Item = Boolean(true) }) } :> IAstItem<expr>), None) } ]) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestFoldVariableIndex1 () =
        let result = parseFormulaString "MyVar|1+2|"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Variable({ Item = Identifier("MyVar") }, None, Some({ Item = Constant({ Item = Number(3.0) }) } :> IAstItem<expr>)) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariableIndex2 () =
        let result = parseFormulaString "MyVar|true && false|"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Variable({ Item = Identifier("MyVar") }, None, Some({ Item = Constant({ Item = Boolean(false) }) } :> IAstItem<expr>)) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariableIndex3 () =
        let result = parseFormulaString "[My Long Variable]| 1 * 0 |"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Variable({ Item = Identifier("My Long Variable") }, None, Some({ Item = Constant({ Item = Number(0.0) }) } :> IAstItem<expr>)) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariableIndex4 () =
        let result = parseFormulaString "[My Long Variable]|\"Test\" |"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Variable({ Item = Identifier("My Long Variable") }, None, Some({ Item = Constant({ Item = Text("Test") }) } :> IAstItem<expr>)) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariableOrderOfOperations1 () =
        let result = parseFormulaString "V1 + V42 * V2"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Arithmetic({ Item = Variable({ Item = Identifier("V1") }, None, None) }, { Item = Add }, { Item = Arithmetic({ Item = Variable({ Item = Identifier("V42") }, None, None) }, { Item = Multiply }, { Item = Variable({ Item = Identifier("V2") }, None, None) }) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariableOrderOfOperations2 () =
        let result = parseFormulaString "(V1 + V42) * V2"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Arithmetic({ Item = Arithmetic({ Item = Variable({ Item = Identifier("V1") }, None, None) }, { Item = Add }, { Item = Variable({ Item = Identifier("V42") }, None, None) }) }, { Item = Multiply }, { Item = Variable({ Item = Identifier("V2") }, None, None) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariableOrderOfOperations3 () =
        let result = parseFormulaString "(V1 + V42) * V2^V3"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Arithmetic({ Item = Arithmetic({ Item = Variable({ Item = Identifier("V1") }, None, None) }, { Item = Add }, { Item = Variable({ Item = Identifier("V42") }, None, None) }) }, { Item = Multiply }, { Item = Arithmetic({ Item = Variable({ Item = Identifier("V2") }, None, None) }, { Item = Power }, { Item = Variable({ Item = Identifier("V3") }, None, None) }) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldVariableOrderOfOperations4 () =
        let result = parseFormulaString "V1 + V42 % V2"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Arithmetic({ Item = Variable({ Item = Identifier("V1") }, None, None) }, { Item = Add }, { Item = Arithmetic({ Item = Variable({ Item = Identifier("V42") }, None, None) }, { Item = Modulus }, { Item = Variable({ Item = Identifier("V2") }, None, None) }) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldFunction () =
        let result = parseFormulaString "COUNT()"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Function({ Item = Identifier("COUNT") }, []) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldFunctionWithParameters () =
        let result = parseFormulaString "COUNT(1 + 42, MyVar)"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Function({ Item = Identifier("COUNT") }, [ { Item = Constant({ Item = Number(1.0 + 42.0) }) }; { Item = Variable({ Item = Identifier("MyVar") }, None, None) } ]) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldFunctionOfFunction () =
        let result = parseFormulaString "COUNT(COUNT())"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Function({ Item = Identifier("COUNT") }, [ { Item = Function({ Item = Identifier("COUNT") }, []) } ]) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldLogicalTrue () =
        let result = parseFormulaString "true"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Constant({ Item = Boolean(true) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldLogicalFalse () =
        let result = parseFormulaString "false"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Constant({ Item = Boolean(false) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldInversion () =
        let result = parseFormulaString "!false"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Boolean(not false) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldInversionVariable () =
        let result = parseFormulaString "!ABC"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Inversion({ Item = Variable({ Item = Identifier("ABC") }, None, None) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldLogicalAnd () =
        let result = parseFormulaString "true && false"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Boolean(true && false) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldLogicalOr () =
        let result = parseFormulaString "true || false"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Boolean(true || false) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldLogicalVariable () =
        let result = parseFormulaString "ABC || DEF"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Logical({ Item = Variable({ Item = Identifier("ABC") }, None, None) }, { Item = Or }, { Item = Variable({ Item = Identifier("DEF") }, None, None) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldComparisonVariable () =
        let result = parseFormulaString "ABC = DEF"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Comparison({ Item = Variable({ Item = Identifier("ABC") }, None, None) }, { Item = Equal }, { Item = Variable({ Item = Identifier("DEF") }, None, None) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldComparisonEqual () =
        let result = parseFormulaString "42 = 1"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Boolean(42.0 = 1.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldComparisonNotEqual () =
        let result = parseFormulaString "42 <> 1"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Boolean(42.0 <> 1.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldComparisonGreaterThan () =
        let result = parseFormulaString "42 > 1"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Boolean(42.0 > 1.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldComparisonLessThan () =
        let result = parseFormulaString "42 < 1"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Boolean(42.0 < 1.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldComparisonGreaterThanEqual () =
        let result = parseFormulaString "42 >= 1"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Boolean(42.0 >= 1.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldComparisonLessThanEqual () =
        let result = parseFormulaString "42 <= 1"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Boolean(42.0 <= 1.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldBranch1 () =
        let result = parseFormulaString "IF true THEN 42 ELSE 1"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Constant({ Item = Number(42.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldBranch2 () =
        let result = parseFormulaString "IF(42<=1)THEN(42)ELSE(1)"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Constant({ Item = Number(1.0) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldBranch3 () =
        let result = parseFormulaString "IF(ABC)THEN 41 + 1 ELSE \"Not\""
        match result with
        | Success (ast, userState, endPos) ->
            let folded = TestHelper.stripPositions (foldConstants ast)
            let expected = { Item = Branch({ Item = Variable({ Item = Identifier("ABC") }, None, None) }, { Item = Constant({ Item = Number(42.0) }) }, { Item = Constant({ Item = Text("Not") }) }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldNothingArithmetic () =
        let result = parseFormulaString "-1 * (42 + null)"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Nothing }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFoldNothingNegation () =
        let result = parseFormulaString "-null"
        match result with
        | Success (ast, userState, endPos) ->
            let folded = foldConstants ast
            let expected = { Item = Constant({ Item = Nothing }) }
            Assert.AreEqual(expected, folded);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
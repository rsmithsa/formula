//-----------------------------------------------------------------------
// <copyright file="ParserTests.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Tests

open System
open FParsec
open FParsec.CharParsers
open Microsoft.VisualStudio.TestTools.UnitTesting

open Formula.Parser
open Formula.Parser.Ast
open Formula.Parser.Parser

[<TestClass>]
type ParserTests () =

    [<TestMethod>]
    member this.TestParseFailure () =
        Assert.ThrowsException<ParserException>(Action(fun x -> parseFormula "+" |> ignore)) |> ignore

    [<TestMethod>]
    member this.TestParseNumberConstant () =
        let result = parseFormulaString "42"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Constant({ Item = Number(42.0) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestParseBooleanConstant () =
        let result = parseFormulaString "true"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Constant({ Item = Boolean(true) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
        let result = parseFormulaString "false"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Constant({ Item = Boolean(false) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestParseTextConstant () =
        let result = parseFormulaString "\"123\""
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Constant({ Item = Text("123") }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestParseTextConstantWithEscaping () =
        let inp = """ "\' \" \\ \0 \a \b \f \n \r \t \v \u00E7" """.Trim()
        let expected = "\' \" \\ \u0000 \a \b \f \n \r \t \v \u00E7"
        let result = parseFormulaString inp
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Constant({ Item = Text(expected) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
     
    [<TestMethod>]
    member this.TestParseNegationConstant () =
        let result = parseFormulaString "-42"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Negation({ Item = Constant({ Item = Number(42.0) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseAdditionConstant () =
        let result = parseFormulaString "1 + 42"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Arithmetic({ Item = Constant({ Item = Number(1.0) }) }, { Item = Add }, { Item = Constant({ Item = Number(42.0) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseSubtractionConstant () =
        let result = parseFormulaString "1 - 42"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Arithmetic({ Item = Constant({ Item = Number(1.0) }) }, { Item = Subtract }, { Item = Constant({ Item = Number(42.0) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseMultiplicationConstant () =
        let result = parseFormulaString "1 * 42"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Arithmetic({ Item = Constant({ Item = Number(1.0) }) }, { Item = Multiply }, { Item = Constant({ Item = Number(42.0) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseDivisionConstant () =
        let result = parseFormulaString "1 / 42"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Arithmetic({ Item = Constant({ Item = Number(1.0) }) }, { Item = Divide }, { Item = Constant({ Item = Number(42.0) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParsePowerConstant () =
        let result = parseFormulaString "1 ^ 42"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Arithmetic({ Item = Constant({ Item = Number(1.0) }) }, { Item = Power }, { Item = Constant({ Item = Number(42.0) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseModulusConstant () =
        let result = parseFormulaString "1 % 42"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Arithmetic({ Item = Constant({ Item = Number(1.0) }) }, { Item = Modulus }, { Item = Constant({ Item = Number(42.0) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseOrderOfOperations1 () =
        let result = parseFormulaString "1 + 42 * 2"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Arithmetic({ Item = Constant({ Item = Number(1.0) }) }, { Item = Add }, { Item = Arithmetic({ Item = Constant({ Item = Number(42.0) }) }, { Item = Multiply }, { Item = Constant({ Item = Number(2.0) }) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseOrderOfOperations2 () =
        let result = parseFormulaString "(1 + 42) * 2"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Arithmetic({ Item = Arithmetic({ Item = Constant({ Item = Number(1.0) }) }, { Item = Add }, { Item = Constant({ Item = Number(42.0) }) }) }, { Item = Multiply }, { Item = Constant({ Item = Number(2.0) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseOrderOfOperations3 () =
        let result = parseFormulaString "(1 + 42) * 2^3"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Arithmetic({ Item = Arithmetic({ Item = Constant({ Item = Number(1.0) }) }, { Item = Add }, { Item = Constant({ Item = Number(42.0) }) }) }, { Item = Multiply }, { Item = Arithmetic({ Item = Constant({ Item = Number(2.0) }) }, { Item = Power }, { Item = Constant({ Item = Number(3.0) }) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseOrderOfOperations4 () =
        let result = parseFormulaString "1 + 42 % 2"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Arithmetic({ Item = Constant({ Item = Number(1.0) }) }, { Item = Add }, { Item = Arithmetic({ Item = Constant({ Item = Number(42.0) }) }, { Item = Modulus }, { Item = Constant({ Item = Number(2.0) }) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariable1 () =
        let result = parseFormulaString "MyVar"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Variable({ Item = Identifier("MyVar") }, None, None) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariable2 () =
        let result = parseFormulaString "MyVar1"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Variable({ Item = Identifier("MyVar1") }, None, None) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariable3 () =
        let result = parseFormulaString "_MyVar_1"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Variable({ Item = Identifier("_MyVar_1") }, None, None) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariable4 () =
        let result = parseFormulaString "[My Long Variable]"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Variable({ Item = Identifier("My Long Variable") }, None, None) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariable5 () =
        let result = parseFormulaString "[My Long @$#% Variable 2]"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Variable({ Item = Identifier("My Long @$#% Variable 2") }, None, None) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariableRange1 () =
        let result = parseFormulaString "MyVar|1:2|"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Variable({ Item = Identifier("MyVar") }, Some({ Item = Constant({ Item = Number(1.0) }) } :> IAstItem<expr>, { Item = Constant({ Item = Number(2.0) }) } :> IAstItem<expr>), None) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariableRange2 () =
        let result = parseFormulaString "MyVar|true:\"2020/01/01\"|"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Variable({ Item = Identifier("MyVar") }, Some({ Item = Constant({ Item = Boolean(true) }) } :> IAstItem<expr>, { Item = Constant({ Item = Text("2020/01/01") }) } :> IAstItem<expr>), None) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariableRange3 () =
        let result = parseFormulaString "[My Long Variable]| 1 : 2 |"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Variable({ Item = Identifier("My Long Variable") }, Some({ Item = Constant({ Item = Number(1.0) }) } :> IAstItem<expr>, { Item = Constant({ Item = Number(2.0) }) } :> IAstItem<expr>), None) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariableRange4 () =
        let result = parseFormulaString "[My Long Variable]|\"Test\" : false|"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Variable({ Item = Identifier("My Long Variable") }, Some({ Item = Constant({ Item = Text("Test") }) } :> IAstItem<expr>, { Item = Constant({ Item = Boolean(false) }) } :> IAstItem<expr>), None) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariableRange5 () =
        let result = parseFormulaString "MyVar|A:B|"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Variable({ Item = Identifier("MyVar") }, Some({ Item = Variable({ Item = Identifier("A") }, None, None) } :> IAstItem<expr>, { Item = Variable({ Item = Identifier("B") }, None, None) } :> IAstItem<expr>), None) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestParseVariableIndex1 () =
        let result = parseFormulaString "MyVar|1|"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Variable({ Item = Identifier("MyVar") }, None, Some({ Item = Constant({ Item = Number(1.0) }) } :> IAstItem<expr>)) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
    
    [<TestMethod>]
    member this.TestParseVariableIndex2 () =
        let result = parseFormulaString "MyVar|true|"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Variable({ Item = Identifier("MyVar") }, None, Some({ Item = Constant({ Item = Boolean(true) }) } :> IAstItem<expr>)) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariableIndex3 () =
        let result = parseFormulaString "[My Long Variable]| 1 |"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Variable({ Item = Identifier("My Long Variable") }, None, Some({ Item = Constant({ Item = Number(1.0) }) } :> IAstItem<expr>)) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariableIndex4 () =
        let result = parseFormulaString "[My Long Variable]|\"Test\" |"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Variable({ Item = Identifier("My Long Variable") }, None, Some({ Item = Constant({ Item = Text("Test") }) } :> IAstItem<expr>)) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariableIndex5 () =
        let result = parseFormulaString "MyVar|A|"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Variable({ Item = Identifier("MyVar") }, None, Some({ Item = Variable({ Item = Identifier("A") }, None, None) } :> IAstItem<expr>)) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariableOrderOfOperations1 () =
        let result = parseFormulaString "V1 + V42 * V2"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Arithmetic({ Item = Variable({ Item = Identifier("V1") }, None, None) }, { Item = Add }, { Item = Arithmetic({ Item = Variable({ Item = Identifier("V42") }, None, None) }, { Item = Multiply }, { Item = Variable({ Item = Identifier("V2") }, None, None) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariableOrderOfOperations2 () =
        let result = parseFormulaString "(V1 + V42) * V2"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Arithmetic({ Item = Arithmetic({ Item = Variable({ Item = Identifier("V1") }, None, None) }, { Item = Add }, { Item = Variable({ Item = Identifier("V42") }, None, None) }) }, { Item = Multiply }, { Item = Variable({ Item = Identifier("V2") }, None, None) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariableOrderOfOperations3 () =
        let result = parseFormulaString "(V1 + V42) * V2^V3"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Arithmetic({ Item = Arithmetic({ Item = Variable({ Item = Identifier("V1") }, None, None) }, { Item = Add }, { Item = Variable({ Item = Identifier("V42") }, None, None) }) }, { Item = Multiply }, { Item = Arithmetic({ Item = Variable({ Item = Identifier("V2") }, None, None) }, { Item = Power }, { Item = Variable({ Item = Identifier("V3") }, None, None) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseVariableOrderOfOperations4 () =
        let result = parseFormulaString "V1 + V42 % V2"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Arithmetic({ Item = Variable({ Item = Identifier("V1") }, None, None) }, { Item = Add }, { Item = Arithmetic({ Item = Variable({ Item = Identifier("V42") }, None, None) }, { Item = Modulus }, { Item = Variable({ Item = Identifier("V2") }, None, None) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseFunction () =
        let result = parseFormulaString "COUNT()"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Function({ Item = Identifier("COUNT") }, []) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseFunctionWithParameters () =
        let result = parseFormulaString "COUNT(1 + 42, MyVar)"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Function({ Item = Identifier("COUNT") }, [ { Item = Arithmetic({ Item = Constant({ Item = Number(1.0) }) }, { Item = Add }, { Item = Constant({ Item = Number(42.0) }) }) }; { Item = Variable({ Item = Identifier("MyVar") }, None, None) } ]) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseFunctionOfFunction () =
        let result = parseFormulaString "COUNT(COUNT())"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Function({ Item = Identifier("COUNT") }, [ { Item = Function({ Item = Identifier("COUNT") }, []) } ]) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestParseFunctionWithRanges () =
        let result = parseFormulaString "COUNT(1 + 42, MyVar|1:2|)"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Function({ Item = Identifier("COUNT") }, [ { Item = Arithmetic({ Item = Constant({ Item = Number(1.0) }) }, { Item = Add }, { Item = Constant({ Item = Number(42.0) }) }) }; { Item = Variable({ Item = Identifier("MyVar") }, Some({ Item = Constant({ Item = Number(1.0) }) } :> IAstItem<expr>, { Item = Constant({ Item = Number(2.0) }) } :> IAstItem<expr>), None) } ]) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestParseFunctionWithRangesFail () =
        let result = parseFormulaString "COUNT(1 + 42, MyVar|1:2| + MyVar|1:2|)"
        match result with
        | Success (ast, userState, endPos) ->
            let actual = TestHelper.stripPositions ast
            Assert.Fail($"{actual}")
        | Failure (msg, error, userState) ->
            Assert.AreEqual("Ranges are not supported outside of function parameters", (error.Messages.Head :?> ErrorMessage.Message).String)
            

    [<TestMethod>]
    member this.TestParseLogicalTrue () =
        let result = parseFormulaString "true"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Constant({ Item = Boolean(true) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseLogicalFalse () =
        let result = parseFormulaString "false"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Constant({ Item = Boolean(false) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseInversion () =
        let result = parseFormulaString "!false"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Inversion({ Item = Constant({ Item = Boolean(false) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseLogicalAnd () =
        let result = parseFormulaString "true && false"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Logical({ Item = Constant({ Item = Boolean(true) }) }, { Item = And }, { Item = Constant({ Item = Boolean(false) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseLogicalOr () =
        let result = parseFormulaString "true || false"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Logical({ Item = Constant({ Item = Boolean(true) }) }, { Item = Or }, { Item = Constant({ Item = Boolean(false) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseComparisonEqual () =
        let result = parseFormulaString "42 = 1"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Comparison({ Item = Constant({ Item = Number(42.0) }) }, { Item = Equal }, { Item = Constant({ Item = Number(1.0) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseComparisonNotEqual () =
        let result = parseFormulaString "42 <> 1"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Comparison({ Item = Constant({ Item = Number(42.0) }) }, { Item = NotEqual }, { Item = Constant({ Item = Number(1.0) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseComparisonGreaterThan () =
        let result = parseFormulaString "42 > 1"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Comparison({ Item = Constant({ Item = Number(42.0) }) }, { Item = GreaterThan }, { Item = Constant({ Item = Number(1.0) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseComparisonLessThan () =
        let result = parseFormulaString "42 < 1"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Comparison({ Item = Constant({ Item = Number(42.0) }) }, { Item = LessThan }, { Item = Constant({ Item = Number(1.0) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseComparisonGreaterThanEqual () =
        let result = parseFormulaString "42 >= 1"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Comparison({ Item = Constant({ Item = Number(42.0) }) }, { Item = GreaterThanEqual }, { Item = Constant({ Item = Number(1.0) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseComparisonLessThanEqual () =
        let result = parseFormulaString "42 <= 1"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Comparison({ Item = Constant({ Item = Number(42.0) }) }, { Item = LessThanEqual }, { Item = Constant({ Item = Number(1.0) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseBranch1 () =
        let result = parseFormulaString "IF true THEN 42 ELSE 1"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Branch({ Item = Constant({ Item = Boolean(true) }) }, { Item = Constant({ Item = Number(42.0) }) }, { Item = Constant({ Item = Number(1.0) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestParseBranch2 () =
        let result = parseFormulaString "IF(42<=1)THEN(42)ELSE(1)"
        match result with
        | Success (ast, userState, endPos) ->
            let expected = { Item = Branch({ Item = Comparison({ Item = Constant({ Item = Number(42.0) }) }, { Item = LessThanEqual }, { Item = Constant({ Item = Number(1.0) }) }) }, { Item = Constant({ Item = Number(42.0) }) }, { Item = Constant({ Item = Number(1.0) }) }) }
            Assert.AreEqual(expected, TestHelper.stripPositions ast);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
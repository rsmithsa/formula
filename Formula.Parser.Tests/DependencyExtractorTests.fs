//-----------------------------------------------------------------------
// <copyright file="DependencyExtractorTests.fs" company="Richard Smith">
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
open Formula.Parser.DependencyExtractor

[<TestClass>]
type DependencyExtractorTests () =

    let getSimpleDependencyList (list: IAstItem<identifier> list) =
        list
        |> List.map (fun x -> x.Item)

    let getSimpleRangeDependencyList (list: (IAstItem<identifier> * (IAstItem<expr> * IAstItem<expr>) option) list) =
        list
        |> List.map (
            fun x ->
            (
                match snd x with
                | Some (a, b) -> ((fst x).Item, Some(a.Item, b.Item))
                | None -> ((fst x).Item, None)
            )
        )
    
    [<TestMethod>]
    member this.TestConstantDependencies () =
        let result = parseFormulaString "42"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleDependencyList (extractDependencies ast [])
            let expected = List.empty<identifier>
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestVariableDependencies () =
        let result = parseFormulaString "[My Var]"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleDependencyList (extractDependencies ast [])
            let expected = [ Identifier("My Var") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestVariableRangeDependencies () =
        let result = parseFormulaString "[My Var]|A:B|"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleDependencyList (extractDependencies ast [])
            let expected = [ Identifier("B"); Identifier("A"); Identifier("My Var") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestVariableRangeDependencies2 () =
        let result = parseFormulaString "[My Var]|1:1|"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleDependencyList (extractDependencies ast [])
            let expected = [ Identifier("My Var") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestVariableRangeDependencies3 () =
        let result = parseFormulaString "[My Var]|PI():PI()|"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleDependencyList (extractDependencies ast [])
            let expected = [ Identifier("My Var") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFunctionDependencies () =
        let result = parseFormulaString "POW(SQRT([My Var]), [A])"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleDependencyList (extractDependencies ast [])
            let expected = [ Identifier("My Var"); Identifier("A") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestFunctionRangeDependencies () =
        let result = parseFormulaString "POW(SQRT([My Var]|PI():PI()|), [A])"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleDependencyList (extractDependencies ast [])
            let expected = [ Identifier("My Var"); Identifier("A") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)      

    [<TestMethod>]
    member this.TestBranchDependencies () =
        let result = parseFormulaString "IF [My Var] THEN [A] ELSE B"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleDependencyList (extractDependencies ast [])
            let expected = [ Identifier("B"); Identifier("A"); Identifier("My Var") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestComparisonDependencies () =
        let result = parseFormulaString "-[My Var] = [A]"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleDependencyList (extractDependencies ast [])
            let expected = [ Identifier("A"); Identifier("My Var") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestLogicalDependencies () =
        let result = parseFormulaString "[My Var] || ![A]"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleDependencyList (extractDependencies ast [])
            let expected = [ Identifier("A"); Identifier("My Var") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestNegationDependencies () =
        let result = parseFormulaString "-[A]"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleDependencyList (extractDependencies ast [])
            let expected = [ Identifier("A") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInversionDependencies () =
        let result = parseFormulaString "![A]"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleDependencyList (extractDependencies ast [])
            let expected = [ Identifier("A") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestArithmeticDependencies () =
        let result = parseFormulaString "[My Var] * [A] % B"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleDependencyList (extractDependencies ast [])
            let expected = [ Identifier("B"); Identifier("A"); Identifier("My Var") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestConstantDependenciesWithRanges () =
        let result = parseFormulaString "42"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleRangeDependencyList (extractDependenciesWithRanges ast [])
            let expected = List.empty<identifier * option<expr * expr>>
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestConstantRangeDependenciesWithRanges () =
        let result = parseFormulaString "[My Var]|1:0|"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleRangeDependencyList (extractDependenciesWithRanges (TestHelper.stripPositions ast) [])
            let expected: list<identifier * option<expr * expr>> = [ (Identifier("My Var"), Some(Constant({ Item = Number(1.0) }), Constant({ Item = Number(0.0) }))) ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestVariableDependenciesWithRanges () =
        let result = parseFormulaString "[My Var]"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleRangeDependencyList (extractDependenciesWithRanges ast [])
            let expected: list<identifier * option<expr * expr>> = [ (Identifier("My Var"), None) ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestVariableRangeDependenciesWithRanges () =
        let result = parseFormulaString "[My Var]|A:B|"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleRangeDependencyList (extractDependenciesWithRanges (TestHelper.stripPositions ast) [])
            let expected: list<identifier * option<expr * expr>> = [ (Identifier("B"), None); (Identifier("A"), None); (Identifier("My Var"), Some(Variable({ Item = Identifier("A") }, None), Variable({ Item = Identifier("B") }, None))) ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestVariableRangeDependenciesWithRanges2 () =
        let result = parseFormulaString "[My Var]|PI():PI()|"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleRangeDependencyList (extractDependenciesWithRanges (TestHelper.stripPositions ast) [])
            let expected: list<identifier * option<expr * expr>> = [ (Identifier("My Var"), Some(Function({ Item = Identifier("PI") }, []), Function({ Item = Identifier("PI") }, []))) ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFunctionDependenciesWithRanges () =
        let result = parseFormulaString "POW(SQRT([My Var]), [A])"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleRangeDependencyList (extractDependenciesWithRanges ast [])
            let expected: list<identifier * option<expr * expr>> = [ (Identifier("My Var"), None); (Identifier("A"), None) ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestBranchDependenciesWithRanges () =
        let result = parseFormulaString "IF [My Var] THEN [A] ELSE B"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleRangeDependencyList (extractDependenciesWithRanges ast [])
            let expected: list<identifier * option<expr * expr>> = [ (Identifier("B"), None); (Identifier("A"), None); (Identifier("My Var"), None) ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestComparisonDependenciesWithRanges () =
        let result = parseFormulaString "-[My Var] = [A]"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleRangeDependencyList (extractDependenciesWithRanges ast [])
            let expected: list<identifier * option<expr * expr>> = [ (Identifier("A"), None); (Identifier("My Var"), None) ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestLogicalDependenciesWithRanges () =
        let result = parseFormulaString "[My Var] || ![A]"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleRangeDependencyList (extractDependenciesWithRanges ast [])
            let expected: list<identifier * option<expr * expr>> = [ (Identifier("A"), None); (Identifier("My Var"), None) ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestNegationDependenciesWithRanges () =
        let result = parseFormulaString "-[A]"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleRangeDependencyList (extractDependenciesWithRanges ast [])
            let expected: list<identifier * option<expr * expr>> = [ (Identifier("A"), None) ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInversionDependenciesWithRanges () =
        let result = parseFormulaString "![A]"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleRangeDependencyList (extractDependenciesWithRanges ast [])
            let expected: list<identifier * option<expr * expr>> = [ (Identifier("A"), None) ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestArithmeticDependenciesWithRanges () =
        let result = parseFormulaString "[My Var] * [A] % B"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = getSimpleRangeDependencyList (extractDependenciesWithRanges ast [])
            let expected: list<identifier * option<expr * expr>> = [ (Identifier("B"), None); (Identifier("A"), None); (Identifier("My Var"), None) ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
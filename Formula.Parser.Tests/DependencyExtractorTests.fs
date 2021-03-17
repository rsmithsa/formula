//-----------------------------------------------------------------------
// <copyright file="DependencyExtractorTests.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Tests

open System
open FParsec.CharParsers
open Microsoft.VisualStudio.TestTools.UnitTesting

open Formula.Parser.Ast
open Formula.Parser.Parser
open Formula.Parser.DependencyExtractor

[<TestClass>]
type DependencyExtractorTests () =

    [<TestMethod>]
    member this.TestConstantDependencies () =
        let result = parseFormulaString "42"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = extractDependencies ast []
            let expected = List.empty<identifier>
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestVariableDependencies () =
        let result = parseFormulaString "[My Var]"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = extractDependencies ast []
            let expected = [ Identifier("My Var") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestVariableRangeDependencies () =
        let result = parseFormulaString "[My Var]|A:B|"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = extractDependencies ast []
            let expected = [ Identifier("B"); Identifier("A"); Identifier("My Var") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestFunctionDependencies () =
        let result = parseFormulaString "POW(SQRT([My Var]), [A])"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = extractDependencies ast []
            let expected = [ Identifier("My Var"); Identifier("A") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestBranchDependencies () =
        let result = parseFormulaString "IF [My Var] THEN [A] ELSE B"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = extractDependencies ast []
            let expected = [ Identifier("B"); Identifier("A"); Identifier("My Var") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestComparisonDependencies () =
        let result = parseFormulaString "-[My Var] = [A]"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = extractDependencies ast []
            let expected = [ Identifier("A"); Identifier("My Var") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestLogicalDependencies () =
        let result = parseFormulaString "[My Var] || ![A]"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = extractDependencies ast []
            let expected = [ Identifier("A"); Identifier("My Var") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestNegationDependencies () =
        let result = parseFormulaString "-[A]"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = extractDependencies ast []
            let expected = [ Identifier("A") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestInversionDependencies () =
        let result = parseFormulaString "![A]"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = extractDependencies ast []
            let expected = [ Identifier("A") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member this.TestArithmeticDependencies () =
        let result = parseFormulaString "[My Var] * [A] % B"
        match result with
        | Success (ast, userState, endPos) ->
            let deps = extractDependencies ast []
            let expected = [ Identifier("B"); Identifier("A"); Identifier("My Var") ]
            Assert.AreEqual(expected, deps);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
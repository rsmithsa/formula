//-----------------------------------------------------------------------
// <copyright file="FunctionValidatorTests.fs" company="Richard Smith">
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
open Formula.Parser.FunctionValidator
open Formula.Parser.Integration

[<TestClass>]
type FunctionValidatorTests () =
    
    let getSimpleErrorList (list: (string * IPositionedAstItem<identifier>) list) =
        list
        |> List.map (fun (s, a) -> a.Item)
    
    [<TestMethod>]
    member this.TestNoFunction () =
        let result = parseFormulaString "42 + 1 + [A]"
        match result with
        | Success (ast, userState, endPos) ->
            let errors = getSimpleErrorList (validateFunctions ast DefaultFunctionProvider.Instance [])
            let expected = List.empty<identifier>
            Assert.AreEqual(expected, errors);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestMissingFunction () =
        let result = parseFormulaString "NOTAFUNC()"
        match result with
        | Success (ast, userState, endPos) ->
            let errors = getSimpleErrorList (validateFunctions ast DefaultFunctionProvider.Instance [])
            let expected = [ Identifier("NOTAFUNC") ]
            Assert.AreEqual(expected, errors);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestKnownFunction () =
        let result = parseFormulaString "COUNT()"
        match result with
        | Success (ast, userState, endPos) ->
            let errors = getSimpleErrorList (validateFunctions ast DefaultFunctionProvider.Instance [])
            let expected = List.empty<identifier>
            Assert.AreEqual(expected, errors);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestNestedMissingFunction () =
        let result = parseFormulaString "COUNT(1, [A], NOTAFUNC())"
        match result with
        | Success (ast, userState, endPos) ->
            let errors = getSimpleErrorList (validateFunctions ast DefaultFunctionProvider.Instance [])
            let expected = [ Identifier("NOTAFUNC") ]
            Assert.AreEqual(expected, errors);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
            
    [<TestMethod>]
    member this.TestNestedKnownFunction () =
        let result = parseFormulaString "COUNT(PI(), PI())"
        match result with
        | Success (ast, userState, endPos) ->
            let errors = getSimpleErrorList (validateFunctions ast DefaultFunctionProvider.Instance [])
            let expected = List.empty<identifier>
            Assert.AreEqual(expected, errors);
        | Failure (msg, error, userState) ->
            Assert.Fail(msg)
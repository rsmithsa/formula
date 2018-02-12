//-----------------------------------------------------------------------
// <copyright file="CsWrapper.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open System
open FParsec.CharParsers

open Formula.Parser.Parser
open Formula.Parser.ConstantFolder
open Formula.Parser.Interpreter
open Formula.Parser

[<AbstractClass; Sealed>]
type CsWrapper private() =

    static member ParseFormula input =
        let result = parseFormulaString input
        match result with
        | Success (ast, a, b) ->
            ast
        | Failure (msg, a, b) ->
            raise (ArgumentException(msg, "input"))

    static member ConstantFoldExpression ast =
        foldConstants ast

    static member InterpretExpression (ast) =
        CsWrapper.InterpretExpression(ast, MapVariableProvider.Empty, DefaultFunctionProvider.Instance)

    static member InterpretExpression (ast, (variableProvider: IVariableProvider)) =
        CsWrapper.InterpretExpression(ast, variableProvider, DefaultFunctionProvider.Instance)

    static member InterpretExpression (ast, (functionProvider: IFunctionProvider)) =
        CsWrapper.InterpretExpression(ast, MapVariableProvider.Empty, functionProvider)

    static member InterpretExpression (ast, (variableProvider: IVariableProvider), (functionProvider: IFunctionProvider)) =
        interpretFormula ast variableProvider functionProvider

    static member InterpretFormula (input) =
        CsWrapper.InterpretFormula(input, MapVariableProvider.Empty, DefaultFunctionProvider.Instance)

    static member InterpretFormula (input, (variableProvider: IVariableProvider)) =
        CsWrapper.InterpretFormula(input, variableProvider, DefaultFunctionProvider.Instance)
    
    static member InterpretFormula (input, (functionProvider: IFunctionProvider)) =
        CsWrapper.InterpretFormula(input, MapVariableProvider.Empty, functionProvider)

    static member InterpretFormula (input, (variableProvider: IVariableProvider), (functionProvider: IFunctionProvider)) =
        let result = parseFormulaString input
        match result with
        | Success (ast, userState, endPos) ->
            interpretFormula ast variableProvider functionProvider
        | Failure (msg, error, userState) ->
            raise (ArgumentException(msg, "input"))

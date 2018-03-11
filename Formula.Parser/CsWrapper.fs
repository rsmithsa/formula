//-----------------------------------------------------------------------
// <copyright file="CsWrapper.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open FParsec.CharParsers

open Formula.Parser.Ast
open Formula.Parser.Parser
open Formula.Parser.DependencyExtractor
open Formula.Parser.ConstantFolder
open Formula.Parser.Interpreter
open Formula.Parser.Compiler
open Formula.Parser

[<AbstractClass; Sealed>]
type CsWrapper private() =

    static member ParseFormula input =
        parseFormula input

    static member ExtractExpressionDependencies ast =
        let deps =
            extractDependencies ast []
            |> List.map (fun x ->
                match x with
                | Identifier i -> i
            )
        new System.Collections.Generic.HashSet<string>(deps)
    
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

    static member CompileExpression ast =
        compileFormula ast

    static member InterpretFormula (input) =
        CsWrapper.InterpretFormula(input, MapVariableProvider.Empty, DefaultFunctionProvider.Instance)

    static member InterpretFormula (input, (variableProvider: IVariableProvider)) =
        CsWrapper.InterpretFormula(input, variableProvider, DefaultFunctionProvider.Instance)
    
    static member InterpretFormula (input, (functionProvider: IFunctionProvider)) =
        CsWrapper.InterpretFormula(input, MapVariableProvider.Empty, functionProvider)

    static member InterpretFormula (input, (variableProvider: IVariableProvider), (functionProvider: IFunctionProvider)) =
        let ast = parseFormula input
        interpretFormula ast variableProvider functionProvider

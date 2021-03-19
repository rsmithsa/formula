//-----------------------------------------------------------------------
// <copyright file="CsWrapper.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open System
open System.Linq;
open FParsec

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
            |> Seq.map (fun x ->
                match x.item with
                | Identifier i -> i
            )
        new System.Collections.Generic.HashSet<string>(deps)

    static member ExtractExpressionDependenciesWithRanges ast =
        let deps =
            extractDependenciesWithRanges ast []
            |> Seq.map (fun x ->
                match x with
                | ({ item = Identifier i }, r) ->
                    match r with
                    | Some ({ item = Constant a }, { item = Constant b }) -> struct (i, a.item, b.item)
                    | _ -> struct (i, Unchecked.defaultof<value>, Unchecked.defaultof<value>)
            )
        new System.Collections.Generic.HashSet<ValueTuple<string, value, value>>(deps)
    
    static member ExtractExpressionDependenciesWithPositions ast =
        let deps =
            extractDependencies ast []
            |> Seq.map (fun x ->
                match x with
                | { item = Identifier i } -> (i, struct (x.startPosition, x.endPosition))
            )
            |> Seq.groupBy (fun x -> fst x)
        deps.ToDictionary(fst, fun x -> (snd x |> Seq.map snd).ToList())

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

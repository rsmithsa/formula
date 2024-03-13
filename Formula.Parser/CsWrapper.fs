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
open Formula.Parser.FunctionValidator
open Formula.Parser.ConstantFolder
open Formula.Parser.Interpreter
open Formula.Parser

[<AbstractClass; Sealed>]
type CsWrapper private() =

    static member ParseFormula input =
        parseFormula input

    static member ExtractExpressionDependencies ast =
        let deps =
            extractDependencies ast []
            |> Seq.map (fun x ->
                match x.Item with
                | Identifier i -> i
            )
        new System.Collections.Generic.HashSet<string>(deps)

    static member ExtractExpressionDependenciesWithRanges ast =
        let deps =
            extractDependenciesWithRanges (foldConstants ast) []
            |> Seq.map (fun x ->
                let (i, r) = x
                match (i.Item, r) with
                | (Identifier i, r) ->
                    match r with
                    | Some (a, b) ->
                        match (a.Item, b.Item) with
                        | (Constant a, Constant b) -> (i, struct (i, a.Item, b.Item))
                        | (Constant a, _) -> (i, struct (i, a.Item, Unchecked.defaultof<value>))
                        | (_, Constant b) -> (i, struct (i, Unchecked.defaultof<value>, b.Item))
                        | _ -> (i, struct (i, Unchecked.defaultof<value>, Unchecked.defaultof<value>))
                    | _ -> (i, struct (i, Number(0), Number(0)))
            )
            |> Map.ofSeq
        new System.Collections.Generic.Dictionary<string, ValueTuple<string, value, value>>(deps)
    
    static member ExtractExpressionDependenciesWithPositions (ast: IPositionedAstItem<expr>) =
        let deps =
            extractDependencies ast []
            |> Seq.map (fun x ->
                let t = x :?> IPositionedAstItem<identifier>
                match t.Item with
                | Identifier i -> (i, struct (t.StartPosition, t.EndPosition))
            )
            |> Seq.groupBy (fun x -> fst x)
        deps.ToDictionary(fst, fun x -> (snd x |> Seq.map snd).ToList())

    static member ValidateFunctions (ast, (functionProvider: IFunctionProvider)) =
        let errors =
            validateFunctions ast functionProvider []
            |> Seq.map (fun (m, x) ->
                match x.Item with
                | Identifier i -> struct (m, i, x.StartPosition, x.EndPosition)
            )
        errors.ToList()
    
    static member ConstantFoldExpression ast =
        foldConstants ast
        
    static member ConstantFoldExpression (ast, (functionProvider: IFunctionProvider)) =
        foldConstantsFunctions ast functionProvider

    static member InterpretExpression (ast) =
        CsWrapper.InterpretExpression(ast, MapVariableProvider.Empty, DefaultFunctionProvider.Instance)

    static member InterpretExpression (ast, (variableProvider: IVariableProvider)) =
        CsWrapper.InterpretExpression(ast, variableProvider, DefaultFunctionProvider.Instance)

    static member InterpretExpression (ast, (functionProvider: IFunctionProvider)) =
        CsWrapper.InterpretExpression(ast, MapVariableProvider.Empty, functionProvider)

    static member InterpretExpression (ast, (variableProvider: IVariableProvider), (functionProvider: IFunctionProvider)) =
        let result = interpretFormula ast variableProvider functionProvider
        match result with
        | Some x -> Nullable(x)
        | _ -> Nullable()

    static member CompileExpression ast =
        Compiler.compileFormula<Nullable<double>> ast

    static member ILCompileExpression ast =
        ILCompiler.compileFormula<Nullable<double>> ast
    
    static member InterpretFormula (input) =
        CsWrapper.InterpretFormula(input, MapVariableProvider.Empty, DefaultFunctionProvider.Instance)

    static member InterpretFormula (input, (variableProvider: IVariableProvider)) =
        CsWrapper.InterpretFormula(input, variableProvider, DefaultFunctionProvider.Instance)
    
    static member InterpretFormula (input, (functionProvider: IFunctionProvider)) =
        CsWrapper.InterpretFormula(input, MapVariableProvider.Empty, functionProvider)

    static member InterpretFormula (input, (variableProvider: IVariableProvider), (functionProvider: IFunctionProvider)) =
        let ast = parseFormula input
        let result = interpretFormula ast variableProvider functionProvider
        match result with
        | Some x -> Nullable(x)
        | _ -> Nullable()

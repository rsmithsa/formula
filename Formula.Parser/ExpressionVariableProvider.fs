//-----------------------------------------------------------------------
// <copyright file="ExpressionVariableProvider.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open Formula.Parser
open Formula.Parser.Ast
open Formula.Parser.Parser
open Formula.Parser.ConstantFolder
open Formula.Parser.Compiler

type ExpressionVariableProvider(expressionMap: Map<string, expr>, functionProvider: IFunctionProvider) =

    static let dictionaryToMap (dictionary : System.Collections.Generic.IDictionary<_, _>) = 
        dictionary 
        |> Seq.map (|KeyValue|)  
        |> Map.ofSeq
    
    new(expressions: System.Collections.Generic.IDictionary<string, expr>, functionProvider: IFunctionProvider) = ExpressionVariableProvider(dictionaryToMap expressions, functionProvider)

    new(expressions: System.Collections.Generic.IDictionary<string, string>, functionProvider: IFunctionProvider) =
        let expressionMap =
            dictionaryToMap expressions
            |> Map.toSeq
            |> Seq.map (fun (x, y) -> (x, parseFormula y))
            |> Map.ofSeq

        ExpressionVariableProvider(expressionMap, functionProvider)

    member this.KnownExpressions: Map<string, expr> = expressionMap

    member this.CompiledExpressions =
        expressionMap
        |> Map.toSeq
        |> Seq.map (fun (x, y) -> (x, foldConstants y |> compileFormula))
        |> Map.ofSeq

    member this.IsDefined name = 
        this.KnownExpressions.ContainsKey name
    member this.Lookup name =
        this.CompiledExpressions.[name].Invoke(this, functionProvider)

    interface IVariableProvider with 
        member this.IsDefined name = this.IsDefined name
        member this.Lookup name = this.Lookup name

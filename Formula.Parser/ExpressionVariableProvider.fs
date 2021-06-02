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

type ExpressionVariableProvider(expressionMap: Map<string, IAstItem<expr>>, functionProvider: IFunctionProvider, ?variableProvider: IVariableProvider) =

    static let dictionaryToMap (dictionary : System.Collections.Generic.IDictionary<_, _>) = 
        dictionary 
        |> Seq.map (|KeyValue|)  
        |> Map.ofSeq
    
    new(expressions: System.Collections.Generic.IDictionary<string, IAstItem<expr>>, functionProvider: IFunctionProvider, variableProvider: IVariableProvider) = ExpressionVariableProvider(dictionaryToMap expressions, functionProvider, variableProvider)

    new(expressions: System.Collections.Generic.IDictionary<string, string>, functionProvider: IFunctionProvider, variableProvider: IVariableProvider) =
        let expressionMap =
            dictionaryToMap expressions
            |> Map.toSeq
            |> Seq.map (fun (x, y) -> (x, parseFormula y :> IAstItem<expr>))
            |> Map.ofSeq

        ExpressionVariableProvider(expressionMap, functionProvider, variableProvider)

    new(expressions: System.Collections.Generic.IDictionary<string, IAstItem<expr>>, functionProvider: IFunctionProvider) = ExpressionVariableProvider(expressions, functionProvider, null)

    new(expressions: System.Collections.Generic.IDictionary<string, string>, functionProvider: IFunctionProvider) = ExpressionVariableProvider(expressions, functionProvider, null)

    member this.KnownExpressions: Map<string, IAstItem<expr>> = expressionMap

    member this.CompiledExpressions =
        expressionMap
        |> Map.toSeq
        |> Seq.map (fun (x, y) -> (x, foldConstants y |> compileFormula))
        |> Map.ofSeq

    member this.IsDefined name =
        match variableProvider with
        | None -> this.KnownExpressions.ContainsKey name
        | Some v ->
            match this.KnownExpressions.ContainsKey name with
            | true -> true
            | false -> v.IsDefined (name, this)
    member this.Lookup name =
        let result =
            match variableProvider with
            | None -> this.CompiledExpressions.[name].Invoke(this, functionProvider)
            | Some v ->
                match this.CompiledExpressions.TryGetValue name with
                | (true, f) -> f.Invoke(this, functionProvider)
                | (false, f) -> Helpers.castToDouble(v.Lookup (name, this))
        Number(result)
    member this.LookupRange name lower upper =
        match variableProvider with
        | None ->
            let value = this.CompiledExpressions.[name].Invoke(this, functionProvider)
            match (lower, upper) with
            | (Number a, Number b) -> Array.init (int(b - a) + 1) (fun x -> Number(value))
            | _ -> invalidArg "range" "Numeric range expected."
        | Some v ->
            match this.CompiledExpressions.TryGetValue name with
            | (true, f) ->
                let value = f.Invoke(this, functionProvider)
                match (lower, upper) with
                | (Number a, Number b) -> Array.init (int(b - a) + 1) (fun x -> Number(value))
                | _ -> invalidArg "range" "Numeric range expected."
            | (false, f) -> v.LookupRange (name, lower, upper, this)
    member this.LookupIndex name index =
        this.Lookup name

    interface IVariableProvider with 
        member this.IsDefined (name) = this.IsDefined name
        member this.IsDefined (name, sender) = this.IsDefined name
        member this.Lookup (name) = this.Lookup name
        member this.Lookup (name, sender) = this.Lookup name
        member this.LookupRange (name, lower, upper) = this.LookupRange name lower upper
        member this.LookupRange (name, lower, upper, sender) = this.LookupRange name lower upper
        member this.LookupIndex (name, index) = this.LookupIndex name index
        member this.LookupIndex (name, index, sender) = this.LookupIndex name index

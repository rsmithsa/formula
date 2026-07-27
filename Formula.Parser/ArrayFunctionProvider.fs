//-----------------------------------------------------------------------
// <copyright file="ArrayFunctionProvider.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open Microsoft.FSharp.Linq.NullableOperators
open System.Runtime.InteropServices

open Formula.Parser
open Formula.Parser.Ast

type FilterFunction() =
    
    let filter op a b c =
        match op with
        | ">" -> if b > a then c b
        | "<" -> if b < a then c b
        | ">=" -> if b >= a then c b
        | "<=" -> if b <= a then c b
        | "=" -> if b = a then c b
        | "<>" -> if b <> a then c b
        | _ -> c b
    
    member this.Name =
        "FILTER"

    member this.IsNonDeterministic = false

    member this.Execute (input: value[]) =
        let values = Helpers.flattenValues input
        let operator =
            match values.[0] with
            | Text t -> t
            | _ -> ""

        let comparison = values.[1]

        let r = ResizeArray(values.Length - 2)
        for v in values[2..] do
            filter operator comparison v r.Add

        ValueArray(r.ToArray())

    member this.Validate (input: value[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "FILTER expects at least two arguments."
            false
        | false ->
            let flat = Helpers.flattenValues input
            match flat.Length with
            | 0 | 1 ->
                message <- "FILTER expects at least two arguments."
                false
            | _ -> true

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.IsNonDeterministic = this.IsNonDeterministic
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type VMultFunction() =
    member this.Name =
        "VMULT"

    member this.IsNonDeterministic = false

    member this.Execute (input: value[]) =
        let values = Helpers.castToNullableDoubles input
        let scalar = values.[0]

        let result = 
            values 
            |> Seq.skip 1 
            |> Seq.map (
                fun x ->
                    let n = scalar ?*? x
                    if n.HasValue then Number(n.GetValueOrDefault()) else Nothing
            )
            |> Seq.toArray

        ValueArray(result)

    member this.Validate (input: value[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "VMULT expects at least two arguments."
            false
        | false ->
            let flat = Helpers.flattenValues input
            match flat.Length with
            | 0 | 1 ->
                message <- "VMULT expects at least two arguments."
                false
            | _ -> true

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.IsNonDeterministic = this.IsNonDeterministic
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type ArrayFunctionProvider() =

    static let instance = ArrayFunctionProvider()

    let knownFunctions: Map<string, IFunctionImplementation> =
        Map.empty.
            Add("FILTER", FilterFunction() :> IFunctionImplementation).
            Add("VMULT", VMultFunction())

    static member Instance = instance

    member this.KnownFunctions =
        knownFunctions |> Map.toSeq |> Seq.map fst
    member this.IsDefined name = 
        knownFunctions.ContainsKey name
    member this.Lookup name =
        knownFunctions.[name]

    interface IFunctionProvider with
        member this.KnownFunctions = this.KnownFunctions
        member this.IsDefined name = this.IsDefined name
        member this.Lookup name = this.Lookup name

//-----------------------------------------------------------------------
// <copyright file="DefaultFunctions.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open System.Runtime.InteropServices

open Formula.Parser

type SqrtFunction() =
    member this.Name =
        "SQRT"
    member this.Execute (input: float[]) =
        sqrt input.[0]
    member this.Validate (input: float[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "SQRT expects one argument."
            false
        | false ->
            match input.Length with
            | 1 -> true
            | _ ->
                message <- "SQRT expects one argument."
                false

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type PiFunction() =
    member this.Name =
        "PI"
    member this.Execute (input: float[]) =
        System.Math.PI
    member this.Validate (input: float[], [<Out>]message: string byref) =
        match isNull input with
        | true -> true
        | false ->
            match input.Length with
            | 0 -> true
            | _ ->
                message <- "PI expects no arguments."
                false

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type PowFunction() =
    member this.Name =
        "POW"
    member this.Execute (input: float[]) =
        input.[0] ** input.[1]
    member this.Validate (input: float[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "POW expects two arguments."
            false
        | false ->
            match input.Length with
            | 2 -> true
            | _ ->
                message <- "POW expects two arguments."
                false

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type ModFunction() =
    member this.Name =
        "MOD"
    member this.Execute (input: float[]) =
        input.[0] % input.[1]
    member this.Validate (input: float[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "MOD expects two arguments."
            false
        | false ->
            match input.Length with
            | 2 -> true
            | _ ->
                message <- "MOD expects two arguments."
                false

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type CountFunction() =
    member this.Name =
        "COUNT"
    member this.Execute (input: float[]) =
        match isNull input with
        | true -> 0.0
        | false -> float(input.Length)
    member this.Validate (input: float[], [<Out>]message: string byref) =
        true

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type DefaultFunctionProvider() =

    static let instance = DefaultFunctionProvider()

    let knownFunctions: Map<string, IFunctionImplementation> =
        Map.empty.
            Add("SQRT", SqrtFunction() :> IFunctionImplementation).
            Add("PI", PiFunction()).
            Add("POW", PowFunction()).
            Add("MOD", ModFunction()).
            Add("COUNT", CountFunction())

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

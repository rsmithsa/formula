//-----------------------------------------------------------------------
// <copyright file="DefaultFunctions.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open System.Runtime.InteropServices

open Formula.Parser
open Formula.Parser.Ast

type SqrtFunction() =
    member this.Name =
        "SQRT"

    member this.Execute (input: value[]) =
        let values = Helpers.asDoubles(input)
        sqrt values.[0]

    member this.Validate (input: value[], [<Out>]message: string byref) =
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

    member this.Execute (input: value[]) =
        System.Math.PI

    member this.Validate (input: value[], [<Out>]message: string byref) =
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

    member this.Execute (input: value[]) =
        let values = Helpers.asDoubles(input)
        values.[0] ** values.[1]

    member this.Validate (input: value[], [<Out>]message: string byref) =
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

    member this.Execute (input: value[]) =
        let values = Helpers.asDoubles(input)
        values.[0] % values.[1]

    member this.Validate (input: value[], [<Out>]message: string byref) =
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

    member this.Execute (input: value[]) =
        match isNull input with
        | true -> 0.0
        | false -> float(input.Length)

    member this.Validate (input: value[], [<Out>]message: string byref) =
        true

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type SumFunction() =
    member this.Name =
        "SUM"

    member this.Execute (input: value[]) =
        match isNull input with
        | true -> 0.0
        | false ->
            let values = Helpers.asDoubles(input)
            Array.sum values

    member this.Validate (input: value[], [<Out>]message: string byref) =
        true

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type AvgFunction() =
    member this.Name =
        "AVG"

    member this.Execute (input: value[]) =
        let values = Helpers.asDoubles(input)
        Array.average values

    member this.Validate (input: value[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "AVG expects at least one argument."
            false
        | false ->
            match input.Length with
            | 0 ->
                message <- "AVG expects at least one argument."
                false
            | _ -> true

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type FirstFunction() =
    member this.Name =
        "FIRST"

    member this.Execute (input: value[]) =
        Helpers.castToDouble input.[0]

    member this.Validate (input: value[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "FIRST expects at least one argument."
            false
        | false ->
            match input.Length with
            | 0 ->
                message <- "FIRST expects at least one argument."
                false
            | _ -> true

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type LastFunction() =
    member this.Name =
        "LAST"

    member this.Execute (input: value[]) =
        Helpers.castToDouble input.[input.Length - 1]

    member this.Validate (input: value[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "LAST expects at least one argument."
            false
        | false ->
            match input.Length with
            | 0 ->
                message <- "LAST expects at least one argument."
                false
            | _ -> true

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
            Add("COUNT", CountFunction()).
            Add("SUM", SumFunction()).
            Add("AVG", AvgFunction()).
            Add("FIRST", FirstFunction()).
            Add("LAST", LastFunction())

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

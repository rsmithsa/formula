//-----------------------------------------------------------------------
// <copyright file="DefaultFunctions.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open Formula.Parser

type SqrtFunction() =
    interface IFunctionImplementation with
        member this.Name =
            "SQRT"
        member this.Execute input =
            sqrt input.[0]
        member this.Validate (input, message) =
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

type PiFunction() =
    interface IFunctionImplementation with
        member this.Name =
            "PI"
        member this.Execute input =
            System.Math.PI
        member this.Validate (input, message) =
            match isNull input with
            | true -> true
            | false ->
                match input.Length with
                | 0 -> true
                | _ ->
                    message <- "PI expects no arguments."
                    false

type PowFunction() =
    interface IFunctionImplementation with
        member this.Name =
            "POW"
        member this.Execute input =
            input.[0] ** input.[1]
        member this.Validate (input, message) =
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

type ModFunction() =
    interface IFunctionImplementation with
        member this.Name =
            "MOD"
        member this.Execute input =
            input.[0] % input.[1]
        member this.Validate (input, message) =
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

type CountFunction() =
    interface IFunctionImplementation with
        member this.Name =
            "COUNT"
        member this.Execute input =
            match isNull input with
            | true -> 0.0
            | false -> float(input.Length)
        member this.Validate (input, message) =
            true

type DefaultFunctionProvider() =

    static let instance = DefaultFunctionProvider() :> IFunctionProvider

    let knownFunctions: Map<string, IFunctionImplementation> =
        Map.empty.
            Add("SQRT", SqrtFunction() :> IFunctionImplementation).
            Add("PI", PiFunction() :> IFunctionImplementation).
            Add("POW", PowFunction() :> IFunctionImplementation).
            Add("MOD", ModFunction() :> IFunctionImplementation).
            Add("COUNT", CountFunction() :> IFunctionImplementation)

    static member Instance = instance

    interface IFunctionProvider with
        member this.KnownFunctions =
            knownFunctions |> Map.toSeq |> Seq.map fst
        member this.IsDefined name = 
            knownFunctions.ContainsKey name
        member this.Lookup name =
            knownFunctions.[name]

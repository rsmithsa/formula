//-----------------------------------------------------------------------
// <copyright file="FinancialFunctions.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open System.Runtime.InteropServices

open Formula.Parser

type DdbFunction() =
    let rec depreciate balance salvage rate count =
        let depreciation = balance * rate
        match balance - depreciation - salvage >= 0.0 with
        | false ->
            match count with
            | 1 -> balance - salvage
            | _ -> 0.0
        | true ->
            match count with
            | 1 -> depreciation
            | _ -> depreciate (balance - depreciation) salvage rate (count - 1)

    member this.Name =
        "DDB"

    member this.Execute (input: float[]) =
        let factor =
            match input.Length with
            | 5 -> input.[4]
            | _ -> 2.0

        let rate = 1.0 / input.[2] * factor;
        depreciate input.[0] input.[1] rate (int input.[3])

    member this.Validate (input: float[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "DDB expects four or five arguments."
            false
        | false ->
            match input.Length with
            | 4 | 5 -> true
            | _ ->
                message <- "DDB expects four or five arguments."
                false

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type FvFunction() =
    member this.Name =
        "FV"

    member this.Execute (input: float[]) =
        let pv =
            match input.Length with
            | 4 | 5 -> (1.0 + input.[0]) ** input.[1] * input.[3]
            | _ -> 0.0

        let annuityDue =
            match input.Length with
            | 5 -> input.[4]
            | _ -> 0.0

        let fv = input.[2] * ((1.0 + input.[0]) ** input.[1] - 1.0) / input.[0]

        match Helpers.castToBool annuityDue with
        | true -> -(pv + (1.0 + input.[0]) * fv)
        | false -> -(pv + fv)

    member this.Validate (input: float[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "FV expects three, four or five arguments."
            false
        | false ->
            match input.Length with
            | 3 | 4 | 5 -> true
            | _ ->
                message <- "FV expects three, four or five arguments."
                false

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type NpvFunction() =
    member this.Name =
        "NPV"

    member this.Execute (input: float[]) =
        input.[1..]
        |> Array.fold (fun (rate, npv) flow -> (rate * (1.0 + input.[0]), npv + (flow / rate))) (1.0 + input.[0], 0.0)
        |> snd

    member this.Validate (input: float[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "NPV expects at least two arguments."
            false
        | false ->
            match input.Length with
            | 0 | 1 ->
                message <- "NPV expects at least two arguments."
                false
            | _ -> true

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type PvFunction() =
    member this.Name =
        "PV"

    member this.Execute (input: float[]) =
        let fv =
            match input.Length with
            | 4 | 5 -> (1.0 + input.[0]) ** -input.[1] * input.[3]
            | _ -> 0.0

        let annuityDue =
            match input.Length with
            | 5 -> input.[4]
            | _ -> 0.0

        match Helpers.castToBool annuityDue with
        | true -> -(fv + input.[2] + input.[2] * (1.0 - (1.0 + input.[0]) ** -(input.[1] - 1.0)) / input.[0])
        | false -> -(fv + input.[2] * (1.0 - (1.0 + input.[0]) ** -input.[1]) / input.[0])

    member this.Validate (input: float[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "PV expects three, four or five arguments."
            false
        | false ->
            match input.Length with
            | 3 | 4 | 5 -> true
            | _ ->
                message <- "PV expects three, four or five arguments."
                false

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type SlnFunction() =
    member this.Name =
        "SLN"

    member this.Execute (input: float[]) =
        (input.[0] - input.[1]) / input.[2]

    member this.Validate (input: float[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "SLN expects three arguments."
            false
        | false ->
            match input.Length with
            | 3 -> true
            | _ ->
                message <- "SLN expects three arguments."
                false

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type FinancialFunctionProvider() =

    static let instance = FinancialFunctionProvider()

    let knownFunctions: Map<string, IFunctionImplementation> =
        Map.empty.
            Add("DDB", DdbFunction() :> IFunctionImplementation).
            Add("FV", FvFunction()).
            Add("NPV", NpvFunction()).
            Add("PV", PvFunction()).
            Add("SLN", SlnFunction())

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

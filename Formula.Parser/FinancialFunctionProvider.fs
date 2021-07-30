//-----------------------------------------------------------------------
// <copyright file="FinancialFunctionProvider.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open System.Runtime.InteropServices

open Formula.Parser
open Formula.Parser.Ast

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

    member this.Execute (input: value[]) =
        let values = Helpers.asDoubles(input)

        let factor =
            match values.Length with
            | 5 -> values.[4]
            | _ -> 2.0

        let rate = 1.0 / values.[2] * factor;
        Number(depreciate values.[0] values.[1] rate (int values.[3]))

    member this.Validate (input: value[], [<Out>]message: string byref) =
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

    member this.Execute (input: value[]) =
        let values = Helpers.asDoubles(input.[0..3])

        let pv =
            match values.Length with
            | 4 | 5 -> (1.0 + values.[0]) ** values.[1] * values.[3]
            | _ -> 0.0

        let annuityDue =
            match input.Length with
            | 5 -> input.[4]
            | _ -> Boolean(false)

        let fv = values.[2] * ((1.0 + values.[0]) ** values.[1] - 1.0) / values.[0]

        match Helpers.castToBool annuityDue with
        | true -> -(pv + (1.0 + values.[0]) * fv)
        | false -> -(pv + fv)
        |> Number

    member this.Validate (input: value[], [<Out>]message: string byref) =
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

    member this.Execute (input: value[]) =
        let values = Helpers.asDoubles(input)

        values.[1..]
        |> Array.fold (fun (rate, npv) flow -> (rate * (1.0 + values.[0]), npv + (flow / rate))) (1.0 + values.[0], 0.0)
        |> snd
        |> Number

    member this.Validate (input: value[], [<Out>]message: string byref) =
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

type IrrFunction() =
    let npvImplementation = new NpvFunction();
    member this.Name =
        "IRR"

    member this.Execute (input: value[]) =
        let values = Helpers.asDoubles(input)

        let delta = 0.01
        let guess = values.[0]

        let workingInput = Array.copy input
        let fx irr =
            workingInput.[0] <- Number(irr)
            (npvImplementation.Execute(workingInput)).NumberValue
        
        Newton.newtonsMethod fx guess delta
        |> Number

    member this.Validate (input: value[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "IRR expects at least three arguments."
            false
        | false ->
            match input.Length with
            | 0 | 1 | 2 ->
                message <- "IRR expects at least three arguments."
                false
            | _ -> true

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type MirrFunction() =
    let npvImplementation = new NpvFunction();
    member this.Name =
        "MIRR"

    member this.Execute (input: value[]) =
        let values = Helpers.asDoubles(input)

        let financeRate = values.[0]
        let reinvestmentRate = values.[1]

        let negativeFlows =
            values.[2..]
            |> Seq.where(fun x -> x < 0.0)
            |> Seq.toArray

        let positiveFlows =
            values.[2..]
            |> Seq.where(fun x -> x >= 0.0)
            |> Seq.rev
            |> Seq.toArray

        let negative = 
            negativeFlows
            |> Array.fold (fun (rate, npv) flow -> (rate * (1.0 + financeRate), npv + (flow / rate))) (1.0, 0.0)
            |> snd

        let positive = 
            positiveFlows
            |> Array.fold (fun (rate, npv) flow -> (rate * (1.0 + reinvestmentRate), npv + (flow * rate))) (1.0, 0.0)
            |> snd

        let root = (1.0 / ((float)input.Length - 3.0))
        Number(((positive / -negative) ** root) - 1.0)
        
    member this.Validate (input: value[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "MIRR expects at least four arguments."
            false
        | false ->
            match input.Length with
            | 0 | 1 | 2 | 3 ->
                message <- "MIRR expects at least four arguments."
                false
            | _ -> true

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type NperFunction() =
    member this.Name =
        "NPER"

    member this.Execute (input: value[]) =
        let values = Helpers.asDoubles(input.[0..3])

        let rate = values.[0]
        let pmt = values.[1]
        let pv = values.[2]
        let fv =
            match values.Length with
            | 4 | 5 -> values.[3]
            | _ -> 0.0

        let annuityDue =
            match input.Length with
            | 5 -> input.[4]
            | _ -> Boolean(false)

        match values.[0] with
        | 0.0 -> (-fv - pv) / pmt
        | _ ->
            match Helpers.castToBool annuityDue with
            | true ->
                log((-rate * fv + pmt * (1.0 + rate)) / (rate * pv + pmt * (1.0 + rate))) / log(1.0 + rate)        
            | false ->
                log((-rate * fv + pmt) / (rate * pv + pmt)) / log(1.0 + rate)
        |> Number

    member this.Validate (input: value[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "NPER expects three, four or five arguments."
            false
        | false ->
            match input.Length with
            | 3 | 4 | 5 -> true
            | _ ->
                message <- "NPER expects three, four or five arguments."
                false

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type PmtFunction() =
    member this.Name =
        "PMT"

    member this.Execute (input: value[]) =
        let values = Helpers.asDoubles(input.[0..3])

        let fv =
            match values.Length with
            | 4 | 5 -> values.[3]
            | _ -> 0.0

        let annuityDue =
            match input.Length with
            | 5 -> input.[4]
            | _ -> Boolean(false)

        let pvPmt = values.[0] * values.[2] / (1.0 - (1.0 + values.[0]) ** -values.[1])
        let fvPmt = values.[0] * fv / ((1.0 + values.[0]) ** values.[1] - 1.0)

        match Helpers.castToBool annuityDue with
        | true ->
            -(pvPmt * 1.0 / (1.0 + values.[0]) + fvPmt * 1.0 / (1.0 + values.[0]))
        | false ->
            -(pvPmt + fvPmt)
        |> Number

    member this.Validate (input: value[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "PMT expects three, four or five arguments."
            false
        | false ->
            match input.Length with
            | 3 | 4 | 5 -> true
            | _ ->
                message <- "PMT expects three, four or five arguments."
                false

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type IpmtFunction() =
    let pmtImplementation = new PmtFunction();
    let fvImplementation = new FvFunction();
    member this.Name =
        "IPMT"

    member this.Execute (input: value[]) =
        let values = Helpers.asDoubles(input.[0..4])

        let annuityDue =
            match input.Length with
            | 6 -> input.[5]
            | _ -> Boolean(false)
        let pmt = pmtImplementation.Execute(Array.append input.[0..0] input.[2..])
        let fv = fvImplementation.Execute(List.toArray [input.[0]; Number(values.[1] - 1.0); pmt; input.[3]; annuityDue])

        match Helpers.castToBool annuityDue with
        | true ->
            fv.NumberValue * values.[0] / (1.0 + values.[0])
        | false ->
            fv.NumberValue * values.[0]
        |> Number

    member this.Validate (input: value[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "IPMT expects four, five or six arguments."
            false
        | false ->
            match input.Length with
            | 4 | 5 | 6 -> true
            | _ ->
                message <- "IPMT expects four, five or six arguments."
                false

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type PpmtFunction() =
    let pmtImplementation = new PmtFunction();
    let ipmtImplementation = new IpmtFunction();
    member this.Name =
        "PPMT"

    member this.Execute (input: value[]) =
        let pmt = pmtImplementation.Execute(Array.append input.[0..0] input.[2..])
        let ipmt = ipmtImplementation.Execute(input)
        Number(pmt.NumberValue - ipmt.NumberValue)

    member this.Validate (input: value[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "PPMT expects four, five or six arguments."
            false
        | false ->
            match input.Length with
            | 4 | 5 | 6 -> true
            | _ ->
                message <- "PPMT expects four, five or six arguments."
                false

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type PvFunction() =
    member this.Name =
        "PV"

    member this.Execute (input: value[]) =
        let values = Helpers.asDoubles(input.[0..3])

        let fv =
            match values.Length with
            | 4 | 5 -> (1.0 + values.[0]) ** -values.[1] * values.[3]
            | _ -> 0.0

        let annuityDue =
            match input.Length with
            | 5 -> input.[4]
            | _ -> Boolean(false)

        let pv = values.[2] * (1.0 - (1.0 + values.[0]) ** -values.[1]) / values.[0]

        match Helpers.castToBool annuityDue with
        | true -> -(fv + (1.0 + values.[0]) * pv)
        | false -> -(fv + pv)
        |> Number

    member this.Validate (input: value[], [<Out>]message: string byref) =
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

type RateFunction() =
    member this.Name =
        "RATE"

    member this.Execute (input: value[]) =
        let values = Helpers.asDoubles(Array.append input.[0..3] input.[5..5])

        let nper = values.[0]
        let pmt = values.[1]
        let pv = values.[2]
        let fv =
            match values.Length with
            | 4 | 5 | 6 -> values.[3]
            | _ -> 0.0

        let annuityDue =
            match input.Length with
            | 5 | 6 -> input.[4]
            | _ -> Boolean(false)

        let guess =
            match values.Length with
            | 5 -> values.[4]
            | _ -> 0.1

        let delta = 0.01

        match Helpers.castToBool annuityDue with
        | true ->
            let fx rate = fv + pv * (1.0 + rate) ** nper + pmt * (1.0 + rate) / rate * ((1.0 + rate) ** nper - 1.0)
            Newton.newtonsMethod fx guess delta
        | false ->
            let fx rate = fv + pv * (1.0 + rate) ** nper + pmt / rate * ((1.0 + rate) ** nper - 1.0)
            Newton.newtonsMethod fx guess delta
        |> Number

    member this.Validate (input: value[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "RATE expects three, four, five or six arguments."
            false
        | false ->
            match input.Length with
            | 3 | 4 | 5 | 6 -> true
            | _ ->
                message <- "RATE expects three, four, five or six arguments."
                false

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type SlnFunction() =
    member this.Name =
        "SLN"

    member this.Execute (input: value[]) =
        let values = Helpers.asDoubles(input)

        Number((values.[0] - values.[1]) / values.[2])

    member this.Validate (input: value[], [<Out>]message: string byref) =
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

type SydFunction() =
    member this.Name =
        "SYD"

    member this.Execute (input: value[]) =
        let values = Helpers.asDoubles(input)

        Number(((values.[0] - values.[1]) * (values.[2] - values.[3] + 1.0) * 2.0) / (values.[2] * (values.[2] + 1.0)))

    member this.Validate (input: value[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "SYD expects four arguments."
            false
        | false ->
            match input.Length with
            | 4 -> true
            | _ ->
                message <- "SYD expects four arguments."
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
            Add("IPMT", IpmtFunction()).
            Add("IRR", IrrFunction()).
            Add("MIRR", MirrFunction()).
            Add("NPER", NperFunction()).
            Add("NPV", NpvFunction()).
            Add("PMT", PmtFunction()).
            Add("PPMT", PpmtFunction()).
            Add("PV", PvFunction()).
            Add("RATE", RateFunction()).
            Add("SLN", SlnFunction()).
            Add("SYD", SydFunction())

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

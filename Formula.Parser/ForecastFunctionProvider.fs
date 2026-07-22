//-----------------------------------------------------------------------
// <copyright file="ForecastFunctionProvider.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open System.Runtime.InteropServices

open Formula.Parser
open Formula.Parser.Ast

type SesFunction() =
    member this.Name =
        "SES"

    member this.IsNonDeterministic = false

    member this.Execute (input: value[]) =
        let values = Helpers.castToDoubles input
        let alpha = (values[0..0] |> Array.choose id).[0]
        let data = values.[1..] |> Array.choose id

        if data.Length < 2 then Nothing
        else
            let mutable s = data.[0]
            for i in 1 .. data.Length - 1 do
                s <- alpha * data.[i] + (1.0 - alpha) * s
            Number(s)

    member this.Validate (input: value[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "SES expects at least three arguments."
            false
        | false ->
            let flat = Helpers.flattenValues input
            match flat.Length with
            | 0 | 1 | 2 ->
                message <- "SES expects at least three arguments."
                false
            | _ -> true

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.IsNonDeterministic = this.IsNonDeterministic
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type HoltFunction() =
    member this.Name =
        "HOLT"

    member this.IsNonDeterministic = false

    member this.Execute (input: value[]) =
        let values = Helpers.castToDoubles input
        let params' = values.[0..1] |> Array.choose id
        let alpha = params'.[0]
        let beta = params'.[1]
        let data = values.[2..] |> Array.choose id

        if data.Length < 2 then Nothing
        else
            let mutable level = data.[0]
            let mutable trend = data.[1] - data.[0]

            for i in 1 .. data.Length - 1 do
                let prevLevel = level
                level <- alpha * data.[i] + (1.0 - alpha) * (prevLevel + trend)
                trend <- beta * (level - prevLevel) + (1.0 - beta) * trend

            Number(level + trend)

    member this.Validate (input: value[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "HOLT expects at least four arguments."
            false
        | false ->
            let flat = Helpers.flattenValues input
            match flat.Length with
            | 0 | 1 | 2 | 3 ->
                message <- "HOLT expects at least four arguments."
                false
            | _ -> true

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.IsNonDeterministic = this.IsNonDeterministic
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type HwaFunction() =
    member this.Name =
        "HWA"

    member this.IsNonDeterministic = false

    member this.Execute (input: value[]) =
        let values = Helpers.castToDoubles input
        let params' = values.[0..3] |> Array.choose id
        let alpha = params'.[0]
        let beta = params'.[1]
        let gamma = params'.[2]
        let period = int params'.[3]
        let data = values.[4..] |> Array.choose id

        let n = data.Length
        if n < period * 2 then Nothing
        else
            let avg = (data.[0 .. period - 1] |> Array.sum) / float period

            let seasonal = Array.zeroCreate n
            for i in 0 .. period - 1 do
                seasonal.[i] <- data.[i] - avg

            let mutable level = avg
            let mutable trend = (data.[period] - data.[0]) / float period

            for i in period .. n - 1 do
                let prevLevel = level
                level <- alpha * (data.[i] - seasonal.[i - period]) + (1.0 - alpha) * (prevLevel + trend)
                trend <- beta * (level - prevLevel) + (1.0 - beta) * trend
                seasonal.[i] <- gamma * (data.[i] - level) + (1.0 - gamma) * seasonal.[i - period]

            Number(level + trend + seasonal.[n - period])

    member this.Validate (input: value[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "HWA expects at least seven arguments."
            false
        | false ->
            let flat = Helpers.flattenValues input
            match flat.Length with
            | l when l < 7 ->
                message <- "HWA expects at least seven arguments."
                false
            | _ -> true

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.IsNonDeterministic = this.IsNonDeterministic
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type HwmFunction() =
    member this.Name =
        "HWM"

    member this.IsNonDeterministic = false

    member this.Execute (input: value[]) =
        let values = Helpers.castToDoubles input
        let params' = values.[0..3] |> Array.choose id
        let alpha = params'.[0]
        let beta = params'.[1]
        let gamma = params'.[2]
        let period = int params'.[3]
        let data = values.[4..] |> Array.choose id

        let n = data.Length
        if n < period * 2 then Nothing
        else
            let avg = (data.[0 .. period - 1] |> Array.sum) / float period

            let seasonal = Array.zeroCreate n
            for i in 0 .. period - 1 do
                seasonal.[i] <- data.[i] / avg

            let mutable level = avg
            let mutable trend = (data.[period] - data.[0]) / float period

            for i in period .. n - 1 do
                let prevLevel = level
                level <- alpha * (data.[i] / seasonal.[i - period]) + (1.0 - alpha) * (prevLevel + trend)
                trend <- beta * (level - prevLevel) + (1.0 - beta) * trend
                seasonal.[i] <- gamma * (data.[i] / level) + (1.0 - gamma) * seasonal.[i - period]

            Number((level + trend) * seasonal.[n - period])

    member this.Validate (input: value[], [<Out>]message: string byref) =
        match isNull input with
        | true ->
            message <- "HWM expects at least seven arguments."
            false
        | false ->
            let flat = Helpers.flattenValues input
            match flat.Length with
            | l when l < 7 ->
                message <- "HWM expects at least seven arguments."
                false
            | _ -> true

    interface IFunctionImplementation with
        member this.Name = this.Name
        member this.IsNonDeterministic = this.IsNonDeterministic
        member this.Execute input = this.Execute input
        member this.Validate (input, message) = this.Validate (input, &message)

type ForecastFunctionProvider() =

    static let instance = ForecastFunctionProvider()

    let ses = SesFunction() :> IFunctionImplementation
    let holt = HoltFunction() :> IFunctionImplementation
    let hwa = HwaFunction() :> IFunctionImplementation
    let hwm = HwmFunction() :> IFunctionImplementation

    let knownFunctions: Map<string, IFunctionImplementation> =
        Map.empty.
            Add("SES", ses).
            Add("FORECAST", ses).
            Add("HOLT", holt).
            Add("DES", holt).
            Add("HWA", hwa).
            Add("HOLTWINTERS", hwa).
            Add("HWM", hwm).
            Add("HOLTWINTERSM", hwm)

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

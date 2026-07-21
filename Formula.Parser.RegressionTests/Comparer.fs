//-----------------------------------------------------------------------
// <copyright file="Comparer.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

/// Compares two snapshots and reports where results differ. Used both to assert
/// the current build against a committed baseline and to diff two stored versions.
module Formula.Parser.RegressionTests.Comparer

open System

open Formula.Parser.RegressionTests.Model

/// A stable key identifying one entry across snapshots.
type private Key = string * string

let private keyOf (entry: SnapshotEntry) : Key = (entry.CaseId, entry.Engine)

/// Are two computed doubles equal under the given options? NaN equals NaN and
/// equal infinities match; `Epsilon = 0.0` means exact.
let private numbersEqual (opts: ComparisonOptions) (a: float) (b: float) =
    if Double.IsNaN a && Double.IsNaN b then true
    elif a = b then true // handles equal infinities and exact equality
    elif Double.IsFinite a && Double.IsFinite b && opts.Epsilon > 0.0 then
        let diff = abs (a - b)
        if opts.Relative then diff <= opts.Epsilon * max (abs a) (abs b)
        else diff <= opts.Epsilon
    else false

/// Are two results equivalent? Differing kinds (value/nothing/error) never match.
let resultsEqual (opts: ComparisonOptions) (a: ResultValue) (b: ResultValue) =
    match a, b with
    | NoValue, NoValue -> true
    | Failed x, Failed y -> x = y
    | Computed x, Computed y -> numbersEqual opts x y
    | _ -> false

/// Compare a baseline snapshot against the current one. `optionsFor` yields the
/// comparison options for a given case id (enabling per-case tolerance). Returns
/// every differing, added, or removed entry, ordered for readable reporting.
let compare (optionsFor: string -> ComparisonOptions) (baseline: Snapshot) (current: Snapshot) : Mismatch list =
    let baseMap = baseline.Entries |> List.map (fun e -> keyOf e, e.Result) |> Map.ofList
    let currMap = current.Entries |> List.map (fun e -> keyOf e, e.Result) |> Map.ofList

    let keys =
        Set.union (baseMap |> Map.toSeq |> Seq.map fst |> Set.ofSeq)
                  (currMap |> Map.toSeq |> Seq.map fst |> Set.ofSeq)

    [ for key in keys do
        let (caseId, engine) = key
        let baseResult = Map.tryFind key baseMap
        let currResult = Map.tryFind key currMap
        let differs =
            match baseResult, currResult with
            | Some b, Some c -> not (resultsEqual (optionsFor caseId) b c)
            | _ -> true // added or removed
        if differs then
            { CaseId = caseId
              Engine = engine
              Baseline = baseResult
              Current = currResult } ]
    |> List.sortBy (fun m -> m.CaseId, m.Engine)

/// Render a single result for display.
let formatResult (result: ResultValue) =
    match result with
    | NoValue -> "Nothing"
    | Computed v -> string v
    | Failed msg -> sprintf "Error(%s)" msg

let private describe (result: ResultValue option) =
    match result with
    | None -> "<absent>"
    | Some r -> formatResult r

/// Render mismatches as a human-readable, one-line-per-diff report.
let report (mismatches: Mismatch list) : string =
    mismatches
    |> List.map (fun m ->
        sprintf "%s | %s : %s -> %s"
            m.CaseId m.Engine (describe m.Baseline) (describe m.Current))
    |> String.concat Environment.NewLine

//-----------------------------------------------------------------------
// <copyright file="Corpus.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

/// Loads formula cases from JSON and builds the named variable/function providers
/// they reference.
module Formula.Parser.RegressionTests.Corpus

open System.IO
open System.Text.Json.Nodes

open Formula.Parser
open Formula.Parser.Integration

open Formula.Parser.RegressionTests.Model

/// Build the variable provider named by a case. Unknown names fail loudly.
let buildVariableProvider (name: string) (variables: Map<string, float>) : IVariableProvider =
    match name with
    | "map" -> MapVariableProvider(variables) :> IVariableProvider
    | "empty" -> MapVariableProvider.Empty :> IVariableProvider
    | other -> failwithf "Unknown variable provider '%s' (expected 'map' or 'empty')." other

/// Build the function provider named by a case. Unknown names fail loudly.
let buildFunctionProvider (name: string) : IFunctionProvider =
    match name with
    | "default" -> DefaultFunctionProvider.Instance :> IFunctionProvider
    | "financial" -> FinancialFunctionProvider.Instance :> IFunctionProvider
    | "forecast" -> ForecastFunctionProvider.Instance :> IFunctionProvider
    | "all" ->
        CompositeFunctionProvider(
            [ DefaultFunctionProvider.Instance :> IFunctionProvider
              FinancialFunctionProvider.Instance :> IFunctionProvider
              ForecastFunctionProvider.Instance :> IFunctionProvider ])
        :> IFunctionProvider
    | other -> failwithf "Unknown function provider '%s' (expected 'default', 'financial', 'forecast' or 'all')." other

let private optString (node: JsonNode) (key: string) =
    match node.[key] with
    | null -> None
    | v -> Some(v.GetValue<string>())

let private parseCase (node: JsonNode) : Case =
    let variables =
        match node.["variables"] with
        | null -> Map.empty
        | v ->
            v.AsObject()
            |> Seq.map (fun kvp -> kvp.Key, kvp.Value.GetValue<float>())
            |> Map.ofSeq
    { Id = node.["id"].GetValue<string>()
      Formula = node.["formula"].GetValue<string>()
      Variables = variables
      FunctionProvider = defaultArg (optString node "functionProvider") "default"
      VariableProvider = defaultArg (optString node "variableProvider") "map"
      Fold =
        match node.["fold"] with
        | null -> false
        | v -> v.GetValue<bool>()
      Tolerance =
        match node.["tolerance"] with
        | null -> None
        | v -> Some(v.GetValue<float>()) }

/// Parse a corpus document (an object with a `cases` array) into cases.
let parseCases (json: string) : Case list =
    let doc = JsonNode.Parse(json)
    doc.["cases"].AsArray()
    |> Seq.map parseCase
    |> List.ofSeq

/// Load and concatenate every `*.json` corpus file under a directory, ordered by
/// file name then case order, and assert case ids are globally unique.
let loadDirectory (directory: string) : Case list =
    let cases =
        Directory.GetFiles(directory, "*.json")
        |> Array.sort
        |> Array.collect (fun path -> File.ReadAllText path |> parseCases |> Array.ofList)
        |> List.ofArray

    let duplicates =
        cases
        |> List.countBy (fun c -> c.Id)
        |> List.filter (fun (_, n) -> n > 1)
        |> List.map fst

    if not (List.isEmpty duplicates) then
        failwithf "Duplicate case ids in corpus: %s" (String.concat ", " duplicates)

    cases

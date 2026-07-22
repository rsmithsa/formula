//-----------------------------------------------------------------------
// <copyright file="Corpus.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

/// Loads formula cases from JSON and builds the named variable/function providers
/// they reference.
module Formula.Parser.RegressionTests.Corpus

open System.IO
open System.Text.Json
open System.Text.Json.Nodes

open Formula.Parser
open Formula.Parser.Ast
open Formula.Parser.Integration

open Formula.Parser.RegressionTests.Model

/// A variable provider over typed `value`s. Mirrors `MapVariableProvider`'s range/index
/// semantics (a range returns `upper - lower + 1` copies of the single stored value; an
/// index returns that value) but supports boolean/text/nothing variables too.
type TypedMapVariableProvider(map: Map<string, value>) =

    member _.IsDefined name = map.ContainsKey name
    member _.Lookup name = map.[name]
    member _.LookupRange (name, lower, upper) =
        match lower, upper with
        | Number a, Number b -> ValueArray(Array.init (int (b - a) + 1) (fun _ -> map.[name]))
        | _ -> invalidArg "range" "Numeric range expected."
    member _.LookupIndex (name, _index) = map.[name]

    interface IVariableProvider with
        member this.IsDefined name = this.IsDefined name
        member this.IsDefined (name, _sender) = this.IsDefined name
        member this.Lookup name = this.Lookup name
        member this.Lookup (name, _sender) = this.Lookup name
        member this.LookupRange (name, lower, upper) = this.LookupRange (name, lower, upper)
        member this.LookupRange (name, lower, upper, _sender) = this.LookupRange (name, lower, upper)
        member this.LookupIndex (name, index) = this.LookupIndex (name, index)
        member this.LookupIndex (name, index, _sender) = this.LookupIndex (name, index)

/// Build the variable provider named by a case. Unknown names fail loudly.
let buildVariableProvider (name: string) (variables: Map<string, value>) : IVariableProvider =
    match name with
    | "map" -> TypedMapVariableProvider(variables) :> IVariableProvider
    | "empty" -> TypedMapVariableProvider(Map.empty) :> IVariableProvider
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

/// Interpret a JSON variable value as a typed `value`. A JSON number/bool/string/null
/// becomes Number/Boolean/Text/Nothing respectively.
let private parseValue (node: JsonNode) : value =
    match node with
    | null -> Nothing
    | _ ->
        match node.GetValueKind() with
        | JsonValueKind.Number -> Number(node.GetValue<float>())
        | JsonValueKind.True -> Boolean true
        | JsonValueKind.False -> Boolean false
        | JsonValueKind.String -> Text(node.GetValue<string>())
        | JsonValueKind.Null -> Nothing
        | kind -> failwithf "Unsupported variable value kind '%O'." kind

let private parseCase (node: JsonNode) : Case =
    let variables =
        match node.["variables"] with
        | null -> Map.empty
        | v ->
            v.AsObject()
            |> Seq.map (fun kvp -> kvp.Key, parseValue kvp.Value)
            |> Map.ofSeq
    { Id = node.["id"].GetValue<string>()
      Formula = node.["formula"].GetValue<string>()
      Variables = variables
      FunctionProvider = defaultArg (optString node "functionProvider") "default"
      VariableProvider = defaultArg (optString node "variableProvider") "map"
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

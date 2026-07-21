//-----------------------------------------------------------------------
// <copyright file="Snapshotter.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

/// Generates a versioned snapshot from a corpus and (de)serialises it to JSON.
/// Uses System.Text.Json.Nodes directly so non-finite doubles and the
/// `ResultValue` union round-trip without extra dependencies.
module Formula.Parser.RegressionTests.Snapshotter

open System
open System.Globalization
open System.Reflection
open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Json.Nodes

open Formula.Parser.RegressionTests.Model

/// The version of the Formula.Parser assembly under test, e.g. "1.4.1".
let libraryVersion () : string =
    let asm = typeof<Formula.Parser.Ast.expr>.Assembly
    let informational =
        asm.GetCustomAttributes(typeof<AssemblyInformationalVersionAttribute>, false)
        |> Array.tryHead
        |> Option.map (fun a -> (a :?> AssemblyInformationalVersionAttribute).InformationalVersion)
    match informational with
    | Some v ->
        // Strip any "+<source-revision>" build metadata suffix.
        match v.IndexOf('+') with
        | i when i >= 0 -> v.Substring(0, i)
        | _ -> v
    | None -> string (asm.GetName().Version)

/// Evaluate every case and build a snapshot stamped with the current library
/// version. Entries are sorted so serialised output (and thus diffs) is stable.
let generate (cases: Case list) : Snapshot =
    let entries =
        cases
        |> List.collect Engines.evaluate
        |> List.sortBy (fun e -> e.CaseId, e.Engine)
    { LibraryVersion = libraryVersion ()
      GeneratedUtc = DateTime.UtcNow.ToString("o", CultureInfo.InvariantCulture)
      Engines = Engines.engineNames
      Entries = entries }

let private writeResult (result: ResultValue) (target: JsonObject) =
    match result with
    | NoValue -> target.["kind"] <- JsonValue.Create("nothing")
    | Failed msg ->
        target.["kind"] <- JsonValue.Create("error")
        target.["error"] <- JsonValue.Create(msg)
    | Computed v ->
        target.["kind"] <- JsonValue.Create("value")
        // Raw JSON cannot express NaN/Infinity, so encode them as string tokens.
        if Double.IsNaN v then target.["value"] <- JsonValue.Create("NaN")
        elif Double.IsPositiveInfinity v then target.["value"] <- JsonValue.Create("Infinity")
        elif Double.IsNegativeInfinity v then target.["value"] <- JsonValue.Create("-Infinity")
        else target.["value"] <- JsonValue.Create(v)

let private readResult (node: JsonNode) : ResultValue =
    match node.["kind"].GetValue<string>() with
    | "nothing" -> NoValue
    | "error" -> Failed(node.["error"].GetValue<string>())
    | "value" ->
        let value = node.["value"]
        match value.GetValueKind() with
        | JsonValueKind.String ->
            match value.GetValue<string>() with
            | "NaN" -> Computed nan
            | "Infinity" -> Computed infinity
            | "-Infinity" -> Computed -infinity
            | s -> Computed(Double.Parse(s, CultureInfo.InvariantCulture))
        | _ -> Computed(value.GetValue<float>())
    | other -> failwithf "Unknown result kind '%s'." other

/// Serialise a snapshot to indented JSON.
let serialize (snapshot: Snapshot) : string =
    let root = JsonObject()
    root.["libraryVersion"] <- JsonValue.Create(snapshot.LibraryVersion)
    root.["generatedUtc"] <- JsonValue.Create(snapshot.GeneratedUtc)

    let engines = JsonArray()
    for name in snapshot.Engines do
        engines.Add(JsonValue.Create(name))
    root.["engines"] <- engines

    let entries = JsonArray()
    for entry in snapshot.Entries do
        let node = JsonObject()
        node.["caseId"] <- JsonValue.Create(entry.CaseId)
        node.["engine"] <- JsonValue.Create(entry.Engine)
        writeResult entry.Result node
        entries.Add(node)
    root.["entries"] <- entries

    // Relaxed encoding so engine names like "Interpreter+ConstantFold" are written
    // with a literal '+' rather than '+', keeping committed diffs readable.
    root.ToJsonString(JsonSerializerOptions(WriteIndented = true, Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping))

/// Parse a snapshot from JSON.
let deserialize (json: string) : Snapshot =
    let root = JsonNode.Parse(json)
    let engines =
        root.["engines"].AsArray()
        |> Seq.map (fun n -> n.GetValue<string>())
        |> List.ofSeq
    let entries =
        root.["entries"].AsArray()
        |> Seq.map (fun n ->
            { CaseId = n.["caseId"].GetValue<string>()
              Engine = n.["engine"].GetValue<string>()
              Result = readResult n })
        |> List.ofSeq
    { LibraryVersion = root.["libraryVersion"].GetValue<string>()
      GeneratedUtc = root.["generatedUtc"].GetValue<string>()
      Engines = engines
      Entries = entries }

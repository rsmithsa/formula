//-----------------------------------------------------------------------
// <copyright file="RegressionTests.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.RegressionTests

open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

open Formula.Parser.RegressionTests.Model
open Formula.Parser.RegressionTests

/// Regression harness entry points.
///
/// Normal run: `RegressionSnapshot` recomputes every case through every engine and
/// asserts equality against the committed baseline, failing with a per-entry diff.
///
/// Regenerate: set `FORMULA_REGEN_BASELINE=1` and `RegressionSnapshot` instead writes
/// the fresh snapshot to the source tree (rolling baseline + per-version archive).
///
/// Version diff: set `FORMULA_COMPARE_A`/`FORMULA_COMPARE_B` to two snapshot files and
/// run `CompareVersions` to print the differences between any two stored versions.
[<TestClass>]
type RegressionTests() =

    static let regenEnabled =
        match Environment.GetEnvironmentVariable("FORMULA_REGEN_BASELINE") with
        | "1"
        | "true"
        | "TRUE" -> true
        | _ -> false

    static let baseDir = AppContext.BaseDirectory
    static let casesDir = Path.Combine(baseDir, "Cases")
    static let baselinePath = Path.Combine(baseDir, "Baselines", "baseline.json")

    // Walk up from the copied output directory to the project folder (the one
    // containing the fsproj) so regeneration writes back to the source tree.
    static let projectDir =
        let rec walk (dir: DirectoryInfo) =
            if isNull (box dir) then None
            elif File.Exists(Path.Combine(dir.FullName, "Formula.Parser.RegressionTests.fsproj")) then Some dir.FullName
            else walk dir.Parent
        walk (DirectoryInfo(baseDir))

    /// Comparison options per case id: an absolute tolerance if the case sets one,
    /// otherwise exact equality.
    let optionsFor (cases: Case list) =
        let tolerances =
            cases
            |> List.choose (fun c -> c.Tolerance |> Option.map (fun t -> c.Id, ComparisonOptions.absolute t))
            |> Map.ofList
        fun caseId -> Map.tryFind caseId tolerances |> Option.defaultValue ComparisonOptions.exact

    [<TestMethod>]
    member _.RegressionSnapshot() =
        let cases = Corpus.loadDirectory casesDir
        Assert.IsTrue(cases.Length > 0, sprintf "Corpus at '%s' is empty." casesDir)

        let current = Snapshotter.generate cases

        if regenEnabled then
            let dir =
                match projectDir with
                | Some d -> d
                | None -> failwith "Could not locate the project directory to write the baseline to."
            let baselineDir = Path.Combine(dir, "Baselines")
            let archiveDir = Path.Combine(baselineDir, "Archive")
            Directory.CreateDirectory(baselineDir) |> ignore
            Directory.CreateDirectory(archiveDir) |> ignore

            let json = Snapshotter.serialize current
            File.WriteAllText(Path.Combine(baselineDir, "baseline.json"), json)
            File.WriteAllText(Path.Combine(archiveDir, sprintf "baseline-%s.json" current.LibraryVersion), json)

            printfn "Regenerated baseline for version %s (%d entries) at %s"
                current.LibraryVersion current.Entries.Length baselineDir
        else
            if not (File.Exists baselinePath) then
                Assert.Fail(
                    sprintf "No baseline found at '%s'. Generate one by running with FORMULA_REGEN_BASELINE=1." baselinePath)

            let baseline = Snapshotter.deserialize (File.ReadAllText baselinePath)
            let mismatches = Comparer.compare (optionsFor cases) baseline current

            if not (List.isEmpty mismatches) then
                let header =
                    sprintf "%d result(s) changed vs baseline version %s (current %s):"
                        mismatches.Length baseline.LibraryVersion current.LibraryVersion
                Assert.Fail(header + Environment.NewLine + Comparer.report mismatches)

    /// Diff two arbitrary stored snapshots (e.g. two archived versions). Inconclusive
    /// unless FORMULA_COMPARE_A and FORMULA_COMPARE_B point at snapshot files.
    [<TestMethod>]
    member _.CompareVersions() =
        let pathA = Environment.GetEnvironmentVariable("FORMULA_COMPARE_A")
        let pathB = Environment.GetEnvironmentVariable("FORMULA_COMPARE_B")

        if String.IsNullOrWhiteSpace pathA || String.IsNullOrWhiteSpace pathB then
            Assert.Inconclusive("Set FORMULA_COMPARE_A and FORMULA_COMPARE_B to two snapshot files to diff.")
        else
            let snapA = Snapshotter.deserialize (File.ReadAllText pathA)
            let snapB = Snapshotter.deserialize (File.ReadAllText pathB)
            let mismatches = Comparer.compare (fun _ -> ComparisonOptions.exact) snapA snapB

            printfn "Comparing %s (v%s) -> %s (v%s): %d difference(s)"
                pathA snapA.LibraryVersion pathB snapB.LibraryVersion mismatches.Length
            if not (List.isEmpty mismatches) then
                printfn "%s" (Comparer.report mismatches)

    /// Every engine permutation (Interpreter/Compiler/ILCompiler, with and without
    /// constant folding) must agree on every case in the corpus. Fails listing any
    /// case whose permutations disagree, showing each permutation's result.
    ///
    /// NOTE: this test currently FAILS. It surfaces a real inconsistency — the
    /// ConstantFolder evaluates comparisons via numeric coercion while the backends
    /// compare the `value` union structurally, so type-mixed/text comparisons (e.g.
    /// "1 = true", "\"abc\" = \"abc\"") differ between the folded and unfolded
    /// permutations. The test should pass once the pipeline agrees.
    [<TestMethod>]
    member _.CrossEngineConsistency() =
        let cases = Corpus.loadDirectory casesDir
        Assert.IsTrue(cases.Length > 0, sprintf "Corpus at '%s' is empty." casesDir)

        let options = optionsFor cases

        let failures =
            [ for case in cases do
                let entries = Engines.evaluate case
                match entries with
                | [] -> ()
                | reference :: rest ->
                    let disagree =
                        rest |> List.exists (fun e -> not (Comparer.resultsEqual (options case.Id) reference.Result e.Result))
                    if disagree then
                        yield (case, entries) ]

        if not (List.isEmpty failures) then
            let report =
                failures
                |> List.map (fun (case, entries) ->
                    let perms =
                        entries
                        |> List.map (fun e -> sprintf "    %-24s = %s" e.Engine (Comparer.formatResult e.Result))
                        |> String.concat Environment.NewLine
                    sprintf "%s  [%s]%s%s" case.Id case.Formula Environment.NewLine perms)
                |> String.concat Environment.NewLine
            Assert.Fail(
                sprintf "%d expression(s) produced different results across engine permutations:%s%s"
                    failures.Length Environment.NewLine report)

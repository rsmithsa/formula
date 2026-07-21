//-----------------------------------------------------------------------
// <copyright file="Model.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

/// Core data types for the regression snapshot harness.
module Formula.Parser.RegressionTests.Model

open Formula.Parser.Ast

/// The outcome of evaluating a single case through a single engine.
/// A formula either computes a finite/non-finite number, returns no value
/// (the language's `Nothing`), or throws while parsing/evaluating.
type ResultValue =
    | Computed of float
    | NoValue
    | Failed of string

/// A single formula case from the corpus. `Variables` are typed `value`s
/// (number, boolean, text or nothing) so coercion behaviour can be exercised.
/// Every case is evaluated across all engine permutations (see Engines); constant
/// folding is one of those permutations rather than a per-case flag.
type Case =
    { Id: string
      Formula: string
      Variables: Map<string, value>
      /// Name of a function provider config in the Corpus registry (e.g. "default").
      FunctionProvider: string
      /// Name of a variable provider config in the Corpus registry (e.g. "map").
      VariableProvider: string
      /// Optional per-case absolute tolerance override for comparison.
      Tolerance: float option }

/// One recorded result: a case evaluated through one engine permutation. The
/// `Engine` name encodes both the backend and whether constant folding was applied
/// (e.g. "Interpreter", "Interpreter+ConstantFold").
type SnapshotEntry =
    { CaseId: string
      Engine: string
      Result: ResultValue }

/// A full, versioned baseline of every entry produced by a corpus.
type Snapshot =
    { LibraryVersion: string
      GeneratedUtc: string
      Engines: string list
      Entries: SnapshotEntry list }

/// How two `Computed` values are compared. `Epsilon = 0.0` means exact equality.
type ComparisonOptions =
    { Epsilon: float
      Relative: bool }

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ComparisonOptions =
    /// Exact bit-level comparison (the default).
    let exact = { Epsilon = 0.0; Relative = false }

    /// An absolute-tolerance comparison.
    let absolute epsilon = { Epsilon = epsilon; Relative = false }

/// A difference between a baseline entry and the current one, keyed by (case, engine, folded).
/// `Baseline`/`Current` are `None` when the entry was added or removed.
type Mismatch =
    { CaseId: string
      Engine: string
      Baseline: ResultValue option
      Current: ResultValue option }

//-----------------------------------------------------------------------
// <copyright file="Engines.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

/// Evaluates a case through every engine permutation, capturing the result
/// (or the failure) as a `ResultValue`.
module Formula.Parser.RegressionTests.Engines

open Formula.Parser
open Formula.Parser.Ast
open Formula.Parser.Parser
open Formula.Parser.ConstantFolder
open Formula.Parser.Interpreter

open Formula.Parser.RegressionTests.Model
open Formula.Parser.RegressionTests.Corpus

/// The three execution backends. Each takes an AST plus providers and yields
/// `float option` (`None` == the language's `Nothing`).
let private backends
    : (string * (IAstItem<expr> -> IVariableProvider -> IFunctionProvider -> float option)) list =
    [ "Interpreter", (fun ast varP funcP -> interpretFormula ast varP funcP)
      "Compiler", (fun ast varP funcP -> (Compiler.compileFormula<float option> ast).Invoke(varP, funcP))
      "ILCompiler", (fun ast varP funcP -> (ILCompiler.compileFormula<float option> ast).Invoke(varP, funcP)) ]

/// The full set of engine permutations: each backend crossed with whether the
/// ConstantFolder pass is applied first. Constant folding is treated as a first-class
/// engine permutation, giving 6 permutations in stable order.
let permutations
    : (string * bool * (IAstItem<expr> -> IVariableProvider -> IFunctionProvider -> float option)) list =
    [ for name, run in backends do
        (name, false, run)
        (name + "+ConstantFold", true, run) ]

/// The engine permutation names, in order, for stamping into a snapshot's metadata.
let engineNames = permutations |> List.map (fun (name, _, _) -> name)

let private toResult (value: float option) =
    match value with
    | Some v -> Computed v
    | None -> NoValue

let private describe (ex: exn) =
    sprintf "%s: %s" (ex.GetType().Name) ex.Message

/// Evaluate one case across all engine permutations. Parse and evaluation failures
/// are captured as `Failed`, never thrown, so one bad case cannot abort a whole run.
let evaluate (case: Case) : SnapshotEntry list =
    let funcP = buildFunctionProvider case.FunctionProvider
    let varP = buildVariableProvider case.VariableProvider case.Variables

    // Parse once. A parse failure applies to every permutation.
    let parsed =
        try Ok(parseFormula case.Formula)
        with ex -> Result.Error(describe ex)

    [ for engineName, fold, run in permutations do
        let result =
            match parsed with
            | Result.Error msg -> Failed msg
            | Ok ast ->
                try
                    let target = if fold then foldConstantsFunctions ast funcP else ast
                    toResult (run target varP funcP)
                with ex -> Failed(describe ex)
        { CaseId = case.Id
          Engine = engineName
          Result = result } ]

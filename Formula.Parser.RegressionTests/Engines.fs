//-----------------------------------------------------------------------
// <copyright file="Engines.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

/// Evaluates a case through every execution backend, capturing the result
/// (or the failure) as a `ResultValue`.
module Formula.Parser.RegressionTests.Engines

open Formula.Parser
open Formula.Parser.Ast
open Formula.Parser.Parser
open Formula.Parser.ConstantFolder
open Formula.Parser.Interpreter

open Formula.Parser.RegressionTests.Model
open Formula.Parser.RegressionTests.Corpus

/// The engines exercised, in stable order. Each takes an AST plus providers and
/// yields `float option` (`None` == the language's `Nothing`).
let private engines
    : (string * (IAstItem<expr> -> IVariableProvider -> IFunctionProvider -> float option)) list =
    [ "Interpreter", (fun ast varP funcP -> interpretFormula ast varP funcP)
      "Compiler", (fun ast varP funcP -> (Compiler.compileFormula<float option> ast).Invoke(varP, funcP))
      "ILCompiler", (fun ast varP funcP -> (ILCompiler.compileFormula<float option> ast).Invoke(varP, funcP)) ]

/// The engine names, in order, for stamping into a snapshot's metadata.
let engineNames = engines |> List.map fst

let private toResult (value: float option) =
    match value with
    | Some v -> Computed v
    | None -> NoValue

let private describe (ex: exn) =
    sprintf "%s: %s" (ex.GetType().Name) ex.Message

/// Evaluate one case, producing an entry per engine and (when `Fold` is set) an
/// additional folded entry per engine. Parse and evaluation failures are captured
/// as `Failed`, never thrown, so one bad case cannot abort a whole run.
let evaluate (case: Case) : SnapshotEntry list =
    let funcP = buildFunctionProvider case.FunctionProvider
    let varP = buildVariableProvider case.VariableProvider case.Variables

    // Parse once. A parse failure applies to every engine/fold combination.
    let parsed =
        try Ok(parseFormula case.Formula)
        with ex -> Result.Error(describe ex)

    let folds = if case.Fold then [ false; true ] else [ false ]

    [ for folded in folds do
        for (engineName, run) in engines do
            let result =
                match parsed with
                | Result.Error msg -> Failed msg
                | Ok ast ->
                    try
                        let target = if folded then foldConstantsFunctions ast funcP else ast
                        toResult (run target varP funcP)
                    with ex -> Failed(describe ex)
            { CaseId = case.Id
              Engine = engineName
              Folded = folded
              Result = result } ]

//-----------------------------------------------------------------------
// <copyright file="DependencyExtractor.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module DependencyExtractor =

    open Formula.Parser.Ast

    let rec extractDependencies ast deps =

        match ast with
        | Constant c ->
            deps
        | Variable (v, r) ->
            match r with
            | Some (a, b) ->
                (extractDependencies b (extractDependencies a (v::deps)))
            | None -> v::deps
        | Negation n ->
            extractDependencies n deps
        | Arithmetic (a, op, b) ->
            extractDependencies b (extractDependencies a deps)
        | Inversion i ->
            extractDependencies i deps
        | Comparison (a, op, b) ->
            extractDependencies b (extractDependencies a deps)
        | Logical (a, op, b) ->
            extractDependencies b (extractDependencies a deps)
        | Function (f, args) ->
            args |> List.map (fun a -> extractDependencies a deps) |> List.concat
        | Branch (cond, a, b) ->
            extractDependencies b (extractDependencies a (extractDependencies cond deps))

    let rec extractDependenciesWithRanges ast deps =

        match ast with
        | Constant c ->
            deps
        | Variable (v, r) ->
            let d = (v, r)
            match r with
            | Some (a, b) ->
                (extractDependenciesWithRanges b (extractDependenciesWithRanges a (d::deps)))
            | None -> d::deps
        | Negation n ->
            extractDependenciesWithRanges n deps
        | Arithmetic (a, op, b) ->
            extractDependenciesWithRanges b (extractDependenciesWithRanges a deps)
        | Inversion i ->
            extractDependenciesWithRanges i deps
        | Comparison (a, op, b) ->
            extractDependenciesWithRanges b (extractDependenciesWithRanges a deps)
        | Logical (a, op, b) ->
            extractDependenciesWithRanges b (extractDependenciesWithRanges a deps)
        | Function (f, args) ->
            args |> List.map (fun a -> extractDependenciesWithRanges a deps) |> List.concat
        | Branch (cond, a, b) ->
            extractDependenciesWithRanges b (extractDependenciesWithRanges a (extractDependenciesWithRanges cond deps))


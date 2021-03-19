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
        | { item = Constant c } ->
            deps
        | { item = Variable (v, r) } ->
            match r with
            | Some (a, b) ->
                (extractDependencies b (extractDependencies a (v::deps)))
            | None -> v::deps
        | { item = Negation n }->
            extractDependencies n deps
        | { item = Arithmetic (a, op, b) } ->
            extractDependencies b (extractDependencies a deps)
        | { item = Inversion i } ->
            extractDependencies i deps
        | { item = Comparison (a, op, b) } ->
            extractDependencies b (extractDependencies a deps)
        | { item = Logical (a, op, b) } ->
            extractDependencies b (extractDependencies a deps)
        | { item = Function (f, args) } ->
            args |> List.map (fun a -> extractDependencies a deps) |> List.concat
        | { item = Branch (cond, a, b) } ->
            extractDependencies b (extractDependencies a (extractDependencies cond deps))

    let rec extractDependenciesWithRanges ast deps =

        match ast with
        | { item = Constant c } ->
            deps
        | { item = Variable (v, r) } ->
            let d = (v, r)
            match r with
            | Some (a, b) ->
                (extractDependenciesWithRanges b (extractDependenciesWithRanges a (d::deps)))
            | None -> d::deps
        | { item = Negation n } ->
            extractDependenciesWithRanges n deps
        | { item = Arithmetic (a, op, b) } ->
            extractDependenciesWithRanges b (extractDependenciesWithRanges a deps)
        | { item = Inversion i } ->
            extractDependenciesWithRanges i deps
        | { item = Comparison (a, op, b) } ->
            extractDependenciesWithRanges b (extractDependenciesWithRanges a deps)
        | { item = Logical (a, op, b) } ->
            extractDependenciesWithRanges b (extractDependenciesWithRanges a deps)
        | { item = Function (f, args) } ->
            args |> List.map (fun a -> extractDependenciesWithRanges a deps) |> List.concat
        | { item = Branch (cond, a, b) } ->
            extractDependenciesWithRanges b (extractDependenciesWithRanges a (extractDependenciesWithRanges cond deps))


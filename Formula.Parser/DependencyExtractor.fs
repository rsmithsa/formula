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
        | Variable v ->
            v::deps
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
            //let res = args |> List.map foldConstants
            //Function(f, res)
            deps
        | Branch (cond, a, b) ->
            extractDependencies b (extractDependencies a (extractDependencies cond deps))


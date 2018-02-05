//-----------------------------------------------------------------------
// <copyright file="Interpreter.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module Interpreter =

    open Formula.Parser.Ast

    // Would be nice to be Map<string, expr>
    type VariableMap = Map<string, float>

    // Hardcoded for now
    let functions: Map<string, float list -> float> =
        Map.empty.
            Add("SQRT", fun(args: float list) -> sqrt args.[0]).
            Add("PI", fun(args: float list) -> System.Math.PI).
            Add("POW", fun(args: float list) -> args.[0] ** args.[1]).
            Add("COUNT", fun(args: float list) -> float(args.Length))

    let rec interpretFormula ast (vars: VariableMap) =

        let castToBool value =
            match value with
            | 0.0 -> 
                false
            | _ ->
                true

        let castToDouble value =
            match value with
            | true ->
                1.0
            | false ->
                0.0

        let interpretConstant constant =
            match constant with
            | Number n -> n
            | Boolean b -> castToDouble b

        let interpetVariable variable =
            match variable with
            | Identifier id -> vars.[id]

        let interpretNegation negation = 
            -interpretFormula negation vars

        let interpretArtithmetic a op b =
            match op with
            | Add ->
                interpretFormula a vars + interpretFormula b vars
            | Subtract ->
                interpretFormula a vars - interpretFormula b vars
            | Multiply ->
                interpretFormula a vars * interpretFormula b vars
            | Divide ->
                interpretFormula a vars / interpretFormula b vars
            | Power ->
                interpretFormula a vars ** interpretFormula b vars

        let interpretInversion inversion =
            let value = castToBool (interpretFormula inversion vars)
            castToDouble (not value)

        let interpretComparison a op b =
            let valueA = interpretFormula a vars
            let valueB = interpretFormula b vars
            match op with
            | Equal ->
                castToDouble (valueA = valueB)
            | NotEqual ->
                castToDouble (valueA <> valueB)
            | GreaterThan ->
                castToDouble (valueA > valueB)
            | LessThan ->
                castToDouble (valueA < valueB)
            | GreaterThanEqual ->
                castToDouble (valueA >= valueB)
            | LessThanEqual ->
                castToDouble (valueA <= valueB)

        let interpretLogical a op b =
            let valueA = castToBool (interpretFormula a vars)
            let valueB = lazy (castToBool (interpretFormula b vars))
            match op with
            | And ->
                castToDouble (valueA && valueB.Force())
            | Or ->
                castToDouble (valueA || valueB.Force())

        let interpretFunction f args =
            match f with
            | Identifier id ->
                let interpretArg arg = interpretFormula arg vars
                let interpretedArgs = args |> List.map interpretArg
                functions.[id] interpretedArgs

        match ast with
        | Constant c ->
            interpretConstant c
        | Variable v ->
            interpetVariable v
        | Negation n ->
            interpretNegation n
        | Arithmetic (a, op, b) ->
            interpretArtithmetic a op b
        | Inversion i ->
            interpretInversion i
        | Comparison (a, op, b) ->
            interpretComparison a op b
        | Logical (a, op, b) ->
            interpretLogical a op b
        | Function (f, args) ->
            interpretFunction f args
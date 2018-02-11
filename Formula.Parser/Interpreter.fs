//-----------------------------------------------------------------------
// <copyright file="Interpreter.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module Interpreter =

    open Formula.Parser.Ast

    let rec interpretFormula ast (vars: IVariableProvider) (functions: IFunctionProvider) =

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
            | Identifier id -> vars.Lookup id

        let interpretNegation negation = 
            -interpretFormula negation vars functions

        let interpretArtithmetic a op b =
            match op with
            | Add ->
                interpretFormula a vars functions + interpretFormula b vars functions
            | Subtract ->
                interpretFormula a vars functions - interpretFormula b vars functions
            | Multiply ->
                interpretFormula a vars functions * interpretFormula b vars functions
            | Divide ->
                interpretFormula a vars functions / interpretFormula b vars functions
            | Modulus ->
                interpretFormula a vars functions % interpretFormula b vars functions
            | Power ->
                interpretFormula a vars functions ** interpretFormula b vars functions

        let interpretInversion inversion =
            let value = castToBool (interpretFormula inversion vars functions)
            castToDouble (not value)

        let interpretComparison a op b =
            let valueA = interpretFormula a vars functions
            let valueB = interpretFormula b vars functions
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
            let valueA = castToBool (interpretFormula a vars functions)
            let valueB = lazy (castToBool (interpretFormula b vars functions))
            match op with
            | And ->
                castToDouble (valueA && valueB.Force())
            | Or ->
                castToDouble (valueA || valueB.Force())

        let interpretFunction f args =
            match f with
            | Identifier id ->
                let interpretArg arg = interpretFormula arg vars functions
                let interpretedArgs = args |> List.map interpretArg
                let imp = functions.Lookup id
                imp.Execute (List.toArray interpretedArgs)

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
//-----------------------------------------------------------------------
// <copyright file="Interpreter.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module Interpreter =

    open Formula.Parser.Ast

    let rec interpretFormula ast (vars: IVariableProvider) (functions: IFunctionProvider) =

        let interpretConstant constant =
            match constant with
            | Number n -> n
            | Boolean b -> Helpers.castToDouble b

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
            let value = Helpers.castToBool (interpretFormula inversion vars functions)
            Helpers.castToDouble (not value)

        let interpretComparison a op b =
            let valueA = interpretFormula a vars functions
            let valueB = interpretFormula b vars functions
            match op with
            | Equal ->
                Helpers.castToDouble (valueA = valueB)
            | NotEqual ->
                Helpers.castToDouble (valueA <> valueB)
            | GreaterThan ->
                Helpers.castToDouble (valueA > valueB)
            | LessThan ->
                Helpers.castToDouble (valueA < valueB)
            | GreaterThanEqual ->
                Helpers.castToDouble (valueA >= valueB)
            | LessThanEqual ->
                Helpers.castToDouble (valueA <= valueB)

        let interpretLogical a op b =
            let valueA = Helpers.castToBool (interpretFormula a vars functions)
            let valueB = lazy (Helpers.castToBool (interpretFormula b vars functions))
            match op with
            | And ->
                Helpers.castToDouble (valueA && valueB.Force())
            | Or ->
                Helpers.castToDouble (valueA || valueB.Force())

        let interpretFunction f args =
            match f with
            | Identifier id ->
                let interpretArg arg = interpretFormula arg vars functions
                let interpretedArgs = args |> List.map interpretArg
                let imp = functions.Lookup id
                imp.Execute (List.toArray interpretedArgs)

        let interpretBranch cond a b =
            let valueCond = Helpers.castToBool (interpretFormula cond vars functions)
            match valueCond with
            | true ->
                interpretFormula a vars functions
            | false ->
                interpretFormula b vars functions

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
        | Branch (cond, a, b) ->
            interpretBranch cond a b
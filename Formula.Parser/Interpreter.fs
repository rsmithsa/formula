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
        | Function (f, args) ->
            interpretFunction f args
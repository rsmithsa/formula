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
            Add("SQRT", fun(args: float list) -> sqrt args.Head).
            Add("PI", fun(args: float list) -> System.Math.PI)

    let rec interpretFormula ast (vars: VariableMap) =
        let interpretConstant constant =
            match constant with
            | Number n -> n

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
        | Function (f, args) ->
            interpretFunction f args
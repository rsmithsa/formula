//-----------------------------------------------------------------------
// <copyright file="FunctionValidator.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

open FParsec

module FunctionValidator =
    
    open Formula.Parser.Ast
    
    let rec validateFunctions (ast: IAstItem<expr>) (functions: IFunctionProvider) errors =
        
        match ast.Item with
        | Constant c ->
            errors
        | Variable (v, r, idx) ->
            match r with
            | Some (a, b) ->
                (validateFunctions b functions (validateFunctions a functions errors))
            | None ->
                match idx with
                | Some i -> (validateFunctions i functions errors)
                | None -> errors
        | Coalesce (a, b) ->
            validateFunctions b functions (validateFunctions a functions errors)
        | Negation n ->
            validateFunctions n functions errors
        | Arithmetic (a, op, b) ->
            validateFunctions b functions (validateFunctions a functions errors)
        | Inversion i ->
            validateFunctions i functions errors
        | Comparison (a, op, b) ->
            validateFunctions b functions (validateFunctions a functions errors)
        | Logical (a, op, b) ->
            validateFunctions b functions (validateFunctions a functions errors)
        | Function (f, args) ->
            let id = f.Item.IdentifierValue
            let item =
                match f with
                | :? IPositionedAstItem<identifier> as positionedAstItem -> positionedAstItem
                | _ -> { Item = f.Item; StartPosition = Position("", -1L, -1L, -1L); EndPosition = Position("", -1L, -1L, -1L) } :> IPositionedAstItem<identifier>
            match functions.IsDefined id with
            | true ->
                // TODO Validate parameter counts? Non-deterministic though?
                (args |> List.map (fun a -> validateFunctions a functions errors) |> List.concat) @ errors
            | false -> ($"Unknown function: {id}", item)::errors
        | Branch (cond, a, b) ->
            validateFunctions b functions (validateFunctions a functions (validateFunctions cond functions errors))
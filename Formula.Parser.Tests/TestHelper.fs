//-----------------------------------------------------------------------
// <copyright file="TestHelper.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

module TestHelper

open Formula.Parser
open Formula.Parser.Ast

let rec stripPositions (ast: IAstItem<expr>): IAstItem<expr> =
    match ast.Item with
    | Constant c as item ->
        { Item = Constant({ Item = c.Item }) } :> IAstItem<expr>
    | Variable (v, r, idx) as item ->
        match r with
        | Some (a, b) ->
            { Item = Variable({ Item = v.Item }, Some(stripPositions a, stripPositions b), None) } :> IAstItem<expr>
        | None ->
            match idx with
            | Some i -> { Item = Variable({ Item = v.Item }, None, Some(stripPositions i)) } :> IAstItem<expr>
            | None -> { Item = Variable({ Item = v.Item }, None, None) } :> IAstItem<expr>
    | Negation n as item ->
        { Item = Negation(stripPositions n) } :> IAstItem<expr>
    | Arithmetic (a, op, b) ->
        { Item = Arithmetic(stripPositions a, { Item = op.Item }, stripPositions b) } :> IAstItem<expr>
    | Inversion i ->
        { Item = Inversion(stripPositions i) } :> IAstItem<expr>
    | Comparison (a, op, b) ->
        { Item = Comparison(stripPositions a, { Item = op.Item }, stripPositions b) } :> IAstItem<expr>
    | Logical (a, op, b) ->
        { Item = Logical(stripPositions a, { Item = op.Item }, stripPositions b) } :> IAstItem<expr>
    | Function (f, args) ->
        { Item = Function({ Item = f.Item }, args |> List.map (fun a -> stripPositions a)) } :> IAstItem<expr>
    | Branch (cond, a, b) ->
        { Item = Branch(stripPositions cond, stripPositions a, stripPositions b) } :> IAstItem<expr>

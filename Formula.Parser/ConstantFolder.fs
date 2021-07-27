//-----------------------------------------------------------------------
// <copyright file="ConstantFolder.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module ConstantFolder =

    open Formula.Parser.Ast

    let rec foldConstants (ast: IAstItem<expr>) : IAstItem<expr> =

        match ast.Item with
        | Constant c ->
            { Item = Constant(c) } :> IAstItem<expr>
        | Variable (v, r, idx) ->
            match r with
            | Some (a, b) ->
                let resA = foldConstants a
                let resB = foldConstants b
                { Item = Variable(v, Some(resA, resB), None) } :> IAstItem<expr>
            | None ->
                    match idx with
                    | Some i ->
                        let res = foldConstants i
                        { Item = Variable(v, None, Some(res)) } :> IAstItem<expr>
                    | None -> { Item = Variable(v, r, idx) } :> IAstItem<expr>
        | Negation n ->
            let res = foldConstants n
            match res.Item with
            | Constant c ->
                match Helpers.castToDouble c.Item with
                | Some value -> { Item = Constant({ Item = Number(-value) }) } :> IAstItem<expr>
                | None -> { Item = Constant({ Item = Nothing }) } :> IAstItem<expr> 
            | _ -> { Item = Negation(res) } :> IAstItem<expr>
        | Arithmetic (a, op, b) ->
            let resA = foldConstants a
            let resB = foldConstants b
            match (resA.Item, resB.Item) with
            | (Constant cA, Constant cB) ->
                match (Helpers.castToDouble cA.Item, Helpers.castToDouble cB.Item) with
                | (Some valueA, Some valueB) ->
                    match op.Item with
                    | Add ->
                        { Item = Constant({ Item = Number(valueA + valueB) }) } :> IAstItem<expr>
                    | Subtract ->
                        { Item = Constant({ Item = Number(valueA - valueB) }) } :> IAstItem<expr>
                    | Multiply ->
                        { Item = Constant({ Item = Number(valueA * valueB) }) } :> IAstItem<expr>
                    | Divide ->
                        { Item = Constant({ Item = Number(valueA / valueB) }) } :> IAstItem<expr>
                    | Modulus ->
                        { Item = Constant({ Item = Number(valueA % valueB) }) } :> IAstItem<expr>
                    | Power ->
                        { Item = Constant({ Item = Number(valueA ** valueB) }) } :> IAstItem<expr>
                | _ -> { Item = Constant({ Item = Nothing }) } :> IAstItem<expr>
            | _ -> { Item = Arithmetic(resA, op, resB) } :> IAstItem<expr>
        | Inversion i ->
            let res = foldConstants i
            match res.Item with
            | Constant c ->
                let value = Helpers.castToBool c.Item
                { Item = Constant({ Item = Boolean(not value) }) } :> IAstItem<expr>
            | _ -> { Item = Inversion(res) } :> IAstItem<expr>
        | Comparison (a, op, b) ->
            let resA = foldConstants a
            let resB = foldConstants b
            match (resA.Item, resB.Item) with
            | (Constant cA, Constant cB) ->
                let valueA = Helpers.castToDouble cA.Item
                let valueB = Helpers.castToDouble cB.Item

                match op.Item with
                | Equal ->
                    { Item = Constant({ Item = Boolean(valueA = valueB) }) } :> IAstItem<expr>
                | NotEqual ->
                    { Item = Constant({ Item = Boolean(valueA <> valueB) }) } :> IAstItem<expr>
                | GreaterThan ->
                    { Item = Constant({ Item = Boolean(valueA > valueB) }) } :> IAstItem<expr>
                | LessThan ->
                    { Item = Constant({ Item = Boolean(valueA < valueB) }) } :> IAstItem<expr>
                | GreaterThanEqual ->
                    { Item = Constant({ Item = Boolean(valueA >= valueB) }) } :> IAstItem<expr>
                | LessThanEqual ->
                    { Item = Constant({ Item = Boolean(valueA <= valueB) }) } :> IAstItem<expr>
            | _ -> { Item = Comparison(resA, op, resB) } :> IAstItem<expr>
        | Logical (a, op, b) ->
            let resA = foldConstants a
            let resB = foldConstants b
            match (resA.Item, resB.Item) with
            | (Constant cA, Constant cB) ->
                let valueA = Helpers.castToBool cA.Item
                let valueB = Helpers.castToBool cB.Item

                match op.Item with
                | And ->
                    { Item = Constant({ Item = Boolean(valueA && valueB) }) } :> IAstItem<expr>
                | Or ->
                    { Item = Constant({ Item = Boolean(valueA || valueB) }) } :> IAstItem<expr>
            | _ -> { Item = Logical(resA, op, resB) } :> IAstItem<expr>
        | Function (f, args) ->
            let res = args |> List.map foldConstants
            { Item = Function(f, res) } :> IAstItem<expr>
        | Branch (cond, a, b) ->
            let resCond  = foldConstants cond
            match resCond.Item with
            | Constant cCond ->
                let condVal = Helpers.castToBool cCond.Item
                match condVal with
                | true ->
                    foldConstants a
                | false ->
                    foldConstants b
            | _ ->
                let resA = foldConstants a
                let resB = foldConstants b
                { Item = Branch(cond, resA, resB) } :> IAstItem<expr>

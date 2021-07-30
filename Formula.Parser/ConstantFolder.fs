//-----------------------------------------------------------------------
// <copyright file="ConstantFolder.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module ConstantFolder =

    open Formula.Parser.Ast

    let rec foldConstantsFunctions (ast: IAstItem<expr>) (functions: IFunctionProvider) : IAstItem<expr> =

        match ast.Item with
        | Constant c ->
            { Item = Constant(c) } :> IAstItem<expr>
        | Variable (v, r, idx) ->
            match r with
            | Some (a, b) ->
                let resA = foldConstantsFunctions a functions
                let resB = foldConstantsFunctions b functions
                { Item = Variable(v, Some(resA, resB), None) } :> IAstItem<expr>
            | None ->
                    match idx with
                    | Some i ->
                        let res = foldConstantsFunctions i functions
                        { Item = Variable(v, None, Some(res)) } :> IAstItem<expr>
                    | None -> { Item = Variable(v, r, idx) } :> IAstItem<expr>
        | Negation n ->
            let res = foldConstantsFunctions n functions
            match res.Item with
            | Constant c ->
                match Helpers.castToDouble c.Item with
                | Some value -> { Item = Constant({ Item = Number(-value) }) } :> IAstItem<expr>
                | None -> { Item = Constant({ Item = Nothing }) } :> IAstItem<expr> 
            | _ -> { Item = Negation(res) } :> IAstItem<expr>
        | Arithmetic (a, op, b) ->
            let resA = foldConstantsFunctions a functions
            let resB = foldConstantsFunctions b functions
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
            let res = foldConstantsFunctions i functions
            match res.Item with
            | Constant c ->
                let value = Helpers.castToBool c.Item
                { Item = Constant({ Item = Boolean(not value) }) } :> IAstItem<expr>
            | _ -> { Item = Inversion(res) } :> IAstItem<expr>
        | Comparison (a, op, b) ->
            let resA = foldConstantsFunctions a functions
            let resB = foldConstantsFunctions b functions
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
            let resA = foldConstantsFunctions a functions
            let resB = foldConstantsFunctions b functions
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
            match f.Item with
            | Identifier id ->
                let foldArg arg = foldConstantsFunctions arg functions
                let res = args |> List.map foldArg
                
                match (functions.IsDefined id) with
                | true ->
                    let imp = functions.Lookup id
                    match imp.IsNonDeterministic with
                    | false ->
                        let constArgs =
                            res |> List.choose (
                                fun x ->
                                    match x.Item with
                                    | Constant c -> Some(c.Item)
                                    | _ -> None
                            ) |> List.toArray
                        match constArgs.Length = res.Length with
                        | true ->
                            let result = imp.Execute constArgs
                            { Item = Constant({ Item = result}) } :> IAstItem<expr>
                        | false -> { Item = Function(f, res) } :> IAstItem<expr>
                    | true -> { Item = Function(f, res) } :> IAstItem<expr>
                | false -> { Item = Function(f, res) } :> IAstItem<expr>
        | Branch (cond, a, b) ->
            let resCond  = foldConstantsFunctions cond functions
            match resCond.Item with
            | Constant cCond ->
                let condVal = Helpers.castToBool cCond.Item
                match condVal with
                | true ->
                    foldConstantsFunctions a functions
                | false ->
                    foldConstantsFunctions b functions
            | _ ->
                let resA = foldConstantsFunctions a functions
                let resB = foldConstantsFunctions b functions
                { Item = Branch(cond, resA, resB) } :> IAstItem<expr>

    let foldConstants (ast: IAstItem<expr>) : IAstItem<expr> =
        foldConstantsFunctions ast EmptyFunctionProvider.Instance
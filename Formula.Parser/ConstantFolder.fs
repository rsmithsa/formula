//-----------------------------------------------------------------------
// <copyright file="ConstantFolder.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module ConstantFolder =

    open Formula.Parser.Ast

    let rec foldConstants ast =

        match ast with
        | Constant c ->
            Constant(c)
        | Variable v ->
            Variable(v)
        | Negation n ->
            let res = foldConstants n
            match res with
            | Constant c ->
                let value = Helpers.castToDouble c
                Constant(Number(-value))
            | _ -> Negation(res)
        | Arithmetic (a, op, b) ->
            let resA = foldConstants a
            let resB = foldConstants b
            match (resA, resB) with
            | (Constant cA, Constant cB) ->
                let valueA = Helpers.castToDouble cA
                let valueB = Helpers.castToDouble cB

                match op with
                | Add ->
                    Constant(Number(valueA + valueB))
                | Subtract ->
                    Constant(Number(valueA - valueB))
                | Multiply ->
                    Constant(Number(valueA * valueB))
                | Divide ->
                    Constant(Number(valueA / valueB))
                | Modulus ->
                    Constant(Number(valueA % valueB))
                | Power ->
                    Constant(Number(valueA ** valueB))
            | _ -> Arithmetic(resA, op, resB)
        | Inversion i ->
            let res = foldConstants i
            match res with
            | Constant c ->
                let value = Helpers.castToBool c
                Constant(Boolean(not value))
            | _ -> Inversion(res)
        | Comparison (a, op, b) ->
            let resA = foldConstants a
            let resB = foldConstants b
            match (resA, resB) with
            | (Constant cA, Constant cB) ->
                let valueA = Helpers.castToDouble cA
                let valueB = Helpers.castToDouble cB

                match op with
                | Equal ->
                    Constant(Boolean(valueA = valueB))
                | NotEqual ->
                    Constant(Boolean(valueA <> valueB))
                | GreaterThan ->
                    Constant(Boolean(valueA > valueB))
                | LessThan ->
                    Constant(Boolean(valueA < valueB))
                | GreaterThanEqual ->
                    Constant(Boolean(valueA >= valueB))
                | LessThanEqual ->
                    Constant(Boolean(valueA <= valueB))
            | _ -> Comparison(resA, op, resB)
        | Logical (a, op, b) ->
            let resA = foldConstants a
            let resB = foldConstants b
            match (resA, resB) with
            | (Constant cA, Constant cB) ->
                let valueA = Helpers.castToBool cA
                let valueB = Helpers.castToBool cB

                match op with
                | And ->
                    Constant(Boolean(valueA && valueB))
                | Or ->
                    Constant(Boolean(valueA || valueB))
            | _ -> Logical(resA, op, resB)
        | Function (f, args) ->
            let res = args |> List.map foldConstants
            Function(f, res)
        | Branch (cond, a, b) ->
            let resCond  = foldConstants cond
            match resCond with
            | Constant cCond ->
                let condVal = Helpers.castToBool cCond
                match condVal with
                | true ->
                    foldConstants a
                | false ->
                    foldConstants b
            | _ ->
                let resA = foldConstants a
                let resB = foldConstants b
                Branch(cond, resA, resB)

//-----------------------------------------------------------------------
// <copyright file="ConstantFolder.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module ConstantFolder =

    open Formula.Parser.Ast

    let rec foldConstants ast =

        let castToBool value =
            match value with
            | Number n ->
                match n with
                | 0.0 -> 
                    false
                | _ ->
                    true
            | Boolean b ->
                b

        let castToDouble value =
            match value with
            | Number n ->
                n
            | Boolean b ->
                match b with
                | true ->
                    1.0
                | false ->
                    0.0

        match ast with
        | Constant c ->
            Constant(c)
        | Variable v ->
            Variable(v)
        | Negation n ->
            let res = foldConstants n
            match res with
            | Constant c ->
                let value = castToDouble c
                Constant(Number(-value))
            | _ -> Negation(res)
        | Arithmetic (a, op, b) ->
            let resA = foldConstants a
            let resB = foldConstants b
            match (resA, resB) with
            | (Constant cA, Constant cB) ->
                let valueA = castToDouble cA
                let valueB = castToDouble cB

                match op with
                | Add ->
                    Constant(Number(valueA + valueB))
                | Subtract ->
                    Constant(Number(valueA - valueB))
                | Multiply ->
                    Constant(Number(valueA * valueB))
                | Divide ->
                    Constant(Number(valueA / valueB))
                | Power ->
                    Constant(Number(valueA ** valueB))
            | _ -> Arithmetic(resA, op, resB)
        | Inversion i ->
            let res = foldConstants i
            match res with
            | Constant c ->
                let value = castToBool c
                Constant(Boolean(not value))
            | _ -> Inversion(res)
        | Comparison (a, op, b) ->
            Comparison(a, op, b)
        | Logical (a, op, b) ->
            Logical(a, op, b)
        | Function (f, args) ->
            let res = args |> List.map foldConstants
            Function(f, res)


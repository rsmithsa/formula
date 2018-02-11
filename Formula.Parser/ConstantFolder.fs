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
                let (Number value) = c
                Constant(Number(-value))
            | _ -> Negation(res)
        | Arithmetic (a, op, b) ->
            let resA = foldConstants a
            let resB = foldConstants b
            match (resA, resB) with
            | (Constant cA, Constant cB) ->
                let (Number valueA) = cA
                let (Number valueB) = cB

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
        | Function (f, args) ->
            let res = args |> List.map foldConstants
            Function(f, res)


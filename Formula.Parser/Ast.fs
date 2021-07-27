//-----------------------------------------------------------------------
// <copyright file="Ast.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

open FParsec

type IAstItem<'a> =
    abstract member Item : 'a

type IPositionedAstItem<'a> =
    inherit IAstItem<'a>
    abstract member StartPosition : Position
    abstract member EndPosition : Position

module Ast =

    type astItem<'a> =
        { Item: 'a }
        interface IAstItem<'a> with
            member x.Item = x.Item

    type positionedAstItem<'a> =
        { Item: 'a; StartPosition: Position; EndPosition: Position }
        interface IPositionedAstItem<'a> with
            member x.Item = x.Item
            member x.StartPosition = x.StartPosition
            member x.EndPosition = x.EndPosition

    type identifier = Identifier of string
    type value =
               | Number of float
               | Boolean of bool
               | Text of string
               | Nothing
    with
               static member inline op_Implicit(x: float) = Number(x)
               static member inline op_Implicit(x: bool) = Boolean(x)
               static member inline op_Implicit(x: string) = if x = null then Nothing else Text(x)

               static member op_Equality(a: value, b: value) = a = b
               static member op_Inequality(a: value, b: value) = a <> b
               static member op_LessThanOrEqual(a: value, b: value) = a <= b
               static member op_GreaterThanOrEqual(a: value, b: value) = a >= b
               static member op_LessThan(a: value, b: value) = a < b
               static member op_GreaterThan(a: value, b: value) = a > b
               
               static member private Empty() = Nothing

               member x.NumberValue =
                   match x with
                   | Number n -> n
                   | _ -> invalidOp "Not a number value"

    type arithmetic = Add | Subtract | Multiply | Divide | Modulus | Power
    type comparison = Equal | NotEqual | GreaterThan | LessThan | GreaterThanEqual | LessThanEqual
    type logical = And | Or

    type expr =
              | Constant of IAstItem<value>
              | Variable of IAstItem<identifier> * option<IAstItem<expr> * IAstItem<expr>> * option<IAstItem<expr>>
              | Negation of IAstItem<expr>
              | Arithmetic of IAstItem<expr> * IAstItem<arithmetic> * IAstItem<expr>
              | Inversion of IAstItem<expr>
              | Comparison of IAstItem<expr> * IAstItem<comparison> * IAstItem<expr>
              | Logical of IAstItem<expr> * IAstItem<logical> * IAstItem<expr>
              | Function of IAstItem<identifier> * IAstItem<expr> list
              | Branch of IAstItem<expr> * IAstItem<expr> * IAstItem<expr>

//-----------------------------------------------------------------------
// <copyright file="Ast.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

open FParsec

module Ast = 

    type astItem<'a> = { item: 'a; startPosition: Position; endPosition: Position}

    type identifier = Identifier of string
    type value =
               | Number of float
               | Boolean of bool
               | Text of string
    with
               static member inline op_Implicit(x: float) = Number(x)
               static member inline op_Implicit(x: bool) = Boolean(x)
               static member inline op_Implicit(x: string) = Text(x)

               static member op_Equality(a: value, b: value) = a = b
               static member op_Inequality(a: value, b: value) = a <> b
               static member op_LessThanOrEqual(a: value, b: value) = a <= b
               static member op_GreaterThanOrEqual(a: value, b: value) = a >= b
               static member op_LessThan(a: value, b: value) = a < b
               static member op_GreaterThan(a: value, b: value) = a > b

    type arithmetic = Add | Subtract | Multiply | Divide | Modulus | Power
    type comparison = Equal | NotEqual | GreaterThan | LessThan | GreaterThanEqual | LessThanEqual
    type logical = And | Or

    type expr =
              | Constant of astItem<value>
              | Variable of astItem<identifier> * option<astItem<expr> * astItem<expr>>
              | Negation of astItem<expr>
              | Arithmetic of astItem<expr> * astItem<arithmetic> * astItem<expr>
              | Inversion of astItem<expr>
              | Comparison of astItem<expr> * astItem<comparison> * astItem<expr>
              | Logical of astItem<expr> * astItem<logical> * astItem<expr>
              | Function of astItem<identifier> * astItem<expr> list
              | Branch of astItem<expr> * astItem<expr> * astItem<expr>

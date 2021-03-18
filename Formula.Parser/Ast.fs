//-----------------------------------------------------------------------
// <copyright file="Ast.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module Ast = 

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
              | Constant of value
              | Variable of identifier * option<expr * expr>
              | Negation of expr
              | Arithmetic of expr * arithmetic * expr
              | Inversion of expr
              | Comparison of expr * comparison * expr
              | Logical of expr * logical * expr
              | Function of identifier * expr list
              | Branch of expr * expr * expr

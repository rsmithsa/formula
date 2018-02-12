//-----------------------------------------------------------------------
// <copyright file="Ast.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module Ast = 

    type identifier = Identifier of string
    type value  =
                |Number of float
                |Boolean of bool

    type arithmetic = Add | Subtract | Multiply | Divide | Modulus | Power
    type comparison = Equal | NotEqual | GreaterThan | LessThan | GreaterThanEqual | LessThanEqual
    type logical = And | Or

    type expr   =
                | Constant of value
                | Variable of identifier
                | Negation of expr
                | Arithmetic of expr * arithmetic * expr
                | Inversion of expr
                | Comparison of expr * comparison * expr
                | Logical of expr * logical * expr
                | Function of identifier * expr list
                | Branch of expr * expr * expr

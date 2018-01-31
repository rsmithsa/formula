//-----------------------------------------------------------------------
// <copyright file="Ast.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module Ast = 

    type identifier = Identifier of string
    type arithmetic = Add | Subtract | Multiply | Divide | Power
    type value = Number of double

    type expr   =
                | Constant of value
                | Variable of identifier
                | Negation of expr
                | Arithmetic of expr * arithmetic * expr
                | Function of identifier * expr list

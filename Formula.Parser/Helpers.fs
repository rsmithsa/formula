//-----------------------------------------------------------------------
// <copyright file="Helpers.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

open Formula.Parser.Ast

type Helpers = 

    static member castToBool value =
        match value with
        | Boolean x -> x
        | Number x when x = 0.0 -> false
        | Number x when x <> 0.0 -> true
        | Text x when x.ToLowerInvariant() = "true" -> true
        | Text x when x.ToLowerInvariant() = "false" -> false
        | _ -> invalidOp $"Unable to cast '{value}' to boolean."

    static member castToDouble value =
        match value with
        | Number x -> x
        | Boolean x when x = true -> 1.0
        | Boolean x when x = false -> 0.0
        | Text x -> float(x)
        | _ -> invalidOp $"Unable to cast '{value}' to numeric."

    static member asDoubles values =
        values |> Array.map (
            function
            | Number x -> x
            | _ -> invalidArg "input" "Numeric input expected."
        )

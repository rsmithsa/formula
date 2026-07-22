//-----------------------------------------------------------------------
// <copyright file="Helpers.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

open System
open Formula.Parser.Ast

type Helpers = 

    static member castToBool value =
        match value with
        | Boolean x -> x
        | Number x when x = 0.0 -> false
        | Number x when x <> 0.0 -> true
        | Text x when x.ToLowerInvariant() = "true" -> true
        | Text x when x.ToLowerInvariant() = "false" -> false
        | Nothing -> false
        | ValueArray x -> Helpers.castToBool(x.[0])
        | _ -> invalidOp $"Unable to cast '{value}' to boolean."

    static member castToDouble value =
        match value with
        | Number x -> Some(x)
        | Boolean x when x = true -> Some(1.0)
        | Boolean x when x = false -> Some(0.0)
        | Text x -> Some(float(x))
        | Nothing -> None
        | ValueArray x -> Helpers.castToDouble(x.[0])
        | _ -> invalidOp $"Unable to cast '{value}' to numeric."

    static let flatten vals = seq {
        for v in vals do
            match v with
            | ValueArray x -> yield! x//flatten x
            | x -> yield x
    }
    
    static member flattenValues values =
        flatten values |> Seq.toArray
    
    static member castToNullableDouble (value: value) =
        match Helpers.castToDouble value with
        | Some x -> Nullable(x)
        | _ -> Nullable()
    
    static member castToDoubles values =
        flatten values |> Seq.map Helpers.castToDouble |> Seq.toArray

    static member fsEquality x y =
        x = y

    static member fsInequality x y =
        x <> y

    static member fsLessThanOrEqual x y =
        x <= y

    static member fsGreaterThanOrEqual x y =
        x >= y

    static member fsLessThan x y =
        x < y

    static member fsGreaterThan x y =
        x > y

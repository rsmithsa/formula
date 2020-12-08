//-----------------------------------------------------------------------
// <copyright file="Helpers.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

module Helpers

let castToBool value =
    match value with
    | 0.0 -> 
        false
    | _ ->
        true

let castToDouble value =
    match value with
    | true ->
        1.0
    | false ->
        0.0

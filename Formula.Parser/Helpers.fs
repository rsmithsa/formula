//-----------------------------------------------------------------------
// <copyright file="Helpers.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

open System
open System.Buffers
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
        Option.ofNullable (Helpers.castToNullableDouble value)
   
    static member flattenValues (values: value[]) =
        let pool = ArrayPool<value>.Shared
        let mutable requiredCap = values.Length
        let mutable buffer = pool.Rent(requiredCap)
        let mutable count = 0
        
        try
            for v in values do 
                match v with
                | ValueArray a ->
                    requiredCap <- requiredCap + a.Length
                    if requiredCap > buffer.Length then
                        let newBuffer = pool.Rent(requiredCap)
                        Array.blit buffer 0 newBuffer 0 count
                        pool.Return(buffer)
                        buffer <- newBuffer

                    for x in a do
                        buffer.[count] <- x
                        count <- count + 1
                | x ->
                    buffer.[count] <- x
                    count <- count + 1

            let result = buffer.[0 .. count - 1]
            result
        finally
            pool.Return(buffer)
    
    static member castToNullableDouble (value: value) =
        match value with
        | Number x -> Nullable(x)
        | Boolean x when x = true -> Nullable(1.0)
        | Boolean x when x = false -> Nullable(0.0)
        | Text x -> Nullable(float(x))
        | Nothing -> Nullable()
        | ValueArray x -> Helpers.castToNullableDouble(x.[0])
        | _ -> invalidOp $"Unable to cast '{value}' to numeric."
    
    static member castToDoubles (values: value[]) =
        let pool = ArrayPool<float option>.Shared
        let mutable requiredCap = values.Length
        let mutable buffer = pool.Rent(requiredCap)
        let mutable count = 0

        try
            for v in values do 
                match v with
                | ValueArray a ->
                    requiredCap <- requiredCap + a.Length
                    if requiredCap > buffer.Length then
                        let newBuffer = pool.Rent(requiredCap)
                        Array.blit buffer 0 newBuffer 0 count
                        pool.Return(buffer)
                        buffer <- newBuffer

                    for x in a do
                        buffer.[count] <- (Helpers.castToDouble x)
                        count <- count + 1
                | x ->
                    buffer.[count] <- (Helpers.castToDouble x)
                    count <- count + 1

            let result = buffer.[0 .. count - 1]
            result
        finally
            pool.Return(buffer)

    static member castToNullableDoubles (values: value[]) =
        let pool = ArrayPool<Nullable<float>>.Shared
        let mutable requiredCap = values.Length
        let mutable buffer = pool.Rent(requiredCap)
        let mutable count = 0

        try
            for v in values do 
                match v with
                | ValueArray a ->
                    requiredCap <- requiredCap + a.Length
                    if requiredCap > buffer.Length then
                        let newBuffer = pool.Rent(requiredCap)
                        Array.blit buffer 0 newBuffer 0 count
                        pool.Return(buffer)
                        buffer <- newBuffer

                    for x in a do
                        buffer.[count] <- (Helpers.castToNullableDouble x)
                        count <- count + 1
                | x ->
                    buffer.[count] <- (Helpers.castToNullableDouble x)
                    count <- count + 1

            let result = buffer.[0 .. count - 1]
            result
        finally
            pool.Return(buffer)
    
    static member castFilterToNonNullDoubles (values: value[]) =
        let pool = ArrayPool<float>.Shared
        let mutable requiredCap = values.Length
        let mutable buffer = pool.Rent(requiredCap)
        let mutable count = 0

        try
            for v in values do 
                match v with
                | ValueArray a ->
                    requiredCap <- requiredCap + a.Length
                    if requiredCap > buffer.Length then
                        let newBuffer = pool.Rent(requiredCap)
                        Array.blit buffer 0 newBuffer 0 count
                        pool.Return(buffer)
                        buffer <- newBuffer

                    for x in a do
                        let n = Helpers.castToNullableDouble x
                        if n.HasValue then
                            buffer.[count] <- n.GetValueOrDefault()
                            count <- count + 1
                | x ->
                    let n = Helpers.castToNullableDouble x
                    if n.HasValue then
                        buffer.[count] <- n.GetValueOrDefault()
                        count <- count + 1

            let result = buffer.[0 .. count - 1]
            result
        finally
            pool.Return(buffer)
    
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

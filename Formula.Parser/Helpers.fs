//-----------------------------------------------------------------------
// <copyright file="Helpers.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

open System
open System.Buffers
open Formula.Parser.Ast
open Formula.Parser.WildcardMatcher

/// A computation over a read-only span of doubles. F# lambdas cannot take byref-like
/// parameters, but delegates can, so span consumers are expressed as delegates.
type DoubleSpanFunc<'R> = delegate of ReadOnlySpan<float> -> 'R

/// A computation over a read-only span of doubles with a same length scratch span. F# lambdas cannot take byref-like
/// parameters, but delegates can, so span consumers are expressed as delegates.
type DoubleSpanScratchFunc<'R> = delegate of ReadOnlySpan<float> * Span<float> -> 'R

/// A computation over a mutable (scratch) span of doubles, e.g. for in-place sorting.
type MutableDoubleSpanFunc<'R> = delegate of Span<float> -> 'R

type Helpers =

    /// Inputs flattening to at most this many doubles use a stack buffer; larger ones rent from the pool.
    static member StackThreshold = 128

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
        let length = Helpers.flattenedLength values
        let buffer = Array.zeroCreate<value> length
        let mutable count = 0
        
        for v in values do 
            match v with
            | ValueArray a ->
                for x in a do
                    buffer.[count] <- x
                    count <- count + 1
            | x ->
                buffer.[count] <- x
                count <- count + 1

        buffer
    
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
        let length = Helpers.flattenedLength values
        let buffer = Array.zeroCreate<float option> length
        let mutable count = 0
        
        for v in values do 
            match v with
            | ValueArray a ->
                for x in a do
                    buffer.[count] <- (Helpers.castToDouble x)
                    count <- count + 1
            | x ->
                buffer.[count] <- (Helpers.castToDouble x)
                count <- count + 1

        buffer

    static member castToNullableDoubles (values: value[]) =
        let length = Helpers.flattenedLength values
        let buffer = Array.zeroCreate<Nullable<float>> length
        let mutable count = 0
        
        for v in values do 
            match v with
            | ValueArray a ->
                for x in a do
                    buffer.[count] <- (Helpers.castToNullableDouble x)
                    count <- count + 1
            | x ->
                buffer.[count] <- (Helpers.castToNullableDouble x)
                count <- count + 1

        buffer

    /// Number of elements `values` produces when flattened one level (an exact upper
    /// bound on the number of values it can yield).
    static member flattenedLength (values: value[]) =
        let mutable total = 0
        for v in values do
            match v with
            | ValueArray a -> total <- total + a.Length
            | _ -> total <- total + 1
        total

    /// Flattens `values` one level, casts each element to a double and writes the non-null
    /// ones into `dest`, returning how many were written. `dest` must hold at least
    /// `flattenedLength values` elements.
    static member fillNonNullDoubles (values: value[], dest: Span<float>) =
        let mutable count = 0
        for v in values do
            match v with
            | ValueArray a ->
                for x in a do
                    let n = Helpers.castToNullableDouble x
                    if n.HasValue then
                        dest.[count] <- n.GetValueOrDefault()
                        count <- count + 1
            | x ->
                let n = Helpers.castToNullableDouble x
                if n.HasValue then
                    dest.[count] <- n.GetValueOrDefault()
                    count <- count + 1
        count

    static member withNonNullDoubles (values: value[], f: DoubleSpanFunc<'R>) : 'R =
        let cap = Helpers.flattenedLength values
        if cap = 0 then
            f.Invoke(ReadOnlySpan<float>.Empty)
        elif cap <= Helpers.StackThreshold then
            let span = SpanHelpers.stackalloc<float> cap
            let n = Helpers.fillNonNullDoubles(values, span)
            f.Invoke(span.Slice(0, n))
        else
            let pool = ArrayPool<float>.Shared
            let buffer = pool.Rent(cap)
            try
                let n = Helpers.fillNonNullDoubles(values, Span<float>(buffer))
                f.Invoke(ReadOnlySpan<float>(buffer, 0, n))
            finally
                pool.Return(buffer)

    static member withNonNullDoublesScratch (values: value[], f: DoubleSpanScratchFunc<'R>) : 'R =
        let cap = Helpers.flattenedLength values
        if cap = 0 then
            f.Invoke(ReadOnlySpan<float>.Empty, Span<float>.Empty)
        elif cap <= Helpers.StackThreshold then
            let span = SpanHelpers.stackalloc<float> cap
            let n = Helpers.fillNonNullDoubles(values, span)

            let scratch =
                if n = 0 then Span<float>.Empty
                else SpanHelpers.stackalloc<float> n

            f.Invoke(span.Slice(0, n), scratch)
        else
            let pool = ArrayPool<float>.Shared
            let buffer = pool.Rent(cap)
            let scratchBuffer = pool.Rent(cap)
            try
                let n = Helpers.fillNonNullDoubles(values, Span<float>(buffer))
                let s = Span<float>(scratchBuffer, 0, n)
                f.Invoke(ReadOnlySpan<float>(buffer, 0, n), s)
            finally
                pool.Return(buffer)
                pool.Return(scratchBuffer)

    static member withNonNullDoublesMutable (values: value[], f: MutableDoubleSpanFunc<'R>) : 'R =
        let cap = Helpers.flattenedLength values
        if cap = 0 then
            f.Invoke(Span<float>.Empty)
        elif cap <= Helpers.StackThreshold then
            let span = SpanHelpers.stackalloc<float> cap
            let n = Helpers.fillNonNullDoubles(values, span)
            f.Invoke(span.Slice(0, n))
        else
            let pool = ArrayPool<float>.Shared
            let buffer = pool.Rent(cap)
            try
                let n = Helpers.fillNonNullDoubles(values, Span<float>(buffer))
                f.Invoke(Span<float>(buffer, 0, n))
            finally
                pool.Return(buffer)

    static member castFilterToNonNullDoubles (values: value[]) =
        let pool = ArrayPool<float>.Shared
        let buffer = pool.Rent(Helpers.flattenedLength values)
        try
            let n = Helpers.fillNonNullDoubles(values, Span<float>(buffer))
            buffer.[0 .. n - 1]
        finally
            pool.Return(buffer)

    static member collectWildcardMatches (names: System.Collections.Generic.IEnumerable<string>) (pattern: string) (collector: IVariableNameCollector) =
        for item in names do
            if isMatch (item.AsSpan()) (pattern.AsSpan()) then
                collector.Add item

    static member collectWildcardMatchesFromArray (names: string[]) (pattern: string) (collector: IVariableNameCollector) =
        for i = 0 to names.Length - 1 do
            let item = names.[i]
            if isMatch (item.AsSpan()) (pattern.AsSpan()) then
                collector.Add item

    static member matchNames (provider: IVariableProvider) (pattern: string) : string[] =
        use collector = PooledNameCollector.Rent()
        provider.MatchNames(pattern, collector)
        collector.ToArray()

    static member expandWildcard (provider: IVariableProvider) (pattern: string) : value =
        use names = PooledNameCollector.Rent()
        provider.MatchNames(pattern, names)

        let result = Array.zeroCreate<value> names.Count
        for i = 0 to names.Count - 1 do
            result.[i] <- provider.Lookup(names.[i])
        ValueArray(result)

    static member expandWildcardIndex (provider: IVariableProvider) (pattern: string) (index: value) : value =
        use names = PooledNameCollector.Rent()
        provider.MatchNames(pattern, names)

        let result = Array.zeroCreate<value> names.Count
        for i = 0 to names.Count - 1 do
            result.[i] <- provider.LookupIndex(names.[i], index)
        ValueArray(result)

    static member expandWildcardRange (provider: IVariableProvider) (pattern: string) (lower: value) (upper: value) : value =
        use names = PooledNameCollector.Rent()
        provider.MatchNames(pattern, names)

        let pool = ArrayPool<value>.Shared
        let mutable requiredCap = names.Count
        let mutable buffer = pool.Rent(requiredCap)
        let mutable count = 0

        try
            for i = 0 to names.Count - 1 do
                match provider.LookupRange (names.[i], lower, upper) with
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
                | v ->
                    buffer.[count] <- v
                    count <- count + 1

            let result = buffer.[0 .. count - 1]
            ValueArray(result)
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

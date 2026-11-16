//-----------------------------------------------------------------------
// <copyright file="PooledNameCollector.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

open System
open System.Buffers

/// An append-only `IVariableNameCollector` backed by a pooled array. Names are kept exactly
/// as the provider adds them, duplicates included; removing duplicates is the provider's
/// choice (see `DistinctNameCollector`).
///
/// Obtain one with `PooledNameCollector.Rent()` and dispose it exactly once when done. Each
/// thread reuses a cached instance, falling back to a fresh one when the cached instance is
/// already in use (wildcard expansion is re-entrant: evaluating an expression variable can
/// expand another wildcard).
///
/// Internal so callers outside the library cannot misuse the reuse protocol (dispose twice,
/// read after dispose); providers only ever see it as an `IVariableNameCollector`.
[<Sealed; AllowNullLiteral>]
type internal PooledNameCollector() =
    static let initialCapacity = 16

    [<ThreadStatic; DefaultValue>]
    static val mutable private cached: PooledNameCollector

    let mutable buffer: string[] = null
    let mutable count = 0
    let mutable inUse = false

    static member Rent() =
        let c = PooledNameCollector.cached
        if not (isNull c) && not c.InUse then
            c.InUse <- true
            c
        else
            let fresh = new PooledNameCollector()
            fresh.InUse <- true
            if isNull c then PooledNameCollector.cached <- fresh
            fresh

    member private this.InUse
        with get () = inUse
        and set v = inUse <- v

    member this.Count = count

    member this.Item with get (i: int) = buffer.[i]

    member this.AsSpan() : ReadOnlySpan<string> =
        if count = 0 then ReadOnlySpan<string>.Empty else ReadOnlySpan<string>(buffer, 0, count)

    member this.ToArray() : string[] = this.AsSpan().ToArray()

    member this.Add (name: string) =
        if isNull buffer then
            buffer <- ArrayPool<string>.Shared.Rent(initialCapacity)
        elif count = buffer.Length then
            let grown = ArrayPool<string>.Shared.Rent(buffer.Length * 2)
            Array.blit buffer 0 grown 0 count
            ArrayPool<string>.Shared.Return(buffer)
            buffer <- grown
        buffer.[count] <- name
        count <- count + 1

    member this.Dispose() =
        if not (isNull buffer) then
            ArrayPool<string>.Shared.Return(buffer)
            buffer <- null
        count <- 0
        inUse <- false

    interface IVariableNameCollector with
        member this.Add name = this.Add name

    interface IDisposable with
        member this.Dispose() = this.Dispose()

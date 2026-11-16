//-----------------------------------------------------------------------
// <copyright file="DistinctNameCollector.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

open System
open System.Collections.Generic

/// Wraps an `IVariableNameCollector` and forwards each name only the first time it is seen
/// (ordinal comparison), so order follows first occurrence. For providers that merge names
/// from several sources, e.g. a composite, and want each name reported once:
///
///     let distinct = DistinctNameCollector(collector)
///     for p in providers do p.MatchNames(pattern, distinct)
///
/// Use an instance only for the duration of the `MatchNames` call that received `inner`.
[<Sealed; AllowNullLiteral>]
type DistinctNameCollector(inner: IVariableNameCollector) =
    static let retainedSetLimit = 1024

    [<ThreadStatic; DefaultValue>]
    static val mutable private cached: DistinctNameCollector

    let mutable inner = inner
    let mutable seen: HashSet<string> = null
    let mutable count = 0
    let mutable inUse = false

    static member internal Rent(target: IVariableNameCollector) =
        let c = DistinctNameCollector.cached
        let d =
            if not (isNull c) && not c.InUse then c
            else
                let fresh = DistinctNameCollector(null)
                if isNull c then DistinctNameCollector.cached <- fresh
                fresh
        d.InUse <- true
        d.Inner <- target
        d

    member private this.InUse with get () = inUse and set v = inUse <- v
    member private this.Inner with set v = inner <- v

    member this.Add (name: string) =
        if isNull seen then seen <- HashSet<string>(StringComparer.Ordinal)
        if seen.Add name then
            count <- count + 1
            inner.Add name

    member internal this.Release() =
        if not (isNull seen) then
            if count > retainedSetLimit then seen <- null else seen.Clear()
        count <- 0
        inner <- null
        inUse <- false

    interface IVariableNameCollector with
        member this.Add name = this.Add name

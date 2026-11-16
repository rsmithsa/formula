//-----------------------------------------------------------------------
// <copyright file="IVariableProvider.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

open Formula.Parser.Ast

/// Receives the variable names matched by a wildcard pattern. Providers push each match
/// into the collector rather than returning an array; the collector owns the storage and
/// keeps every name added, in order. A provider that merges several sources and wants each
/// name reported once can wrap the collector in a `DistinctNameCollector`.
///
/// Usage contract for `IVariableProvider.MatchNames` implementations:
/// - Only call `Add` during the `MatchNames` call that received the collector. Do not store
///   the collector or use it after returning: collectors are pooled and reused, so a late
///   `Add` would leak names into an unrelated wildcard expansion.
/// - Call `Add` from the calling thread only; collectors are not thread-safe.
/// - Forward the same collector (or a `DistinctNameCollector` wrapping it) to any inner
///   providers (e.g. composites) rather than collecting into a separate list.
[<AllowNullLiteral>]
type IVariableNameCollector =
    abstract member Add: name: string -> unit

[<AllowNullLiteral>]
type IVariableProvider =
    abstract member IsDefined: name: string -> bool
    abstract member IsDefined: name: string * sender: IVariableProvider -> bool
    abstract member Lookup: name: string -> value
    abstract member Lookup: name: string * sender: IVariableProvider -> value
    abstract member LookupRange: name: string * lower: value * upper: value -> value
    abstract member LookupRange: name: string * lower: value * upper: value * sender: IVariableProvider -> value
    abstract member LookupIndex: name: string * index: value -> value
    abstract member LookupIndex: name: string * index: value * sender: IVariableProvider -> value
    abstract member MatchNames: pattern: string * collector: IVariableNameCollector -> unit
    abstract member MatchNames: pattern: string * collector: IVariableNameCollector * sender: IVariableProvider -> unit
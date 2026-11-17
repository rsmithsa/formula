//-----------------------------------------------------------------------
// <copyright file="SpanHelpers.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

#nowarn "9" // NativePtr.stackalloc is unverifiable IL

open System
open FSharp.NativeInterop

type SpanHelpers =
    static member inline stackalloc<'a when 'a: unmanaged> (length: int): Span<'a> =
        let p = NativePtr.stackalloc<'a> length |> NativePtr.toVoidPtr
        Span<'a>(p, length)

    static member inline toReadOnlySpan (span: Span<'T>) : ReadOnlySpan<'T> =
        Span.op_Implicit(span)
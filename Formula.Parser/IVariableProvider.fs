//-----------------------------------------------------------------------
// <copyright file="IVariableProvider.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

open System.Runtime.InteropServices

[<AllowNullLiteral>]
type IVariableProvider =
    abstract member IsDefined: name: string -> bool
    abstract member IsDefined: name: string * sender: IVariableProvider -> bool
    abstract member Lookup: name: string -> float
    abstract member Lookup: name: string * sender: IVariableProvider -> float
//-----------------------------------------------------------------------
// <copyright file="IVariableProvider.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

open System.Runtime.InteropServices

type IVariableProvider =
    abstract member IsDefined: name: string -> bool
    abstract member Lookup: name: string -> float
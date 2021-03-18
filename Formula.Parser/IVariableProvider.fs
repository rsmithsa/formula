//-----------------------------------------------------------------------
// <copyright file="IVariableProvider.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

open Formula.Parser.Ast

[<AllowNullLiteral>]
type IVariableProvider =
    abstract member IsDefined: name: string -> bool
    abstract member IsDefined: name: string * sender: IVariableProvider -> bool
    abstract member Lookup: name: string -> value
    abstract member Lookup: name: string * sender: IVariableProvider -> value
    abstract member LookupRange: name: string * lower: value * upper: value -> value[]
    abstract member LookupRange: name: string * lower: value * upper: value * sender: IVariableProvider -> value[]
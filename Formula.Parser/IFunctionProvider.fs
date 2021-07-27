//-----------------------------------------------------------------------
// <copyright file="IFunctionProvider.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

open System.Runtime.InteropServices

open Formula.Parser.Ast

type IFunctionImplementation =
    abstract member Name: string
    abstract member Execute: input: value[] -> value
    abstract member Validate: input: value[] * [<Out>] message: string byref -> bool

type IFunctionProvider =
    abstract member KnownFunctions: seq<string>
    abstract member IsDefined: name: string -> bool
    abstract member Lookup: name: string -> IFunctionImplementation

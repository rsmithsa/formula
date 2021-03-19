//-----------------------------------------------------------------------
// <copyright file="ParserException.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

open System
open FParsec

type ParserException =
    inherit ArgumentException

    val private parserError: ParserError

    new() = { inherit ArgumentException(); parserError = Unchecked.defaultof<ParserError>; }
    new(message: string) = { inherit ArgumentException(message); parserError = Unchecked.defaultof<ParserError>; }
    new(message: string, paramName: string) = { inherit ArgumentException(message, paramName); parserError = Unchecked.defaultof<ParserError>; }
    new(message: string, parserError: ParserError) = { inherit ArgumentException(message); parserError = parserError; }

    member x.ParserError: ParserError = x.parserError

//-----------------------------------------------------------------------
// <copyright file="MutableVariableProvider.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open Formula.Parser
open Formula.Parser.Ast

type MutableVariableProvider(map: System.Collections.Generic.IDictionary<string, double>) =
    
    member this.KnownVariables: System.Collections.Generic.IDictionary<string, double> = map

    member this.IsDefined name = 
        this.KnownVariables.ContainsKey name
    member this.Lookup name =
        this.KnownVariables.[name]

    interface IVariableProvider with 
        member this.IsDefined (name) = this.IsDefined name
        member this.IsDefined (name, sender) = this.IsDefined name
        member this.Lookup (name) = Number(this.Lookup name)
        member this.Lookup (name, sender) = Number(this.Lookup name)
        member this.LookupRange (name, range) = [| Number(this.Lookup name) |]
        member this.LookupRange (name, range, sender) = [| Number(this.Lookup name) |]


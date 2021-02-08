//-----------------------------------------------------------------------
// <copyright file="MutableVariableProvider.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open Formula.Parser

type MutableVariableProvider(map: System.Collections.Generic.IDictionary<string, double>) =
    
    member this.KnownVariables: System.Collections.Generic.IDictionary<string, double> = map

    member this.IsDefined name = 
        this.KnownVariables.ContainsKey name
    member this.Lookup name =
        this.KnownVariables.[name]

    interface IVariableProvider with 
        member this.IsDefined (name) = this.IsDefined name
        member this.IsDefined (name, sender) = this.IsDefined name
        member this.Lookup (name) = this.Lookup name
        member this.Lookup (name, sender) = this.Lookup name


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
        Number(this.KnownVariables.[name])
    member this.LookupRange name lower upper =
        let value = this.KnownVariables.[name]
        match (lower, upper) with
        | (Number a, Number b) -> Array.init (int(b - a) + 1) (fun x -> Number(value))
        | _ -> invalidArg "range" "Numeric range expected."

    interface IVariableProvider with 
        member this.IsDefined (name) = this.IsDefined name
        member this.IsDefined (name, sender) = this.IsDefined name
        member this.Lookup (name) = this.Lookup name
        member this.Lookup (name, sender) = this.Lookup name
        member this.LookupRange (name, lower, upper) = this.LookupRange name lower upper
        member this.LookupRange (name, lower, upper, sender) = this.LookupRange name lower upper


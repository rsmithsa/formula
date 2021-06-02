//-----------------------------------------------------------------------
// <copyright file="MapVariableProvider.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open Formula.Parser
open Formula.Parser.Ast

type MapVariableProvider(map: Map<string, float>) =

    static let empty = MapVariableProvider(Map.empty)

    static let dictionaryToMap (dictionary : System.Collections.Generic.IDictionary<_, _>) = 
        dictionary 
        |> Seq.map (|KeyValue|)  
        |> Map.ofSeq
    
    static member Empty = empty

    new(variables: System.Collections.Generic.IDictionary<string, double>) = MapVariableProvider(dictionaryToMap variables)

    member this.KnownVariables: Map<string, float> = map

    member this.IsDefined name = 
        this.KnownVariables.ContainsKey name
    member this.Lookup name =
        Number(this.KnownVariables.[name])
    member this.LookupRange name lower upper =
        let value = this.KnownVariables.[name]
        match (lower, upper) with
        | (Number a, Number b) -> Array.init (int(b - a) + 1) (fun x -> Number(value))
        | _ -> invalidArg "range" "Numeric range expected."
    member this.LookupIndex name index =
        this.Lookup name

    interface IVariableProvider with 
        member this.IsDefined (name) = this.IsDefined name
        member this.IsDefined (name, sender) = this.IsDefined name
        member this.Lookup (name) = this.Lookup name
        member this.Lookup (name, sender) = this.Lookup name
        member this.LookupRange (name, lower, upper) = this.LookupRange name lower upper
        member this.LookupRange (name, lower, upper, sender) = this.LookupRange name lower upper
        member this.LookupIndex (name, index) = this.LookupIndex name index
        member this.LookupIndex (name, index, sender) = this.LookupIndex name index


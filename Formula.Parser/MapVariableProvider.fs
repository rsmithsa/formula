//-----------------------------------------------------------------------
// <copyright file="MapVariableProvider.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open Formula.Parser

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
        this.KnownVariables.[name]

    interface IVariableProvider with 
        member this.IsDefined name = this.IsDefined name
        member this.Lookup name = this.Lookup name


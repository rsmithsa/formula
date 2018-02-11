//-----------------------------------------------------------------------
// <copyright file="MapVariableProvider.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open Formula.Parser

type MapVariableProvider(map) =

    member this.KnownVariables: Map<string, float> = map

    interface IVariableProvider with 
        member this.IsDefined name = 
            this.KnownVariables.ContainsKey name
        member this.Lookup name =
            this.KnownVariables.[name]


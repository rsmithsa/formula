//-----------------------------------------------------------------------
// <copyright file="CompositeFunctionProvider.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open Formula.Parser

type CompositeFunctionProvider(providers: seq<IFunctionProvider>) =

    let knownFunctions:  Map<string, IFunctionProvider> =
        providers
        |> Seq.map (fun x -> x.KnownFunctions |> Seq.map (fun y -> (y, x)))
        |> Seq.concat
        |> Map.ofSeq

    member this.KnownFunctions =
        knownFunctions |> Map.toSeq |> Seq.map fst
    member this.IsDefined name = 
        knownFunctions.ContainsKey name
    member this.Lookup name =
        knownFunctions.[name].Lookup name

    interface IFunctionProvider with 
        member this.KnownFunctions = this.KnownFunctions
        member this.IsDefined name = this.IsDefined name
        member this.Lookup name = this.Lookup name

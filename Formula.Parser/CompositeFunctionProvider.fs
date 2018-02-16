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

    interface IFunctionProvider with 
        member this.KnownFunctions =
            knownFunctions |> Map.toSeq |> Seq.map fst
        member this.IsDefined name = 
            knownFunctions.ContainsKey name
        member this.Lookup name =
            knownFunctions.[name].Lookup name
//-----------------------------------------------------------------------
// <copyright file="CompositeVariableProvider.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open Formula.Parser

type CompositeVariableProvider(providers: seq<IVariableProvider>) =

    let knownVariables: Lazy<IVariableProvider[]> =
        lazy(providers |> Seq.toArray)

    new(providerFactories: seq<IVariableProvider -> IVariableProvider>) as this =
        let providers =
            providerFactories
            |> Seq.map (fun x -> x(this))

        CompositeVariableProvider(providers)

    new(providerFactories: seq<System.Func<IVariableProvider, IVariableProvider>>) as this =
        let providers =
            providerFactories
            |> Seq.map (fun x -> x.Invoke(this))

        CompositeVariableProvider(providers)

    member this.IsDefined name sender =
        knownVariables.Force()
        |> Seq.where (fun x -> LanguagePrimitives.PhysicalEquality x sender = false)
        |> Seq.exists (fun x -> x.IsDefined name)
    member this.Lookup name sender =
        let toSearch =
            knownVariables.Force()
            |> Seq.where (fun x -> LanguagePrimitives.PhysicalEquality x sender = false)
            |> Seq.toArray
        let provider =
            toSearch
            |> Seq.find (fun x -> x.IsDefined name)
        provider.Lookup name
    member this.LookupRange name lower upper sender =
        let toSearch =
            knownVariables.Force()
            |> Seq.where (fun x -> LanguagePrimitives.PhysicalEquality x sender = false)
            |> Seq.toArray
        let provider =
            toSearch
            |> Seq.find (fun x -> x.IsDefined name)
        provider.LookupRange (name, lower, upper)
    member this.LookupIndex name index sender =
        let toSearch =
            knownVariables.Force()
            |> Seq.where (fun x -> LanguagePrimitives.PhysicalEquality x sender = false)
            |> Seq.toArray
        let provider =
            toSearch
            |> Seq.find (fun x -> x.IsDefined name)
        provider.LookupIndex (name, index)
    member this.MatchNames (pattern: string) (collector: IVariableNameCollector) sender =
        let providers = knownVariables.Force()
        let mutable sources = 0
        for x in providers do
            if not (LanguagePrimitives.PhysicalEquality x sender) then sources <- sources + 1

        if sources = 1 then
            for x in providers do
                if not (LanguagePrimitives.PhysicalEquality x sender) then
                    x.MatchNames (pattern, collector)
        elif sources > 1 then
            let distinct = DistinctNameCollector.Rent collector
            try
                for x in providers do
                    if not (LanguagePrimitives.PhysicalEquality x sender) then
                        x.MatchNames (pattern, distinct)
            finally
                distinct.Release()

    interface IVariableProvider with
        member this.IsDefined (name) = this.IsDefined name null
        member this.IsDefined (name, sender) = this.IsDefined name sender
        member this.Lookup (name) = this.Lookup name null
        member this.Lookup (name, sender) = this.Lookup name sender
        member this.LookupRange (name, lower, upper) = this.LookupRange name lower upper null
        member this.LookupRange (name, lower, upper, sender) = this.LookupRange name lower upper sender
        member this.LookupIndex (name, index) = this.LookupIndex name index null
        member this.LookupIndex (name, index, sender) = this.LookupIndex name index sender
        member this.MatchNames (pattern, collector) = this.MatchNames pattern collector null
        member this.MatchNames (pattern, collector, sender) = this.MatchNames pattern collector sender
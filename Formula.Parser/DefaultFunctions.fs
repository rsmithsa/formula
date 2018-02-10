//-----------------------------------------------------------------------
// <copyright file="DefaultFunctions.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Integration

open Formula.Parser

type SqrtFunction() =
    interface IFunctionImplementation with
        member this.Name =
            "SQRT"
        member this.Execute input =
            sqrt input.[0]
        member this.Validate (input, message) =
            true

type PiFunction() =
    interface IFunctionImplementation with
        member this.Name =
            "PI"
        member this.Execute input =
            System.Math.PI
        member this.Validate (input, message) =
            true

type PowFunction() =
    interface IFunctionImplementation with
        member this.Name =
            "POW"
        member this.Execute input =
            input.[0] ** input.[1]
        member this.Validate (input, message) =
            true

type ModFunction() =
    interface IFunctionImplementation with
        member this.Name =
            "MOD"
        member this.Execute input =
            input.[0] % input.[1]
        member this.Validate (input, message) =
            true

type CountFunction() =
    interface IFunctionImplementation with
        member this.Name =
            "COUNT"
        member this.Execute input =
            float(input.Length)
        member this.Validate (input, message) =
            true

type DefaultFunctionProvider() =

    static let instance = DefaultFunctionProvider() :> IFunctionProvider

    let knownFunctions: Map<string, IFunctionImplementation> =
        Map.empty.
            Add("SQRT", SqrtFunction() :> IFunctionImplementation).
            Add("PI", PiFunction() :> IFunctionImplementation).
            Add("POW", PowFunction() :> IFunctionImplementation).
            Add("MOD", ModFunction() :> IFunctionImplementation).
            Add("COUNT", CountFunction() :> IFunctionImplementation)

    static member Instance = instance

    interface IFunctionProvider with 
        member this.IsDefined name = 
            knownFunctions.ContainsKey name
        member this.Lookup name =
            knownFunctions.[name]

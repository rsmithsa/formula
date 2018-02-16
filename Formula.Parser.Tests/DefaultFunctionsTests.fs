//-----------------------------------------------------------------------
// <copyright file="DefaultFunctionsTests.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Formula.Parser
open Formula.Parser.Integration

[<TestClass>]
type DefaultFunctionsTests () =

    [<TestMethod>]
    member this.TestDefaultFunctionList () =
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "SQRT")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "PI")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "POW")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "MOD")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "COUNT")

    [<TestMethod>]
    member this.TestDefaultFunctionSqrt () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "SQRT"
        Assert.AreEqual("SQRT", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [4.0]))
        Assert.AreEqual((false, "SQRT expects one argument."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((false, "SQRT expects one argument."), functionImplementation.Validate (List.toArray [4.0; 4.0]))
        Assert.AreEqual((false, "SQRT expects one argument."), functionImplementation.Validate (null))

        Assert.AreEqual(2.0, functionImplementation.Execute (List.toArray [4.0]))
        Assert.AreEqual(3.0, functionImplementation.Execute (List.toArray [9.0]))
        Assert.AreEqual(4.0, functionImplementation.Execute (List.toArray [16.0]))

        let rand = Random(42)
        let a = rand.NextDouble();
        Assert.AreEqual(sqrt a, functionImplementation.Execute (List.toArray [a]))

    [<TestMethod>]
    member this.TestDefaultFunctionPi () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "PI"
        Assert.AreEqual("PI", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (null))
        Assert.AreEqual((false, "PI expects no arguments."), functionImplementation.Validate (List.toArray [4.0]))
        Assert.AreEqual((false, "PI expects no arguments."), functionImplementation.Validate (List.toArray [4.0; 4.0]))

        Assert.AreEqual(Math.PI, functionImplementation.Execute (List.toArray []))

    [<TestMethod>]
    member this.TestDefaultFunctionPow () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "POW"
        Assert.AreEqual("POW", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [2.0; 1.0]))
        Assert.AreEqual((false, "POW expects two arguments."), functionImplementation.Validate (List.toArray [2.0]))
        Assert.AreEqual((false, "POW expects two arguments."), functionImplementation.Validate (List.toArray [2.0; 1.0; 1.0]))
        Assert.AreEqual((false, "POW expects two arguments."), functionImplementation.Validate (null))

        Assert.AreEqual(2.0, functionImplementation.Execute (List.toArray [2.0; 1.0]))
        Assert.AreEqual(4.0, functionImplementation.Execute (List.toArray [2.0; 2.0]))
        Assert.AreEqual(8.0, functionImplementation.Execute (List.toArray [2.0; 3.0]))

        let rand = Random(42)
        let a = rand.NextDouble();
        let b = rand.NextDouble();
        Assert.AreEqual(a ** b, functionImplementation.Execute (List.toArray [a; b]))

    [<TestMethod>]
    member this.TestDefaultFunctionMod () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "MOD"
        Assert.AreEqual("MOD", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [2.0; 1.0]))
        Assert.AreEqual((false, "MOD expects two arguments."), functionImplementation.Validate (List.toArray [2.0]))
        Assert.AreEqual((false, "MOD expects two arguments."), functionImplementation.Validate (List.toArray [2.0; 1.0; 1.0]))
        Assert.AreEqual((false, "MOD expects two arguments."), functionImplementation.Validate (null))

        Assert.AreEqual(0.0, functionImplementation.Execute (List.toArray [2.0; 1.0]))
        Assert.AreEqual(0.0, functionImplementation.Execute (List.toArray [2.0; 2.0]))
        Assert.AreEqual(2.0, functionImplementation.Execute (List.toArray [2.0; 3.0]))

        let rand = Random(42)
        let a = rand.NextDouble();
        let b = rand.NextDouble();
        Assert.AreEqual(a % b, functionImplementation.Execute (List.toArray [a; b]))

    [<TestMethod>]
    member this.TestDefaultFunctionCount () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "COUNT"
        Assert.AreEqual("COUNT", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (null))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [1.0; 1.0]))

        Assert.AreEqual(0.0, functionImplementation.Execute (null))
        Assert.AreEqual(0.0, functionImplementation.Execute (List.toArray []))
        Assert.AreEqual(1.0, functionImplementation.Execute (List.toArray [1.0]))
        Assert.AreEqual(2.0, functionImplementation.Execute (List.toArray [1.0; 1.0]))

        let rand = Random(42)
        let a = rand.Next();
        Assert.AreEqual(float a, functionImplementation.Execute (Array.zeroCreate a))

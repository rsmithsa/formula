//-----------------------------------------------------------------------
// <copyright file="FinancialFunctionsTests.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Formula.Parser
open Formula.Parser.Integration

[<TestClass>]
type FinancialFunctionsTests () =

    [<TestMethod>]
    member this.TestFinancialFunctionList () =
        Assert.IsTrue(FinancialFunctionProvider.Instance.IsDefined "DDB")
        Assert.IsTrue(FinancialFunctionProvider.Instance.IsDefined "FV")
        Assert.IsTrue(FinancialFunctionProvider.Instance.IsDefined "IPMT")
        Assert.IsTrue(FinancialFunctionProvider.Instance.IsDefined "IRR")
        Assert.IsTrue(FinancialFunctionProvider.Instance.IsDefined "MIRR")
        Assert.IsTrue(FinancialFunctionProvider.Instance.IsDefined "NPER")
        Assert.IsTrue(FinancialFunctionProvider.Instance.IsDefined "NPV")
        Assert.IsTrue(FinancialFunctionProvider.Instance.IsDefined "PMT")
        Assert.IsTrue(FinancialFunctionProvider.Instance.IsDefined "PPMT")
        Assert.IsTrue(FinancialFunctionProvider.Instance.IsDefined "PV")
        Assert.IsTrue(FinancialFunctionProvider.Instance.IsDefined "RATE")
        Assert.IsTrue(FinancialFunctionProvider.Instance.IsDefined "SLN")
        Assert.IsTrue(FinancialFunctionProvider.Instance.IsDefined "SYD")

    //[<TestMethod>]
    //member this.TestFinancialFunctionDdb () =
    //    let functionImplementation = FinancialFunctionProvider.Instance.Lookup "DDB"
    //    Assert.AreEqual("DDB", functionImplementation.Name)

    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [20000.0; 8000.0; 5.0; 1.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [20000.0; 8000.0; 5.0; 1.0; 2.0]))
    //    Assert.AreEqual((false, "DDB expects four or five arguments."), functionImplementation.Validate (List.toArray []))
    //    Assert.AreEqual((false, "DDB expects four or five arguments."), functionImplementation.Validate (List.toArray [20000.0; 8000.0; 5.0]))
    //    Assert.AreEqual((false, "DDB expects four or five arguments."), functionImplementation.Validate (null))

    //    Assert.AreEqual(8000.0, functionImplementation.Execute (List.toArray [20000.0; 8000.0; 5.0; 1.0]), 0.000001)
    //    Assert.AreEqual(4000.0, functionImplementation.Execute (List.toArray [20000.0; 8000.0; 5.0; 2.0; 2.0]), 0.000001)
    //    Assert.AreEqual(0.0, functionImplementation.Execute (List.toArray [20000.0; 8000.0; 5.0; 5.0; 2.0]), 0.000001)

    //    Assert.AreEqual(2400.0, functionImplementation.Execute (List.toArray [10000.0; 2000.0; 5.0; 2.0; 2.0]), 0.000001)
    //    Assert.AreEqual(160.0, functionImplementation.Execute (List.toArray [10000.0; 2000.0; 5.0; 4.0; 2.0]), 0.000001)

    //[<TestMethod>]
    //member this.TestFinancialFunctionFv () =
    //    let functionImplementation = FinancialFunctionProvider.Instance.Lookup "FV"
    //    Assert.AreEqual("FV", functionImplementation.Name)

    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 10.0; 1000.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 10.0; 1000.0; 1000.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 10.0; 1000.0; 1000.0; 0.0]))
    //    Assert.AreEqual((false, "FV expects three, four or five arguments."), functionImplementation.Validate (List.toArray []))
    //    Assert.AreEqual((false, "FV expects three, four or five arguments."), functionImplementation.Validate (List.toArray [0.1; 10.0;]))
    //    Assert.AreEqual((false, "FV expects three, four or five arguments."), functionImplementation.Validate (null))

    //    Assert.AreEqual(-15937.424601, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0]), 0.000001)
    //    Assert.AreEqual(-17531.167061, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0; 0.0; 1.0]), 0.000001)
    //    Assert.AreEqual(-18531.167061, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0; 1000.0]), 0.000001)
    //    Assert.AreEqual(-20124.909521, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0; 1000.0; 1.0]), 0.000001)

    //[<TestMethod>]
    //member this.TestFinancialFunctionIpmt () =
    //    let functionImplementation = FinancialFunctionProvider.Instance.Lookup "IPMT"
    //    Assert.AreEqual("IPMT", functionImplementation.Name)

    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 3.0; 10.0; 1000.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 3.0; 10.0; 1000.0; 1000.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 3.0; 10.0; 1000.0; 1000.0; 0.0]))
    //    Assert.AreEqual((false, "IPMT expects four, five or six arguments."), functionImplementation.Validate (List.toArray []))
    //    Assert.AreEqual((false, "IPMT expects four, five or six arguments."), functionImplementation.Validate (List.toArray [0.1; 10.0;]))
    //    Assert.AreEqual((false, "IPMT expects four, five or six arguments."), functionImplementation.Validate (null))

    //    Assert.AreEqual(-86.823467, functionImplementation.Execute (List.toArray [0.1; 3.0; 10.0; 1000.0]), 0.000001)
    //    Assert.AreEqual(-78.930425, functionImplementation.Execute (List.toArray [0.1; 3.0; 10.0; 1000.0; 0.0; 1.0]), 0.000001)
    //    Assert.AreEqual(-73.646934, functionImplementation.Execute (List.toArray [0.1; 3.0; 10.0; 1000.0; 1000.0]), 0.000001)
    //    Assert.AreEqual(-66.951758, functionImplementation.Execute (List.toArray [0.1; 3.0; 10.0; 1000.0; 1000.0; 1.0]), 0.000001)

    //[<TestMethod>]
    //member this.TestFinancialFunctionIrr () =
    //    let functionImplementation = FinancialFunctionProvider.Instance.Lookup "IRR"
    //    Assert.AreEqual("IRR", functionImplementation.Name)

    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; -100000.0; 105000.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; -100000.0; 105000.0; 5000.0]))
    //    Assert.AreEqual((false, "IRR expects at least three arguments."), functionImplementation.Validate (List.toArray []))
    //    Assert.AreEqual((false, "IRR expects at least three arguments."), functionImplementation.Validate (List.toArray [0.1]))
    //    Assert.AreEqual((false, "IRR expects at least three arguments."), functionImplementation.Validate (List.toArray [0.1; -100000.0]))
    //    Assert.AreEqual((false, "IRR expects at least three arguments."), functionImplementation.Validate (null))

    //    Assert.AreEqual(0.05, functionImplementation.Execute (List.toArray [0.1; -100000.0; 105000.0]), 0.000001)
    //    Assert.AreEqual(0.095635, functionImplementation.Execute (List.toArray [0.1; -100000.0; 105000.0; 5000.0]), 0.000001)

    //[<TestMethod>]
    //member this.TestFinancialFunctionMirr () =
    //    let functionImplementation = FinancialFunctionProvider.Instance.Lookup "MIRR"
    //    Assert.AreEqual("MIRR", functionImplementation.Name)

    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 0.1; -100000.0; 105000.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 0.1; -100000.0; 105000.0; 5000.0]))
    //    Assert.AreEqual((false, "MIRR expects at least four arguments."), functionImplementation.Validate (List.toArray []))
    //    Assert.AreEqual((false, "MIRR expects at least four arguments."), functionImplementation.Validate (List.toArray [0.1]))
    //    Assert.AreEqual((false, "MIRR expects at least four arguments."), functionImplementation.Validate (List.toArray [0.1; 0.1]))
    //    Assert.AreEqual((false, "MIRR expects at least four arguments."), functionImplementation.Validate (List.toArray [0.1; 0.1; -100000.0]))
    //    Assert.AreEqual((false, "MIRR expects at least four arguments."), functionImplementation.Validate (null))

    //    Assert.AreEqual(0.05, functionImplementation.Execute (List.toArray [0.1; 0.1; -100000.0; 105000.0]), 0.000001)
    //    Assert.AreEqual(0.097724, functionImplementation.Execute (List.toArray [0.1; 0.1; -100000.0; 105000.0; 5000.0]), 0.000001)
    //    Assert.AreEqual(0.179085, functionImplementation.Execute (List.toArray [0.1; 0.12; -1000.0; -4000.0; 5000.0; 2000.0]), 0.000001)

    //[<TestMethod>]
    //member this.TestFinancialFunctionNper () =
    //    let functionImplementation = FinancialFunctionProvider.Instance.Lookup "NPER"
    //    Assert.AreEqual("NPER", functionImplementation.Name)

    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 10.0; 1000.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 10.0; 1000.0; 1000.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 10.0; 1000.0; 1000.0; 0.0]))
    //    Assert.AreEqual((false, "NPER expects three, four or five arguments."), functionImplementation.Validate (List.toArray []))
    //    Assert.AreEqual((false, "NPER expects three, four or five arguments."), functionImplementation.Validate (List.toArray [0.1; 10.0;]))
    //    Assert.AreEqual((false, "NPER expects three, four or five arguments."), functionImplementation.Validate (null))

    //    Assert.AreEqual(-25.158858, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0]), 0.000001)
    //    Assert.AreEqual(-24.253809, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0; 0.0; 1.0]), 0.000001)
    //    Assert.AreEqual(-17.886317, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0; -100.0]), 0.000001)
    //    Assert.AreEqual(-17.469359, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0; -100.0; 1.0]), 0.000001)
    //    Assert.AreEqual(-100.0, functionImplementation.Execute (List.toArray [0.0; 10.0; 1000.0]), 0.000001)

    //[<TestMethod>]
    //member this.TestFinancialFunctionNpv () =
    //    let functionImplementation = FinancialFunctionProvider.Instance.Lookup "NPV"
    //    Assert.AreEqual("NPV", functionImplementation.Name)

    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; -100000.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; -100000.0; 10000.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; -100000.0; 10000.0; 10000.0]))
    //    Assert.AreEqual((false, "NPV expects at least two arguments."), functionImplementation.Validate (List.toArray []))
    //    Assert.AreEqual((false, "NPV expects at least two arguments."), functionImplementation.Validate (List.toArray [0.1]))
    //    Assert.AreEqual((false, "NPV expects at least two arguments."), functionImplementation.Validate (null))

    //    Assert.AreEqual(-90909.090909, functionImplementation.Execute (List.toArray [0.1; -100000.0]), 0.000001)
    //    Assert.AreEqual(-82644.628099, functionImplementation.Execute (List.toArray [0.1; -100000.0; 10000.0]), 0.000001)
    //    Assert.AreEqual(-75131.480090, functionImplementation.Execute (List.toArray [0.1; -100000.0; 10000.0; 10000.0]), 0.000001)

    //[<TestMethod>]
    //member this.TestFinancialFunctionPmt () =
    //    let functionImplementation = FinancialFunctionProvider.Instance.Lookup "PMT"
    //    Assert.AreEqual("PMT", functionImplementation.Name)

    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 10.0; 1000.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 10.0; 1000.0; 1000.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 10.0; 1000.0; 1000.0; 0.0]))
    //    Assert.AreEqual((false, "PMT expects three, four or five arguments."), functionImplementation.Validate (List.toArray []))
    //    Assert.AreEqual((false, "PMT expects three, four or five arguments."), functionImplementation.Validate (List.toArray [0.1; 10.0;]))
    //    Assert.AreEqual((false, "PMT expects three, four or five arguments."), functionImplementation.Validate (null))

    //    Assert.AreEqual(-162.745395, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0]), 0.000001)
    //    Assert.AreEqual(-147.950359, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0; 0.0; 1.0]), 0.000001)
    //    Assert.AreEqual(-225.490790, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0; 1000.0]), 0.000001)
    //    Assert.AreEqual(-204.991627, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0; 1000.0; 1.0]), 0.000001)

    //[<TestMethod>]
    //member this.TestFinancialFunctionPpmt () =
    //    let functionImplementation = FinancialFunctionProvider.Instance.Lookup "PPMT"
    //    Assert.AreEqual("PPMT", functionImplementation.Name)

    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 3.0; 10.0; 1000.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 3.0; 10.0; 1000.0; 1000.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 3.0; 10.0; 1000.0; 1000.0; 0.0]))
    //    Assert.AreEqual((false, "PPMT expects four, five or six arguments."), functionImplementation.Validate (List.toArray []))
    //    Assert.AreEqual((false, "PPMT expects four, five or six arguments."), functionImplementation.Validate (List.toArray [0.1; 10.0;]))
    //    Assert.AreEqual((false, "PPMT expects four, five or six arguments."), functionImplementation.Validate (null))

    //    Assert.AreEqual(-75.921928, functionImplementation.Execute (List.toArray [0.1; 3.0; 10.0; 1000.0]), 0.000001)
    //    Assert.AreEqual(-69.019934, functionImplementation.Execute (List.toArray [0.1; 3.0; 10.0; 1000.0; 0.0; 1.0]), 0.000001)
    //    Assert.AreEqual(-151.843856, functionImplementation.Execute (List.toArray [0.1; 3.0; 10.0; 1000.0; 1000.0]), 0.000001)
    //    Assert.AreEqual(-138.039869, functionImplementation.Execute (List.toArray [0.1; 3.0; 10.0; 1000.0; 1000.0; 1.0]), 0.000001)

    //[<TestMethod>]
    //member this.TestFinancialFunctionPv () =
    //    let functionImplementation = FinancialFunctionProvider.Instance.Lookup "PV"
    //    Assert.AreEqual("PV", functionImplementation.Name)

    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 10.0; 1000.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 10.0; 1000.0; 1000.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 10.0; 1000.0; 1000.0; 0.0]))
    //    Assert.AreEqual((false, "PV expects three, four or five arguments."), functionImplementation.Validate (List.toArray []))
    //    Assert.AreEqual((false, "PV expects three, four or five arguments."), functionImplementation.Validate (List.toArray [0.1; 10.0;]))
    //    Assert.AreEqual((false, "PV expects three, four or five arguments."), functionImplementation.Validate (null))

    //    Assert.AreEqual(-6144.567105, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0]), 0.000001)
    //    Assert.AreEqual(-6759.023816, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0; 0.0; 1.0]), 0.000001)
    //    Assert.AreEqual(-6530.110395, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0; 1000.0]), 0.000001)
    //    Assert.AreEqual(-7144.567105, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0; 1000.0; 1.0]), 0.000001)

    //[<TestMethod>]
    //member this.TestFinancialFunctionRate () =
    //    let functionImplementation = FinancialFunctionProvider.Instance.Lookup "RATE"
    //    Assert.AreEqual("RATE", functionImplementation.Name)

    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [10.0; -100.0; 800.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [10.0; -100.0; 800.0; 100.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [10.0; -100.0; 800.0; 100.0; 1.0]))
    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [10.0; -100.0; 800.0; 100.0; 1.0; 0.2]))
    //    Assert.AreEqual((false, "RATE expects three, four, five or six arguments."), functionImplementation.Validate (List.toArray []))
    //    Assert.AreEqual((false, "RATE expects three, four, five or six arguments."), functionImplementation.Validate (List.toArray [10.0; -100.0;]))
    //    Assert.AreEqual((false, "RATE expects three, four, five or six arguments."), functionImplementation.Validate (null))

    //    Assert.AreEqual(0.042775, functionImplementation.Execute (List.toArray [10.0; -100.0; 800.0]), 0.000001)
    //    Assert.AreEqual(0.024227, functionImplementation.Execute (List.toArray [10.0; -100.0; 800.0; 100.0]), 0.000001)
    //    Assert.AreEqual(0.024227, functionImplementation.Execute (List.toArray [10.0; -100.0; 800.0; 100.0; 0.0]), 0.000001)
    //    Assert.AreEqual(0.031451, functionImplementation.Execute (List.toArray [10.0; -100.0; 800.0; 100.0; 1.0]), 0.000001)
    //    Assert.AreEqual(0.024227, functionImplementation.Execute (List.toArray [10.0; -100.0; 800.0; 100.0; 0.0; 0.2]), 0.000001)
    //    Assert.AreEqual(0.031451, functionImplementation.Execute (List.toArray [10.0; -100.0; 800.0; 100.0; 1.0; 0.2]), 0.000001)

    //[<TestMethod>]
    //member this.TestFinancialFunctionSln () =
    //    let functionImplementation = FinancialFunctionProvider.Instance.Lookup "SLN"
    //    Assert.AreEqual("SLN", functionImplementation.Name)

    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [10000.0; 1000.0; 5.0]))
    //    Assert.AreEqual((false, "SLN expects three arguments."), functionImplementation.Validate (List.toArray []))
    //    Assert.AreEqual((false, "SLN expects three arguments."), functionImplementation.Validate (List.toArray [10000.0; 1000.0]))
    //    Assert.AreEqual((false, "SLN expects three arguments."), functionImplementation.Validate (null))

    //    Assert.AreEqual(9000.0 / 5.0, functionImplementation.Execute (List.toArray [10000.0; 1000.0; 5.0]))

    //    let rand = Random(42)
    //    let cost = rand.NextDouble();
    //    let salvage = rand.NextDouble();
    //    let life = rand.NextDouble();
    //    Assert.AreEqual((cost- salvage) / life, functionImplementation.Execute (List.toArray [cost; salvage; life]))

    //[<TestMethod>]
    //member this.TestFinancialFunctionSyd () =
    //    let functionImplementation = FinancialFunctionProvider.Instance.Lookup "SYD"
    //    Assert.AreEqual("SYD", functionImplementation.Name)

    //    Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [10000.0; 1000.0; 5.0; 1.0]))
    //    Assert.AreEqual((false, "SYD expects four arguments."), functionImplementation.Validate (List.toArray []))
    //    Assert.AreEqual((false, "SYD expects four arguments."), functionImplementation.Validate (List.toArray [10000.0; 1000.0; 5.0]))
    //    Assert.AreEqual((false, "SYD expects four arguments."), functionImplementation.Validate (null))

    //    Assert.AreEqual(3000.0, functionImplementation.Execute (List.toArray [10000.0; 1000.0; 5.0; 1.0]))
    //    Assert.AreEqual(2400.0, functionImplementation.Execute (List.toArray [10000.0; 1000.0; 5.0; 2.0]))
    //    Assert.AreEqual(1800.0, functionImplementation.Execute (List.toArray [10000.0; 1000.0; 5.0; 3.0]))

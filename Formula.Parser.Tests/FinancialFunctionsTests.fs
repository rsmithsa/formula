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

    [<TestMethod>]
    member this.TestFinancialFunctionDdb () =
        let functionImplementation = FinancialFunctionProvider.Instance.Lookup "DDB"
        Assert.AreEqual("DDB", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [20000.0; 8000.0; 5.0; 1.0]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [20000.0; 8000.0; 5.0; 1.0; 2.0]))
        Assert.AreEqual((false, "DDB expects four or five arguments."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((false, "DDB expects four or five arguments."), functionImplementation.Validate (List.toArray [20000.0; 8000.0; 5.0]))
        Assert.AreEqual((false, "DDB expects four or five arguments."), functionImplementation.Validate (null))

        Assert.AreEqual(8000.0, functionImplementation.Execute (List.toArray [20000.0; 8000.0; 5.0; 1.0]), 0.000001)
        Assert.AreEqual(4000.0, functionImplementation.Execute (List.toArray [20000.0; 8000.0; 5.0; 2.0; 2.0]), 0.000001)
        Assert.AreEqual(0.0, functionImplementation.Execute (List.toArray [20000.0; 8000.0; 5.0; 5.0; 2.0]), 0.000001)

        Assert.AreEqual(2400.0, functionImplementation.Execute (List.toArray [10000.0; 2000.0; 5.0; 2.0; 2.0]), 0.000001)
        Assert.AreEqual(160.0, functionImplementation.Execute (List.toArray [10000.0; 2000.0; 5.0; 4.0; 2.0]), 0.000001)

    [<TestMethod>]
    member this.TestFinancialFunctionFv () =
        let functionImplementation = FinancialFunctionProvider.Instance.Lookup "FV"
        Assert.AreEqual("FV", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 10.0; 1000.0]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 10.0; 1000.0; 1000.0]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 10.0; 1000.0; 1000.0; 0.0]))
        Assert.AreEqual((false, "FV expects three, four or five arguments."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((false, "FV expects three, four or five arguments."), functionImplementation.Validate (List.toArray [0.1; 10.0;]))
        Assert.AreEqual((false, "FV expects three, four or five arguments."), functionImplementation.Validate (null))

        Assert.AreEqual(-15937.424601, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0]), 0.000001)
        Assert.AreEqual(-17531.167061, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0; 0.0; 1.0]), 0.000001)
        Assert.AreEqual(-18531.167061, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0; 1000.0]), 0.000001)
        Assert.AreEqual(-20124.909521, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0; 1000.0; 1.0]), 0.000001)

    [<TestMethod>]
    member this.TestFinancialFunctionNpv () =
        let functionImplementation = FinancialFunctionProvider.Instance.Lookup "NPV"
        Assert.AreEqual("NPV", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; -100000.0]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; -100000.0; 10000.0]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; -100000.0; 10000.0; 10000.0]))
        Assert.AreEqual((false, "NPV expects at least two arguments."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((false, "NPV expects at least two arguments."), functionImplementation.Validate (List.toArray [0.1]))
        Assert.AreEqual((false, "NPV expects at least two arguments."), functionImplementation.Validate (null))

        Assert.AreEqual(-90909.090909, functionImplementation.Execute (List.toArray [0.1; -100000.0]), 0.000001)
        Assert.AreEqual(-82644.628099, functionImplementation.Execute (List.toArray [0.1; -100000.0; 10000.0]), 0.000001)
        Assert.AreEqual(-75131.480090, functionImplementation.Execute (List.toArray [0.1; -100000.0; 10000.0; 10000.0]), 0.000001)

    [<TestMethod>]
    member this.TestFinancialFunctionPv () =
        let functionImplementation = FinancialFunctionProvider.Instance.Lookup "PV"
        Assert.AreEqual("PV", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 10.0; 1000.0]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 10.0; 1000.0; 1000.0]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [0.1; 10.0; 1000.0; 1000.0; 0.0]))
        Assert.AreEqual((false, "PV expects three, four or five arguments."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((false, "PV expects three, four or five arguments."), functionImplementation.Validate (List.toArray [0.1; 10.0;]))
        Assert.AreEqual((false, "PV expects three, four or five arguments."), functionImplementation.Validate (null))

        Assert.AreEqual(-6144.567105, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0]), 0.000001)
        Assert.AreEqual(-6759.023816, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0; 0.0; 1.0]), 0.000001)
        Assert.AreEqual(-6530.110395, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0; 1000.0]), 0.000001)
        Assert.AreEqual(-7144.567105, functionImplementation.Execute (List.toArray [0.1; 10.0; 1000.0; 1000.0; 1.0]), 0.000001)

    [<TestMethod>]
    member this.TestFinancialFunctionSln () =
        let functionImplementation = FinancialFunctionProvider.Instance.Lookup "SLN"
        Assert.AreEqual("SLN", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [10000.0; 1000.0; 5.0]))
        Assert.AreEqual((false, "SLN expects three arguments."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((false, "SLN expects three arguments."), functionImplementation.Validate (List.toArray [10000.0; 1000.0]))
        Assert.AreEqual((false, "SLN expects three arguments."), functionImplementation.Validate (null))

        Assert.AreEqual(9000.0 / 5.0, functionImplementation.Execute (List.toArray [10000.0; 1000.0; 5.0]))

        let rand = Random(42)
        let cost = rand.NextDouble();
        let salvage = rand.NextDouble();
        let life = rand.NextDouble();
        Assert.AreEqual((cost- salvage) / life, functionImplementation.Execute (List.toArray [cost; salvage; life]))

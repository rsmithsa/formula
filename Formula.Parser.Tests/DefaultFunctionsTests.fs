//-----------------------------------------------------------------------
// <copyright file="DefaultFunctionsTests.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Formula.Parser.Ast
open Formula.Parser.Integration

[<TestClass>]
type DefaultFunctionsTests () =

    [<TestMethod>]
    member this.TestDefaultFunctionList () =
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "ABS")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "SQRT")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "PI")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "POW")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "MOD")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "COUNT")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "SUM")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "AVG")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "FIRST")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "LAST")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "MIN")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "MAX")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "COALESCE")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "IFNULL")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "DIV")
        Assert.IsTrue(DefaultFunctionProvider.Instance.IsDefined "SUMPRODUCT")
        
        Assert.AreEqual(16, DefaultFunctionProvider.Instance.KnownFunctions |> Seq.length)

    [<TestMethod>]
    member this.TestDefaultFunctionAbs () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "ABS"
        Assert.AreEqual("ABS", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(4.0)]))
        Assert.AreEqual((false, "ABS expects one argument."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((false, "ABS expects one argument."), functionImplementation.Validate (List.toArray [Number(4.0); Number(4.0)]))
        Assert.AreEqual((false, "ABS expects one argument."), functionImplementation.Validate (null))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [ValueArray([| Number(4.0) |])]))
        Assert.AreEqual((false, "ABS expects one argument."), functionImplementation.Validate (List.toArray [ValueArray([| Number(4.0); Number(4.0) |])]))

        Assert.AreEqual(Number(5.0), functionImplementation.Execute (List.toArray [Number(5.0)]))
        Assert.AreEqual(Number(5.0), functionImplementation.Execute (List.toArray [Number(-5.0)]))
        Assert.AreEqual(Number(0.0), functionImplementation.Execute (List.toArray [Number(0.0)]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Nothing]))

    [<TestMethod>]
    member this.TestDefaultFunctionSqrt () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "SQRT"
        Assert.AreEqual("SQRT", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(4.0)]))
        Assert.AreEqual((false, "SQRT expects one argument."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((false, "SQRT expects one argument."), functionImplementation.Validate (List.toArray [Number(4.0); Number(4.0)]))
        Assert.AreEqual((false, "SQRT expects one argument."), functionImplementation.Validate (null))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [ValueArray([| Number(4.0) |])]))
        Assert.AreEqual((false, "SQRT expects one argument."), functionImplementation.Validate (List.toArray [ValueArray([| Number(4.0); Number(4.0) |])]))

        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [Number(4.0)]))
        Assert.AreEqual(Number(3.0), functionImplementation.Execute (List.toArray [Number(9.0)]))
        Assert.AreEqual(Number(4.0), functionImplementation.Execute (List.toArray [Number(16.0)]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Nothing]))

        let rand = Random(42)
        let a = rand.NextDouble();
        Assert.AreEqual(Number(sqrt a), functionImplementation.Execute (List.toArray [Number(a)]))

    [<TestMethod>]
    member this.TestDefaultFunctionPi () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "PI"
        Assert.AreEqual("PI", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (null))
        Assert.AreEqual((false, "PI expects no arguments."), functionImplementation.Validate (List.toArray [Number(4.0)]))
        Assert.AreEqual((false, "PI expects no arguments."), functionImplementation.Validate (List.toArray [Number(4.0); Number(4.0)]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [ValueArray([||])]))
        Assert.AreEqual((false, "PI expects no arguments."), functionImplementation.Validate (List.toArray [ValueArray([| Number(4.0) |])]))

        Assert.AreEqual(Number(Math.PI), functionImplementation.Execute (List.toArray []))

    [<TestMethod>]
    member this.TestDefaultFunctionPow () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "POW"
        Assert.AreEqual("POW", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(2.0); Number(1.0)]))
        Assert.AreEqual((false, "POW expects two arguments."), functionImplementation.Validate (List.toArray [Number(2.0)]))
        Assert.AreEqual((false, "POW expects two arguments."), functionImplementation.Validate (List.toArray [Number(2.0); Number(1.0); Number(1.0)]))
        Assert.AreEqual((false, "POW expects two arguments."), functionImplementation.Validate (null))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [ValueArray([| Number(2.0); Number(1.0) |])]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(2.0); ValueArray([| Number(1.0) |])]))
        Assert.AreEqual((false, "POW expects two arguments."), functionImplementation.Validate (List.toArray [ValueArray([| Number(2.0); Number(1.0); Number(1.0) |])]))

        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(1.0)]))
        Assert.AreEqual(Number(4.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(2.0)]))
        Assert.AreEqual(Number(8.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(3.0)]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Number(2.0); Nothing]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Nothing; Number(2.0)]))

        let rand = Random(42)
        let a = rand.NextDouble();
        let b = rand.NextDouble();
        Assert.AreEqual(Number(a ** b), functionImplementation.Execute (List.toArray [Number(a); Number(b)]))

    [<TestMethod>]
    member this.TestDefaultFunctionMod () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "MOD"
        Assert.AreEqual("MOD", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(2.0); Number(1.0)]))
        Assert.AreEqual((false, "MOD expects two arguments."), functionImplementation.Validate (List.toArray [Number(2.0)]))
        Assert.AreEqual((false, "MOD expects two arguments."), functionImplementation.Validate (List.toArray [Number(2.0); Number(1.0); Number(1.0)]))
        Assert.AreEqual((false, "MOD expects two arguments."), functionImplementation.Validate (null))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [ValueArray([| Number(2.0); Number(1.0) |])]))
        Assert.AreEqual((false, "MOD expects two arguments."), functionImplementation.Validate (List.toArray [ValueArray([| Number(2.0); Number(1.0); Number(1.0) |])]))

        Assert.AreEqual(Number(0.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(1.0)]))
        Assert.AreEqual(Number(0.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(2.0)]))
        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(3.0)]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Number(2.0); Nothing]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Nothing; Number(2.0)]))

        let rand = Random(42)
        let a = rand.NextDouble();
        let b = rand.NextDouble();
        Assert.AreEqual(Number(a % b), functionImplementation.Execute (List.toArray [Number(a); Number(b)]))

    [<TestMethod>]
    member this.TestDefaultFunctionCount () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "COUNT"
        Assert.AreEqual("COUNT", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (null))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(1.0); Number(1.0)]))

        Assert.AreEqual(Number(0.0), functionImplementation.Execute (null))
        Assert.AreEqual(Number(0.0), functionImplementation.Execute (List.toArray []))
        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [Number(1.0)]))
        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [Number(1.0); Number(2.0)]))
        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [Number(2.0); Nothing]))
        Assert.AreEqual(Number(0.0), functionImplementation.Execute (List.toArray [Nothing; Nothing]))
        Assert.AreEqual(Number(4.0), functionImplementation.Execute (List.toArray [Number(1.0); Number(2.0); ValueArray([| Number(1.0); Number(2.0) |])]))

        let rand = Random(42)
        let a = rand.Next(500);
        Assert.AreEqual(Number(float a), functionImplementation.Execute (Array.init a (fun x -> Number(2.0))))

    [<TestMethod>]
    member this.TestDefaultFunctionSum () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "SUM"
        Assert.AreEqual("SUM", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (null))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(1.0); Number(1.0)]))

        Assert.AreEqual(Number(0.0), functionImplementation.Execute (null))
        Assert.AreEqual(Number(0.0), functionImplementation.Execute (List.toArray []))
        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [Number(1.0)]))
        Assert.AreEqual(Number(3.0), functionImplementation.Execute (List.toArray [Number(1.0); Number(2.0)]))
        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [Number(2.0); Nothing]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Nothing; Nothing]))
        Assert.AreEqual(Number(6.0), functionImplementation.Execute (List.toArray [Number(1.0); Number(2.0); ValueArray([| Number(1.0); Number(2.0) |])]))

        let rand = Random(42)
        let a = rand.Next(500);
        Assert.AreEqual(Number(float a * 2.0), functionImplementation.Execute (Array.init a (fun x -> Number(2.0))))

    [<TestMethod>]
    member this.TestDefaultFunctionAvg () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "AVG"
        Assert.AreEqual("AVG", functionImplementation.Name)

        Assert.AreEqual((false, "AVG expects at least one argument."), functionImplementation.Validate (null))
        Assert.AreEqual((false, "AVG expects at least one argument."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(1.0)]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(1.0); Number(1.0)]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [ValueArray([| Number(1.0) |])]))
        Assert.AreEqual((false, "AVG expects at least one argument."), functionImplementation.Validate (List.toArray [ValueArray([||])]))

        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [Number(1.0)]))
        Assert.AreEqual(Number(1.5), functionImplementation.Execute (List.toArray [Number(1.0); Number(2.0)]))
        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [Number(2.0); Nothing]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Nothing; Nothing]))
        Assert.AreEqual(Number(1.5), functionImplementation.Execute (List.toArray [Number(1.0); Number(2.0); ValueArray([| Number(1.0); Number(2.0) |])]))

        let rand = Random(42)
        let a = rand.Next(500);
        Assert.AreEqual(Number(2.0), functionImplementation.Execute (Array.init a (fun x -> Number(2.0))))

    [<TestMethod>]
    member this.TestDefaultFunctionFirst () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "FIRST"
        Assert.AreEqual("FIRST", functionImplementation.Name)

        Assert.AreEqual((false, "FIRST expects at least one argument."), functionImplementation.Validate (null))
        Assert.AreEqual((false, "FIRST expects at least one argument."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(1.0)]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(1.0); Number(1.0)]))
        // FIRST is not flattened for validation: a single (even empty) array argument counts as one argument
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [ValueArray([||])]))

        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [Number(1.0)]))
        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [Number(1.0); Number(2.0)]))
        Assert.AreEqual(Boolean(false), functionImplementation.Execute (List.toArray [Boolean(false); Number(2.0)]))
        Assert.AreEqual(Text("123"), functionImplementation.Execute (List.toArray [Text("123"); Number(2.0)]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Nothing; Number(2.0)]))
        Assert.AreEqual(ValueArray([| Number(1.0); Number(2.0) |]), functionImplementation.Execute (List.toArray [ValueArray([| Number(1.0); Number(2.0) |]); Number(1.0); Number(2.0)]))
        
    [<TestMethod>]
    member this.TestDefaultFunctionLast () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "LAST"
        Assert.AreEqual("LAST", functionImplementation.Name)

        Assert.AreEqual((false, "LAST expects at least one argument."), functionImplementation.Validate (null))
        Assert.AreEqual((false, "LAST expects at least one argument."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(1.0)]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(1.0); Number(1.0)]))
        // LAST is not flattened for validation: a single (even empty) array argument counts as one argument
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [ValueArray([||])]))

        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [Number(1.0)]))
        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [Number(1.0); Number(2.0)]))
        Assert.AreEqual(Boolean(false), functionImplementation.Execute (List.toArray [Number(2.0); Boolean(false)]))
        Assert.AreEqual(Text("123"), functionImplementation.Execute (List.toArray [Number(2.0); Text("123")]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Number(2.0); Nothing]))
        Assert.AreEqual(ValueArray([| Number(1.0); Number(3.0) |]), functionImplementation.Execute (List.toArray [Number(1.0); Number(2.0); ValueArray([| Number(1.0); Number(3.0) |])]))
        
    [<TestMethod>]
    member this.TestDefaultFunctionMin () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "MIN"
        Assert.AreEqual("MIN", functionImplementation.Name)

        Assert.AreEqual((false, "MIN expects at least one argument."), functionImplementation.Validate (null))
        Assert.AreEqual((false, "MIN expects at least one argument."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(1.0)]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(1.0); Number(1.0)]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [ValueArray([| Number(1.0) |])]))
        Assert.AreEqual((false, "MIN expects at least one argument."), functionImplementation.Validate (List.toArray [ValueArray([||])]))

        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [Number(1.0)]))
        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [Number(1.0); Number(2.0)]))
        Assert.AreEqual(Number(0.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(0.0)]))
        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(123.0)]))
        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [Number(2.0); Nothing]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Nothing; Nothing]))
        Assert.AreEqual(Number(0.0), functionImplementation.Execute (List.toArray [Number(1.0); Number(2.0); ValueArray([| Number(2.0); Number(0.0) |])]))
        
    [<TestMethod>]
    member this.TestDefaultFunctionMax () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "MAX"
        Assert.AreEqual("MAX", functionImplementation.Name)

        Assert.AreEqual((false, "MAX expects at least one argument."), functionImplementation.Validate (null))
        Assert.AreEqual((false, "MAX expects at least one argument."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(1.0)]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(1.0); Number(1.0)]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [ValueArray([| Number(1.0) |])]))
        Assert.AreEqual((false, "MAX expects at least one argument."), functionImplementation.Validate (List.toArray [ValueArray([||])]))

        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [Number(1.0)]))
        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [Number(1.0); Number(2.0)]))
        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(0.0)]))
        Assert.AreEqual(Number(123.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(123.0)]))
        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [Number(2.0); Nothing]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Nothing; Nothing]))
        Assert.AreEqual(Number(3.0), functionImplementation.Execute (List.toArray [Number(1.0); Number(2.0); ValueArray([| Number(0.0); Number(3.0) |])]))
        
    [<TestMethod>]
    member this.TestDefaultFunctionCoalesce () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "COALESCE"
        Assert.AreEqual("COALESCE", functionImplementation.Name)

        Assert.AreEqual((false, "COALESCE expects at least two arguments."), functionImplementation.Validate (null))
        Assert.AreEqual((false, "COALESCE expects at least two arguments."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((false, "COALESCE expects at least two arguments."), functionImplementation.Validate (List.toArray [Number(1.0)]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(1.0); Number(1.0)]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(1.0); Number(1.0); Number(1.0)]))
        Assert.AreEqual((false, "COALESCE expects at least two arguments."), functionImplementation.Validate (List.toArray [ValueArray([| Number(1.0) |])]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [ValueArray([| Number(1.0); Number(1.0) |])]))

        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [Number(1.0); Number(2.0)]))
        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [Nothing; Number(2.0)]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Nothing; Nothing]))
        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [Number(1.0); Nothing; Nothing]))
        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [Nothing; Number(2.0); Number(3.0)]))
        Assert.AreEqual(Number(3.0), functionImplementation.Execute (List.toArray [Nothing; Nothing; Number(3.0)]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Nothing; Nothing; Nothing]))
        Assert.AreEqual(Number(3.0), functionImplementation.Execute (List.toArray [Nothing; ValueArray([| Nothing; Number(3.0) |])]))
        
    [<TestMethod>]
    member this.TestDefaultFunctionIfNull () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "IFNULL"
        Assert.AreEqual("IFNULL", functionImplementation.Name)

        Assert.AreEqual((false, "IFNULL expects two arguments."), functionImplementation.Validate (null))
        Assert.AreEqual((false, "IFNULL expects two arguments."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((false, "IFNULL expects two arguments."), functionImplementation.Validate (List.toArray [Number(1.0)]))
        Assert.AreEqual((false, "IFNULL expects two arguments."), functionImplementation.Validate (List.toArray [Number(1.0); Number(1.0); Number(1.0)]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(1.0); Number(1.0)]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [ValueArray([| Number(1.0); Number(1.0) |])]))
        Assert.AreEqual((false, "IFNULL expects two arguments."), functionImplementation.Validate (List.toArray [ValueArray([| Number(1.0) |])]))

        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [Number(1.0); Number(2.0)]))
        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [Number(1.0); Nothing]))
        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [Nothing; Number(2.0)]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Nothing; Nothing]))
        Assert.AreEqual(Number(3.0), functionImplementation.Execute (List.toArray [ValueArray([| Nothing; Number(3.0) |])]))
        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [ValueArray([| Number(1.0); Number(3.0) |])]))

    [<TestMethod>]
    member this.TestDefaultFunctionDiv () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "DIV"
        Assert.AreEqual("DIV", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(2.0); Number(1.0)]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(2.0); Number(1.0); Nothing]))
        Assert.AreEqual((false, "DIV expects two or three arguments."), functionImplementation.Validate (List.toArray [Number(2.0)]))
        Assert.AreEqual((false, "DIV expects two or three arguments."), functionImplementation.Validate (List.toArray [Number(2.0); Number(1.0); Number(1.0); Number(1.0)]))
        Assert.AreEqual((false, "DIV expects two or three arguments."), functionImplementation.Validate (null))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [ValueArray([| Number(2.0); Number(1.0) |])]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [ValueArray([| Number(2.0); Number(1.0); Nothing |])]))
        Assert.AreEqual((false, "DIV expects two or three arguments."), functionImplementation.Validate (List.toArray [ValueArray([| Number(2.0); Number(1.0); Number(1.0); Number(1.0) |])]))

        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(1.0)]))
        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(2.0)]))
        Assert.AreEqual(Number(0.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(0.0)]))
        Assert.AreEqual(Number(0.0), functionImplementation.Execute (List.toArray [Number(2.0); Nothing]))
        Assert.AreEqual(Number(0.0), functionImplementation.Execute (List.toArray [Nothing; Number(2.0)]))
        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [ValueArray([| Number(2.0); Number(1.0) |])]))
        Assert.AreEqual(Number(0.0), functionImplementation.Execute (List.toArray [ValueArray([| Number(2.0); Nothing |])]))
        Assert.AreEqual(Number(0.0), functionImplementation.Execute (List.toArray [ValueArray([| Nothing; Number(2.0) |])]))

        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(1.0); Boolean(true)]))
        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(2.0); Boolean(true)]))
        Assert.AreEqual(Boolean(true), functionImplementation.Execute (List.toArray [Number(2.0); Number(0.0); Boolean(true)]))
        Assert.AreEqual(Boolean(true), functionImplementation.Execute (List.toArray [Number(2.0); Nothing; Boolean(true)]))
        Assert.AreEqual(Boolean(true), functionImplementation.Execute (List.toArray [Nothing; Number(2.0); Boolean(true)]))
        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [ValueArray([| Number(2.0); Number(1.0); Boolean(true) |])]))
        Assert.AreEqual(Boolean(true), functionImplementation.Execute (List.toArray [ValueArray([| Number(2.0); Nothing; Boolean(true) |])]))
        Assert.AreEqual(Boolean(true), functionImplementation.Execute (List.toArray [ValueArray([| Nothing; Number(2.0); Boolean(true) |])]))
        
        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(1.0); Nothing]))
        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(2.0); Nothing]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Number(2.0); Number(0.0); Nothing]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Number(2.0); Nothing; Nothing]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Nothing; Number(2.0); Nothing]))
        Assert.AreEqual(Number(2.0), functionImplementation.Execute (List.toArray [ValueArray([| Number(2.0); Number(1.0); Boolean(true) |])]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [ValueArray([| Number(2.0); Nothing; Nothing |])]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [ValueArray([| Nothing; Number(2.0); Nothing |])]))
        
        let rand = Random(42)
        let a = rand.NextDouble();
        let b = rand.NextDouble();
        Assert.AreEqual(Number(a / b), functionImplementation.Execute (List.toArray [Number(a); Number(b)]))
        
    [<TestMethod>]
    member this.TestDefaultFunctionSumProduct () =
        let functionImplementation = DefaultFunctionProvider.Instance.Lookup "SUMPRODUCT"
        Assert.AreEqual("SUMPRODUCT", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(1.0); Number(1.0)]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Number(2.0); Number(1.0); Number(2.0); Number(1.0)]))
        Assert.AreEqual((false, "SUMPRODUCT expects an even non-zero number of arguments."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((false, "SUMPRODUCT expects an even non-zero number of arguments."), functionImplementation.Validate (List.toArray [Number(2.0)]))
        Assert.AreEqual((false, "SUMPRODUCT expects an even non-zero number of arguments."), functionImplementation.Validate (List.toArray [Number(2.0); Number(1.0); Number(1.0)]))
        Assert.AreEqual((false, "SUMPRODUCT expects an even non-zero number of arguments."), functionImplementation.Validate (null))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [ValueArray([| Number(1.0); Number(1.0) |])]))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [ValueArray([| Number(2.0); Number(1.0) |]); ValueArray([| Number(2.0); Number(1.0) |])]))
        Assert.AreEqual((false, "SUMPRODUCT expects an even non-zero number of arguments."), functionImplementation.Validate (List.toArray [ValueArray([| Number(2.0); Number(1.0); Number(1.0) |])]))

        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [Number(1.0); Number(1.0)]))
        Assert.AreEqual(Number(5.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(1.0); Number(2.0); Number(1.0)]))
        Assert.AreEqual(Number(3.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(3.0); Nothing; Number(1.0)]))
        Assert.AreEqual(Nothing, functionImplementation.Execute (List.toArray [Nothing; Nothing; Nothing; Nothing]))

        Assert.AreEqual(Number(1.0), functionImplementation.Execute (List.toArray [ValueArray([| Number(1.0); Number(1.0) |])]))
        Assert.AreEqual(Number(5.0), functionImplementation.Execute (List.toArray [ValueArray([| Number(2.0); Number(1.0) |]); Number(2.0); Number(1.0)]))
        Assert.AreEqual(Number(3.0), functionImplementation.Execute (List.toArray [Number(2.0); Number(3.0); ValueArray([| Nothing; Number(1.0) |])]))

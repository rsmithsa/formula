//-----------------------------------------------------------------------
// <copyright file="ArrayFunctionsTests.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Formula.Parser.Ast
open Formula.Parser.Integration

[<TestClass>]
type ArrayFunctionsTests () =

    [<TestMethod>]
    member this.TestFilterFunctionList () =
        Assert.IsTrue(ArrayFunctionProvider.Instance.IsDefined "FILTER")
        Assert.IsTrue(ArrayFunctionProvider.Instance.IsDefined "VMULT")
        
        Assert.AreEqual(2, ArrayFunctionProvider.Instance.KnownFunctions |> Seq.length)

    [<TestMethod>]
    member this.TestFilterFunctionFilter () =
        let functionImplementation = ArrayFunctionProvider.Instance.Lookup "FILTER"
        Assert.AreEqual("FILTER", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Text(">"); Number(4.0)]))
        Assert.AreEqual((false, "FILTER expects at least two arguments."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((false, "FILTER expects at least two arguments."), functionImplementation.Validate (List.toArray [Number(4.0)]))
        Assert.AreEqual((false, "FILTER expects at least two arguments."), functionImplementation.Validate (null))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [ValueArray([| Number(4.0); Number(4.0) |])]))
        Assert.AreEqual((false, "FILTER expects at least two arguments."), functionImplementation.Validate (List.toArray [ValueArray([| Text(">") |])]))

        Assert.AreEqual(ValueArray([| Number(7.0) |]), functionImplementation.Execute (List.toArray [Text(">"); Number(5.0); Number(5.0); Number(1.0); ValueArray([| Number(0.0); Number(1.0); Number(7.0) |])]))
        Assert.AreEqual(ValueArray([| Number(1.0); Number(0.0); Number(1.0) |]), functionImplementation.Execute (List.toArray [Text("<"); Number(5.0); Number(5.0); Number(1.0); ValueArray([| Number(0.0);  Number(1.0); Number(7.0) |])]))
        Assert.AreEqual(ValueArray([| Number(5.0); Number(7.0) |]), functionImplementation.Execute (List.toArray [Text(">="); Number(5.0); Number(5.0); Number(1.0); ValueArray([| Number(0.0); Number(1.0); Number(7.0) |])]))
        Assert.AreEqual(ValueArray([| Number(5.0); Number(1.0); Number(0.0); Number(1.0) |]), functionImplementation.Execute (List.toArray [Text("<="); Number(5.0); Number(5.0); Number(1.0); ValueArray([| Number(0.0);  Number(1.0); Number(7.0) |])]))
        Assert.AreEqual(ValueArray([| Number(5.0) |]), functionImplementation.Execute (List.toArray [Text("="); Number(5.0); Number(5.0); Number(1.0); ValueArray([| Number(0.0); Number(1.0); Number(7.0) |])]))
        Assert.AreEqual(ValueArray([| Number(1.0); Number(0.0); Number(1.0); Number(7.0) |]), functionImplementation.Execute (List.toArray [Text("<>"); Number(5.0); Number(5.0); Number(1.0); ValueArray([| Number(0.0);  Number(1.0); Number(7.0) |])]))

        Assert.AreEqual(ValueArray([| Number(5.0); Number(1.0); Number(0.0); Number(1.0); Number(7.0) |]), functionImplementation.Execute (List.toArray [Text(""); Number(5.0); Number(5.0); Number(1.0); ValueArray([| Number(0.0);  Number(1.0); Number(7.0) |])]))
        Assert.AreEqual(ValueArray([| Number(5.0); Number(1.0); Number(0.0); Number(1.0); Number(7.0) |]), functionImplementation.Execute (List.toArray [Text("nonsense"); Number(5.0); Number(5.0); Number(1.0); ValueArray([| Number(0.0);  Number(1.0); Number(7.0) |])]))

        Assert.AreEqual(ValueArray([| |]), functionImplementation.Execute (List.toArray [Text("nonsense"); Number(5.0) ]))
        
    [<TestMethod>]
    member this.TestFilterFunctionQuery () =
        let functionImplementation = ArrayFunctionProvider.Instance.Lookup "VMULT"
        Assert.AreEqual("VMULT", functionImplementation.Name)

        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [Text(">"); Number(4.0)]))
        Assert.AreEqual((false, "VMULT expects at least two arguments."), functionImplementation.Validate (List.toArray []))
        Assert.AreEqual((false, "VMULT expects at least two arguments."), functionImplementation.Validate (List.toArray [Number(4.0)]))
        Assert.AreEqual((false, "VMULT expects at least two arguments."), functionImplementation.Validate (null))
        Assert.AreEqual((true, (null: string)), functionImplementation.Validate (List.toArray [ValueArray([| Number(4.0); Number(4.0) |])]))
        Assert.AreEqual((false, "VMULT expects at least two arguments."), functionImplementation.Validate (List.toArray [ValueArray([| Text(">") |])]))

        Assert.AreEqual(ValueArray([| Number(5.0); Number(5.0); Number(1.0); Number(0.0); Number(1.0); Number(7.0) |]), functionImplementation.Execute (List.toArray [Number(1.0); Number(5.0); Number(5.0); Number(1.0); ValueArray([| Number(0.0); Number(1.0); Number(7.0) |])]))
        Assert.AreEqual(ValueArray([| Nothing; Nothing; Nothing; Nothing; Nothing; Nothing |]), functionImplementation.Execute (List.toArray [Nothing; Number(5.0); Number(5.0); Number(1.0); ValueArray([| Number(0.0); Number(1.0); Number(7.0) |])]))
        Assert.AreEqual(ValueArray([| Number(10.0); Number(10.0); Number(2.0); Number(0.0); Number(2.0); Number(14.0) |]), functionImplementation.Execute (List.toArray [Number(2.0); Number(5.0); Number(5.0); Number(1.0); ValueArray([| Number(0.0); Number(1.0); Number(7.0) |])]))
        Assert.AreEqual(ValueArray([| Number(5.0); Nothing; Number(1.0); Number(0.0); Number(1.0); Number(7.0) |]), functionImplementation.Execute (List.toArray [Number(1.0); Number(5.0); Nothing; Number(1.0); ValueArray([| Number(0.0); Number(1.0); Number(7.0) |])]))

        Assert.AreEqual(ValueArray([| |]), functionImplementation.Execute (List.toArray [Number(5.0);  ValueArray([| |])]))
//-----------------------------------------------------------------------
// <copyright file="VariableNameCollectorTests.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Tests

open System.Collections.Generic
open FParsec.CharParsers
open Microsoft.VisualStudio.TestTools.UnitTesting

open Formula.Parser
open Formula.Parser.Ast
open Formula.Parser.Parser
open Formula.Parser.Interpreter
open Formula.Parser.Integration

[<TestClass>]
type VariableNameCollectorTests () =

    let mapProvider (pairs: (string * float) list) =
        MapVariableProvider(Map.ofList pairs) :> IVariableProvider

    [<TestMethod>]
    member this.TestCollectorKeepsEveryNameInOrder () =
        use c = PooledNameCollector.Rent()
        // 80 adds crosses the initial pooled capacity (16) several times, so the buffer grows.
        let names = [ for i in 39 .. -1 .. 0 -> sprintf "N%02d" i ]
        for n in names @ names do c.Add n

        Assert.AreEqual(80, c.Count)
        CollectionAssert.AreEqual(List.toArray (names @ names), c.ToArray())

    [<TestMethod>]
    member this.TestDistinctCollectorForwardsFirstOccurrenceOnly () =
        use target = PooledNameCollector.Rent()
        let distinct = DistinctNameCollector(target)
        for n in [ "B"; "A"; "B"; "C"; "A" ] do distinct.Add n

        CollectionAssert.AreEqual([| "B"; "A"; "C" |], target.ToArray())

    [<TestMethod>]
    member this.TestProviderDuplicatesArePreserved () =
        // Whether MatchNames reports a name more than once is the provider's decision.
        let provider =
            { new IVariableProvider with
                member _.IsDefined (name: string) = name = "A"
                member _.IsDefined (name: string, _sender: IVariableProvider) = name = "A"
                member _.Lookup (_name: string) = Number 1.0
                member _.Lookup (_name: string, _sender: IVariableProvider) = Number 1.0
                member _.LookupRange (_name: string, _lower: value, _upper: value) = Number 1.0
                member _.LookupRange (_name: string, _lower: value, _upper: value, _sender: IVariableProvider) = Number 1.0
                member _.LookupIndex (_name: string, _index: value) = Number 1.0
                member _.LookupIndex (_name: string, _index: value, _sender: IVariableProvider) = Number 1.0
                member _.MatchNames (_pattern: string, collector: IVariableNameCollector) =
                    collector.Add "A"
                    collector.Add "A"
                member _.MatchNames (_pattern: string, collector: IVariableNameCollector, _sender: IVariableProvider) =
                    collector.Add "A"
                    collector.Add "A" }

        CollectionAssert.AreEqual([| "A"; "A" |], Helpers.matchNames provider "*")
        Assert.AreEqual(ValueArray([| Number 1.0; Number 1.0 |]), Helpers.expandWildcard provider "*")

    [<TestMethod>]
    member this.TestCollectorIsResetWhenReRented () =
        let first = PooledNameCollector.Rent()
        first.Add "A"
        first.Dispose()

        use second = PooledNameCollector.Rent()
        Assert.AreEqual(0, second.Count)
        second.Add "A"
        Assert.AreEqual(1, second.Count)

    [<TestMethod>]
    member this.TestDistinctCollectorReuseAfterLargeExpansion () =
        // A rented instance that saw more than the retained-set limit (1024) drops its set on
        // release; when reused it must start empty, forward to its new target and still
        // de-duplicate.
        use firstTarget = PooledNameCollector.Rent()
        let large = DistinctNameCollector.Rent firstTarget
        for i in 0 .. 1999 do large.Add (sprintf "L%d" i)
        Assert.AreEqual(2000, firstTarget.Count)
        large.Release()

        use secondTarget = PooledNameCollector.Rent()
        let reused = DistinctNameCollector.Rent secondTarget
        Assert.IsTrue(obj.ReferenceEquals(large, reused), "Expected the thread's cached collector.")
        let names = [ for i in 0 .. 19 -> sprintf "L%d" i ]
        for n in names @ names do reused.Add n
        reused.Release()

        CollectionAssert.AreEqual(List.toArray names, secondTarget.ToArray())
        Assert.AreEqual(2000, firstTarget.Count)

    [<TestMethod>]
    member this.TestCollectorRentWhileInUseGivesSeparateInstance () =
        use outer = PooledNameCollector.Rent()
        outer.Add "Outer"
        use inner = PooledNameCollector.Rent()
        Assert.IsFalse(obj.ReferenceEquals(outer, inner))
        inner.Add "Inner"
        CollectionAssert.AreEqual([| "Outer" |], outer.ToArray())
        CollectionAssert.AreEqual([| "Inner" |], inner.ToArray())

    [<TestMethod>]
    member this.TestCompositeMatchNamesFirstProviderWins () =
        let composite =
            CompositeVariableProvider([ mapProvider [ "B", 1.0; "A", 2.0 ]; mapProvider [ "A", 5.0; "C", 3.0 ] ])
            :> IVariableProvider

        CollectionAssert.AreEqual([| "A"; "B"; "C" |], Helpers.matchNames composite "*")
        Assert.AreEqual(ValueArray([| Number 2.0; Number 1.0; Number 3.0 |]), Helpers.expandWildcard composite "*")

    [<TestMethod>]
    member this.TestExpressionProviderMatchesOwnExpressionsFirst () =
        let expressions = Dictionary<string, string>(dict [ "E1", "10"; "V1", "99" ])
        let provider =
            ExpressionVariableProvider(expressions, DefaultFunctionProvider.Instance, mapProvider [ "V1", 1.0; "V2", 2.0 ])
            :> IVariableProvider

        // Same order as Lookup resolves: expressions first, then underlying names. "V1" is
        // defined by both, so it is listed once, in the expression's position, and its value
        // comes from the expression.
        CollectionAssert.AreEqual([| "E1"; "V1"; "V2" |], Helpers.matchNames provider "*")
        Assert.AreEqual(ValueArray([| Number 10.0; Number 99.0; Number 2.0 |]), Helpers.expandWildcard provider "*")

    [<TestMethod>]
    member this.TestNestedWildcardExpansion () =
        // Expanding {T*} looks up T1/T2, whose formulas themselves expand {V*} while the
        // outer collector is still in use.
        let expressions = Dictionary<string, string>(dict [ "T1", "SUM({V*})"; "T2", "SUM({V*}) * 2" ])
        let provider =
            ExpressionVariableProvider(expressions, DefaultFunctionProvider.Instance, mapProvider [ "V1", 1.0; "V2", 2.0; "V3", 3.0 ])
            :> IVariableProvider

        match parseFormulaString "SUM({T*})" with
        | Success (ast, _, _) ->
            Assert.AreEqual(Some 18.0, interpretFormula ast provider DefaultFunctionProvider.Instance)
        | Failure (msg, _, _) -> Assert.Fail(msg)

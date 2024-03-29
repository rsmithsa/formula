﻿//-----------------------------------------------------------------------
// <copyright file="TestCsWrapper.cs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.CsTests
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Text;
    using Formula.Parser;
    using Formula.Parser.Integration;
    using Microsoft.VisualStudio.TestTools.UnitTesting;

    [TestClass]
    public class TestCsWrapper : TestBase
    {
        [TestMethod]
        public void TestInterpetUsage()
        {
            var input = "42";

            var result = CsWrapper.InterpretFormula(input);
            Assert.AreEqual(42, result);

            var ast = CsWrapper.ParseFormula(input);
            result = CsWrapper.InterpretExpression(ast);
            Assert.AreEqual(42, result);

            var folded = CsWrapper.ConstantFoldExpression(ast);
            result = CsWrapper.InterpretExpression(folded);
            Assert.AreEqual(42, result);
        }

        [TestMethod]
        public void TestInterpeterDepth()
        {
#if DEBUG
            Console.WriteLine("DEBUG");
#else
            Console.WriteLine("RELEASE");
#endif
            var depth = 500;
            var function = "SQRT(Test)";
            var input = new StringBuilder();

            for (int i = 0; i < depth; i++)
            {
                input.Append("(");
            }
            input.Append($"{function})");
            for (int i = 0; i < depth - 1; i++)
            {
                input.Append($"* {function})");
            }

            var inputStr = input.ToString();
            var sw = Stopwatch.StartNew();
            var result = CsWrapper.InterpretFormula(inputStr, new MapVariableProvider(new Dictionary<string, double>() { { "Test", 1 } }), DefaultFunctionProvider.Instance);
            sw.Stop();

            Console.WriteLine($"Depth: {depth}, Time: {sw.ElapsedMilliseconds}ms");

            Assert.AreEqual(1, result);
        }

        [TestMethod]
        public void TestCompilerDepth()
        {
#if DEBUG
            Console.WriteLine("DEBUG");
#else
            Console.WriteLine("RELEASE");
#endif
            var depth = 190;
            var function = "SQRT(Test)";
            var input = new StringBuilder();

            for (int i = 0; i < depth; i++)
            {
                input.Append("(");
            }
            input.Append($"{function})");
            for (int i = 0; i < depth - 1; i++)
            {
                input.Append($"* {function})");
            }
           
            var inputStr = input.ToString();
            var compiled = CsWrapper.ILCompileExpression(CsWrapper.ParseFormula(inputStr));
            
            var sw = Stopwatch.StartNew();
            var result = compiled.Invoke(new MapVariableProvider(new Dictionary<string, double>() { { "Test", 1 } }), DefaultFunctionProvider.Instance);
            sw.Stop();

            Console.WriteLine($"Depth: {depth}, Time: {sw.ElapsedMilliseconds}ms");

            Assert.AreEqual(1, result);
        }

        class CustomFunctionProvider: IFunctionProvider
        {
            class MyFuncImplementation : IFunctionImplementation
            {
                public Ast.value Execute(Ast.value[] input) => 42.0;

                public bool Validate(Ast.value[] input, out string message)
                {
                    if (input.Length == 0)
                    {
                        message = String.Empty;
                        return true;
                    }
                    else
                    {
                        message = "Expected no arguments";
                        return false;
                    }
                }

                public string Name => "MyFunc";
                public bool IsNonDeterministic => false;
            }

            public IEnumerable<string> KnownFunctions => new[] { "MyFunc" };

            public bool IsDefined(string name) => name == "MyFunc";

            public IFunctionImplementation Lookup(string name)
            {
                if (name == "MyFunc")
                {
                    return new MyFuncImplementation();
                }

                return null;
            }
        }

        [TestMethod]
        public void TestCustomFunctionProvider()
        {
            var input = "MyFunc()";

            var result = CsWrapper.InterpretFormula(input, new CustomFunctionProvider());

            Assert.AreEqual(42, result);
            
            result = CsWrapper.CompileExpression(CsWrapper.ParseFormula(input)).Invoke(MapVariableProvider.Empty, new CustomFunctionProvider());

            Assert.AreEqual(42, result);
            
            result = CsWrapper.ILCompileExpression(CsWrapper.ParseFormula(input)).Invoke(MapVariableProvider.Empty, new CustomFunctionProvider());

            Assert.AreEqual(42, result);
        }

        [TestMethod]
        public void TestCompositeFunctionProvider()
        {
            var input = "MyFunc() * SQRT(4)";

            var result = CsWrapper.InterpretFormula(input, new CompositeFunctionProvider(new IFunctionProvider[] {new CustomFunctionProvider(), DefaultFunctionProvider.Instance }));

            Assert.AreEqual(84, result);
            
            result = CsWrapper.CompileExpression(CsWrapper.ParseFormula(input)).Invoke(MapVariableProvider.Empty, new CompositeFunctionProvider(new IFunctionProvider[] {new CustomFunctionProvider(), DefaultFunctionProvider.Instance }));
            
            Assert.AreEqual(84, result);
            
            result = CsWrapper.ILCompileExpression(CsWrapper.ParseFormula(input)).Invoke(MapVariableProvider.Empty, new CompositeFunctionProvider(new IFunctionProvider[] {new CustomFunctionProvider(), DefaultFunctionProvider.Instance }));
            
            Assert.AreEqual(84, result);
        }

        [TestMethod]
        public void TestExpressionVariableProvider()
        {
            var input = new Dictionary<string, string>()
            {
                { "A", "B*C" },
                { "B", "C * 10" },
                { "C", "SQRT(4) * 5" }
            };

            var variableProvider = new ExpressionVariableProvider(input, DefaultFunctionProvider.Instance);

            var result = variableProvider.Lookup("A");

            Assert.AreEqual(1000, result);
        }

        [TestMethod]
        public void TestMixedVariableProvider()
        {
            var input = new Dictionary<string, string>()
            {
                { "A", "[B Test]*C" },
                { "B Test", "C * [D Test]" },
                { "C", "SQRT(E) * 5" }
            };

            var input2 = new Dictionary<string, double>()
            {
                { "D Test", 10.0 },
                { "E", 4.0 }
            };

            var variableProvider = new CompositeVariableProvider(new Func<IVariableProvider, IVariableProvider>[]
            {
                x => new ExpressionVariableProvider(input, DefaultFunctionProvider.Instance, x),
                x => new MutableVariableProvider(input2)
            });

            var result = variableProvider.Lookup("A", null);

            Assert.AreEqual(1000.0, result);

            result = CsWrapper.InterpretFormula("A", variableProvider);

            Assert.AreEqual(1000.0, result);

            input2["D Test"] = 1.0;

            result = CsWrapper.InterpretFormula("A", variableProvider);

            Assert.AreEqual(100.0, result);
        }
        
        [TestMethod]
        public void TestExpressionDependenciesWithRanges()
        {
            var input = "42";

            var ast = CsWrapper.ParseFormula(input);
            var dependencies = CsWrapper.ExtractExpressionDependenciesWithRanges(ast);
            
            Assert.AreEqual(0, dependencies.Count);

            input = "[A]";

            ast = CsWrapper.ParseFormula(input);
            
            dependencies = CsWrapper.ExtractExpressionDependenciesWithRanges(ast);
            
            Assert.AreEqual(1, dependencies.Count);
            var dep = dependencies["A"];
            Assert.AreEqual("A", dep.Item1);
            Assert.AreEqual(0, dep.Item2);
            Assert.AreEqual(0, dep.Item3);
            
            input = "SUM([A]|-1:0|)";

            ast = CsWrapper.ParseFormula(input);
            
            dependencies = CsWrapper.ExtractExpressionDependenciesWithRanges(ast);
            
            Assert.AreEqual(1, dependencies.Count);
            dep = dependencies["A"];
            Assert.AreEqual("A", dep.Item1);
            Assert.AreEqual(-1, dep.Item2);
            Assert.AreEqual(0, dep.Item3);
            
            input = "SUM([A]|\"ABC\":0|)";

            ast = CsWrapper.ParseFormula(input);
            
            dependencies = CsWrapper.ExtractExpressionDependenciesWithRanges(ast);
            
            Assert.AreEqual(1, dependencies.Count);
            dep = dependencies["A"];
            Assert.AreEqual("A", dep.Item1);
            Assert.AreEqual("ABC", dep.Item2);
            Assert.AreEqual(0, dep.Item3);
            
            input = "SUM([A]|[B]:0|)";

            ast = CsWrapper.ParseFormula(input);
            
            dependencies = CsWrapper.ExtractExpressionDependenciesWithRanges(ast);
            
            Assert.AreEqual(2, dependencies.Count);
            dep = dependencies["A"];
            Assert.AreEqual("A", dep.Item1);
            Assert.AreEqual(null, dep.Item2);
            Assert.AreEqual(0, dep.Item3);
            
            input = "SUM([A]|-1:[B]|)";

            ast = CsWrapper.ParseFormula(input);
            
            dependencies = CsWrapper.ExtractExpressionDependenciesWithRanges(ast);
            
            Assert.AreEqual(2, dependencies.Count);
            dep = dependencies["A"];
            Assert.AreEqual("A", dep.Item1);
            Assert.AreEqual(-1, dep.Item2);
            Assert.AreEqual(null, dep.Item3);
            
            input = "SUM([A]|[C]:[B]|)";

            ast = CsWrapper.ParseFormula(input);
            
            dependencies = CsWrapper.ExtractExpressionDependenciesWithRanges(ast);
            
            Assert.AreEqual(3, dependencies.Count);
            dep = dependencies["A"];
            Assert.AreEqual("A", dep.Item1);
            Assert.AreEqual(null, dep.Item2);
            Assert.AreEqual(null, dep.Item3);
            
            input = "[A]|42|";

            ast = CsWrapper.ParseFormula(input);
            dependencies = CsWrapper.ExtractExpressionDependenciesWithRanges(ast);
            
            Assert.AreEqual(1, dependencies.Count);
            dep = dependencies["A"];
            Assert.AreEqual(42, dep.Item2);
            Assert.AreEqual(42, dep.Item3);
            
            input = "[A]|[B]|";

            ast = CsWrapper.ParseFormula(input);
            dependencies = CsWrapper.ExtractExpressionDependenciesWithRanges(ast);
            
            Assert.AreEqual(2, dependencies.Count);
            dep = dependencies["A"];
            Assert.AreEqual(null, dep.Item2);
            Assert.AreEqual(null, dep.Item3);
        }
    }
}

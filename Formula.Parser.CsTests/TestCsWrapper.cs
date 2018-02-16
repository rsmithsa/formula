//-----------------------------------------------------------------------
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
            var function = "SQRT[Test]";
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

        class CustomFunctionProvider: IFunctionProvider
        {
            class MyFuncImplementation : IFunctionImplementation
            {
                public double Execute(double[] input) => 42.0;

                public bool Validate(double[] input, out string message)
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
            var input = "MyFunc[]";

            var result = CsWrapper.InterpretFormula(input, new CustomFunctionProvider());

            Assert.AreEqual(42, result);
        }

        [TestMethod]
        public void TestCompositeFunctionProvider()
        {
            var input = "MyFunc[] * SQRT[4]";

            var result = CsWrapper.InterpretFormula(input, new CompositeFunctionProvider(new [] {new CustomFunctionProvider(), DefaultFunctionProvider.Instance }));

            Assert.AreEqual(84, result);
        }
    }
}

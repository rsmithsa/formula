//-----------------------------------------------------------------------
// <copyright file="TestBase.cs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

using Formula.Parser.Integration;

namespace Formula.Parser.CsTests
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Text;
    using CsvHelper;
    using Microsoft.VisualStudio.TestTools.UnitTesting;

    [TestClass]
    public class TestDataDrivenTestCases : TestBase
    {
        public class DataDrivenTestCase
        {
            public string Name { get; set; }
            public string Expression { get; set; }
            public double Expected { get; set; }
        }

        [TestMethod]
        public void TestAllDataDrivenTestCases()
        {
            foreach (var sourceFile in Directory.EnumerateFiles(@".\Data", "*.csv"))
            {
                using (var sr = new StreamReader(sourceFile))
                {
                    using (var csvReader = new CsvReader(sr))
                    {
                        var assertions = new List<Tuple<string, double>>();
                        var expressions = new Dictionary<string, string>();
                        foreach (var expressionEntry in csvReader.GetRecords<DataDrivenTestCase>())
                        {
                            expressions.Add(expressionEntry.Name, expressionEntry.Expression);
                            assertions.Add(new Tuple<string, double>(expressionEntry.Name, expressionEntry.Expected));
                        }

                        var variableProvider = new ExpressionVariableProvider(expressions, DefaultFunctionProvider.Instance);

                        foreach (var assertion in assertions)
                        {
                            Assert.AreEqual(assertion.Item2, variableProvider.Lookup(assertion.Item1));
                        }

                        Console.WriteLine($"Validated {assertions.Count} entries from {sourceFile}");
                    }
                }
            }
        }
    }
}

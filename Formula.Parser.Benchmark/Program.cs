//-----------------------------------------------------------------------
// <copyright file="Program.cs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Benchmark
{
    using System;
    using BenchmarkDotNet.Running;
    
    internal class Program
    {
        private static void Main(string[] args)
        {
            var summary = BenchmarkRunner.Run(typeof(Program).Assembly);
        }
    }
}
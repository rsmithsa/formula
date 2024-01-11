//-----------------------------------------------------------------------
// <copyright file="Program.cs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Benchmark
{
    using BenchmarkDotNet.Attributes;
    using Formula.Parser.Integration;
    
    [MemoryDiagnoser]
    //[ShortRunJob]
    public class ArithmeticBenchmark
    {
        private const string Formula = "SUM(SQRT((1 + 2 * [Var1] ^ [Var2]) / 5), SQRT((1 + 2 * [Var1] ^ [Var2]) / 5))";
        private readonly MapVariableProvider variableProvider;
        private readonly IAstItem<Ast.expr> ast;
        private readonly IAstItem<Ast.expr> folded;
        private readonly Func<IVariableProvider, IFunctionProvider, double?> expression;
        private readonly Func<IVariableProvider, IFunctionProvider, double?> il;
        private readonly Func<IVariableProvider, IFunctionProvider, double?> expressionF;
        private readonly Func<IVariableProvider, IFunctionProvider, double?> ilF;

        public ArithmeticBenchmark()
        {
            variableProvider = new MapVariableProvider(new Dictionary<string, double>() { { "Var1", 10 }, { "Var2", 3 } });

            ast = CsWrapper.ParseFormula(Formula);
            expression = CsWrapper.CompileExpression(ast);
            il = CsWrapper.ILCompileExpression(ast);
            
            folded = CsWrapper.ConstantFoldExpression(ast);
            expressionF = CsWrapper.CompileExpression(folded);
            ilF = CsWrapper.ILCompileExpression(folded);
        }
        
        [Benchmark]
        public double? InterpretText() => CsWrapper.InterpretFormula(Formula, this.variableProvider);
        
        [Benchmark(Baseline = true)]
        public double? Interpret() => CsWrapper.InterpretExpression(ast, this.variableProvider);
        
        [Benchmark]
        public double? InterpretFolded() => CsWrapper.InterpretExpression(folded, this.variableProvider);
        
        [Benchmark]
        public double? ExpressionCompiler() => expression.Invoke(this.variableProvider, DefaultFunctionProvider.Instance);
        
        [Benchmark]
        public double? ILCompiler() => il.Invoke(this.variableProvider, DefaultFunctionProvider.Instance);
        
        [Benchmark]
        public double? ExpressionFolded() => expressionF.Invoke(this.variableProvider, DefaultFunctionProvider.Instance);
        
        [Benchmark]
        public double? ILFolded() => ilF.Invoke(this.variableProvider, DefaultFunctionProvider.Instance);
    }
}
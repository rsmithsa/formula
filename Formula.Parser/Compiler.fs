//-----------------------------------------------------------------------
// <copyright file="Compiler.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module Compiler =

    open System;
    open System.Linq.Expressions;

    open Formula.Parser.Ast

    let compileFormula ast =

        let variableProvider =
            Expression.Parameter(typeof<IVariableProvider>, "variableProvider")

        let functionProvider =
            Expression.Parameter(typeof<IFunctionProvider>, "functionProvider")

        let castToBoolExpression value =
            Expression.Condition(Expression.Equal(value, Expression.Constant(0.0)), Expression.Constant(false), Expression.Constant(true)) :> Expression

        let castToDoubleExpression value =
            Expression.Condition(Expression.Equal(value, Expression.Constant(true)), Expression.Constant(1.0), Expression.Constant(0.0)) :> Expression

        let rec compileInternal ast: Expression =

            let compileConstant constant =
                match constant with
                | Number n -> Expression.Constant(n) :> Expression
                | Boolean b -> Expression.Constant(Helpers.castToDouble b) :> Expression

            let compileVariable variable =
                match variable with
                | Identifier id -> Expression.Call(variableProvider, typeof<IVariableProvider>.GetMethod("Lookup", [| typeof<String> |]), Expression.Constant(id)) :> Expression

            let compileNegation negation = 
                Expression.Negate(compileInternal negation) :> Expression

            let compileArtithmetic a op b =
                match op with
                | Add ->
                    Expression.Add(compileInternal a, compileInternal b) :> Expression
                | Subtract ->
                    Expression.Subtract(compileInternal a, compileInternal b) :> Expression
                | Multiply ->
                    Expression.Multiply(compileInternal a, compileInternal b) :> Expression
                | Divide ->           
                    Expression.Divide(compileInternal a, compileInternal b) :> Expression
                | Modulus ->
                    Expression.Modulo(compileInternal a, compileInternal b) :> Expression
                | Power ->
                    Expression.Power(compileInternal a, compileInternal b) :> Expression

            let compileInversion inversion =
                let value = castToBoolExpression (compileInternal inversion)
                castToDoubleExpression (Expression.Not value)

            let compileComparison a op b =
                let valueA = compileInternal a
                let valueB = compileInternal b
                match op with
                | Equal ->
                    castToDoubleExpression (Expression.Equal(valueA, valueB))
                | NotEqual ->
                    castToDoubleExpression (Expression.NotEqual(valueA, valueB))
                | GreaterThan ->
                    castToDoubleExpression (Expression.GreaterThan(valueA, valueB))
                | LessThan ->
                    castToDoubleExpression (Expression.LessThan(valueA, valueB))
                | GreaterThanEqual ->
                    castToDoubleExpression (Expression.GreaterThanOrEqual(valueA, valueB))
                | LessThanEqual ->
                    castToDoubleExpression (Expression.LessThanOrEqual(valueA, valueB))

            let compileLogical a op b =
                let valueA = castToBoolExpression (compileInternal a)
                let valueB = castToBoolExpression (compileInternal b)
                match op with
                | And ->
                    castToDoubleExpression (Expression.And(valueA, valueB))
                | Or ->
                    castToDoubleExpression (Expression.Or(valueA, valueB))

            let compileFunction f args =
                match f with
                | Identifier id ->
                    let compileArg arg = compileInternal arg
                    let compiledArgs = args |> List.map compileArg
                    let argExpression = Expression.NewArrayInit(typeof<double>, compiledArgs)
                    let imp = Expression.Call(functionProvider, typeof<IFunctionProvider>.GetMethod("Lookup"), Expression.Constant(id))
                    Expression.Call(imp, typeof<IFunctionImplementation>.GetMethod("Execute"), argExpression) :> Expression

            let compileBranch cond a b =
                let valueCond = castToBoolExpression (compileInternal cond)
                Expression.Condition(valueCond, (compileInternal a), (compileInternal b)) :> Expression

            match ast with
            | Constant c ->
                compileConstant c
            | Variable v ->
                compileVariable v
            | Negation n ->
                compileNegation n
            | Arithmetic (a, op, b) ->
                compileArtithmetic a op b
            | Inversion i ->
                compileInversion i
            | Comparison (a, op, b) ->
                compileComparison a op b
            | Logical (a, op, b) ->
                compileLogical a op b
            | Function (f, args) ->
                compileFunction f args
            | Branch (cond, a, b) ->
                compileBranch cond a b

        Expression.Lambda((compileInternal ast), variableProvider, functionProvider).Compile() :?> Func<IVariableProvider, IFunctionProvider, double>

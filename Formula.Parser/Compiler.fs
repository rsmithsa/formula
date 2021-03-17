//-----------------------------------------------------------------------
// <copyright file="Compiler.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module Compiler =

    open System;
    open System.Linq.Expressions;

    open Formula.Parser
    open Formula.Parser.Ast

    let compileFormula ast =

        let variableProvider =
            Expression.Parameter(typeof<IVariableProvider>, "variableProvider")

        let functionProvider =
            Expression.Parameter(typeof<IFunctionProvider>, "functionProvider")

        let castToBoolExpression (value: Expression) =
            Expression.Call(typeof<Helpers>.GetMethod("castToBool", [| typeof<value> |]), value) :> Expression

        let castToDoubleExpression (value: Expression) =
            Expression.Call(typeof<Helpers>.GetMethod("castToDouble", [| typeof<value> |]), value) :> Expression

        let rec compileInternal ast: Expression =

            let compileConstant constant =
                let result =
                    match constant with
                    | Number n -> Expression.Constant(n)
                    | Boolean b -> Expression.Constant(b)
                    | Text t -> Expression.Constant(t)

                Expression.Convert(result, typeof<value>) :> Expression

            let compileVariable variable =
                match variable with
                | Identifier id -> Expression.Convert(Expression.Call(variableProvider, typeof<IVariableProvider>.GetMethod("Lookup", [| typeof<String> |]), Expression.Constant(id)), typeof<value>) :> Expression

            let compileNegation negation = 
                let value = castToDoubleExpression(compileInternal negation)
                Expression.Convert(Expression.Negate(value), typeof<value>) :> Expression

            let compileArithmetic a op b =
                let valueA = castToDoubleExpression(compileInternal a)
                let valueB = castToDoubleExpression(compileInternal b)
                let result =
                    match op with
                    | Add ->
                        Expression.Add(valueA, valueB)
                    | Subtract ->
                        Expression.Subtract(valueA, valueB)
                    | Multiply ->
                        Expression.Multiply(valueA, valueB)
                    | Divide ->           
                        Expression.Divide(valueA, valueB)
                    | Modulus ->
                        Expression.Modulo(valueA, valueB)
                    | Power ->
                        Expression.Power(valueA, valueB)
                
                Expression.Convert(result, typeof<value>) :> Expression

            let compileInversion inversion =
                let value = castToBoolExpression(compileInternal inversion)
                Expression.Convert(Expression.Not(value), typeof<value>) :> Expression

            let compileComparison a op b =
                let valueA = compileInternal a
                let valueB = compileInternal b
                let result =
                    match op with
                    | Equal ->
                        Expression.Equal(valueA, valueB)
                    | NotEqual ->
                        Expression.NotEqual(valueA, valueB)
                    | GreaterThan ->
                        Expression.GreaterThan(valueA, valueB)
                    | LessThan ->
                        Expression.LessThan(valueA, valueB)
                    | GreaterThanEqual ->
                        Expression.GreaterThanOrEqual(valueA, valueB)
                    | LessThanEqual ->
                        Expression.LessThanOrEqual(valueA, valueB)

                Expression.Convert(result, typeof<value>) :> Expression

            let compileLogical a op b =
                let valueA = castToBoolExpression(compileInternal a)
                let valueB = castToBoolExpression(compileInternal b)
                let result =
                    match op with
                    | And ->
                        Expression.And(valueA, valueB)
                    | Or ->
                        Expression.Or(valueA, valueB)

                Expression.Convert(result, typeof<value>) :> Expression

            let compileFunction f args =
                match f with
                | Identifier id ->
                    let compileArg arg = compileInternal arg
                    let compiledArgs = args |> List.map compileArg
                    let argExpression = Expression.NewArrayInit(typeof<value>, compiledArgs)
                    let imp = Expression.Call(functionProvider, typeof<IFunctionProvider>.GetMethod("Lookup"), Expression.Constant(id))
                    let result = Expression.Call(imp, typeof<IFunctionImplementation>.GetMethod("Execute"), argExpression)
                    Expression.Convert(result, typeof<value>) :> Expression

            let compileBranch cond a b =
                let valueCond = castToBoolExpression(compileInternal cond)
                Expression.Condition(valueCond, (compileInternal a), (compileInternal b)) :> Expression

            match ast with
            | Constant c ->
                compileConstant c
            | Variable (v, r) ->
                compileVariable v
            | Negation n ->
                compileNegation n
            | Arithmetic (a, op, b) ->
                compileArithmetic a op b
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

        Expression.Lambda(castToDoubleExpression(compileInternal ast), variableProvider, functionProvider).Compile() :?> Func<IVariableProvider, IFunctionProvider, double>

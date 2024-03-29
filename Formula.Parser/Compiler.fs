﻿//-----------------------------------------------------------------------
// <copyright file="Compiler.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module Compiler =

    open System;
    open System.Linq.Expressions
    open System.Reflection;

    open Formula.Parser
    open Formula.Parser.Ast

    let compileFormula<'a> (ast: IAstItem<expr>) =

        let variableProvider =
            Expression.Parameter(typeof<IVariableProvider>, "variableProvider")

        let functionProvider =
            Expression.Parameter(typeof<IFunctionProvider>, "functionProvider")

        let castToBoolExpression (value: Expression) =
            Expression.Call(typeof<Helpers>.GetMethod("castToBool", [| typeof<value[]> |]), value) :> Expression

        let castToDoubleExpression (value: Expression) =
            Expression.Call(typeof<Helpers>.GetMethod("castToDouble", [| typeof<value[]> |]), value) :> Expression
            
        let castToNullableDoubleExpression (value: Expression) =
            Expression.Call(typeof<Helpers>.GetMethod("castToNullableDouble", [| typeof<value[]> |]), value) :> Expression

        let arrayConcatExpression (value: seq<Expression>) =
            let param = Expression.NewArrayInit(typeof<value[]>, value)
            Expression.Call(typeof<Helpers>.GetMethod("arrayConcat").MakeGenericMethod(typeof<value>), param) :> Expression

        let equalityExpression (left: Expression, right: Expression) =
            Expression.Call(typeof<Helpers>.GetMethod("fsEquality").MakeGenericMethod(typeof<value[]>), left, right) :> Expression

        let inequalityExpression (left: Expression, right: Expression) =
            Expression.Call(typeof<Helpers>.GetMethod("fsInequality").MakeGenericMethod(typeof<value[]>), left, right) :> Expression

        let lessThanOrEqualExpression (left: Expression, right: Expression) =
            Expression.Call(typeof<Helpers>.GetMethod("fsLessThanOrEqual").MakeGenericMethod(typeof<value[]>), left, right) :> Expression

        let greaterThanOrEqualExpression (left: Expression, right: Expression) =
            Expression.Call(typeof<Helpers>.GetMethod("fsGreaterThanOrEqual").MakeGenericMethod(typeof<value[]>), left, right) :> Expression

        let lessThanExpression (left: Expression, right: Expression) =
            Expression.Call(typeof<Helpers>.GetMethod("fsLessThan").MakeGenericMethod(typeof<value[]>), left, right) :> Expression

        let greaterThanExpression (left: Expression, right: Expression) =
            Expression.Call(typeof<Helpers>.GetMethod("fsGreaterThan").MakeGenericMethod(typeof<value[]>), left, right) :> Expression
            
        let isSomeExpression (value: Expression) =
            Expression.NotEqual(value, Expression.Constant(null, typeof<Object>)) :> Expression
            
        let getSomeValueExpression (value: Expression) =
            Expression.Property(value, typeof<Option<double>>.GetProperty("Value")) :> Expression
            
        let nothingExpression =
            Expression.NewArrayInit(typeof<value>, Expression.Call(typeof<value>.GetMethod("Empty", BindingFlags.Static ||| BindingFlags.NonPublic))) :> Expression
            
        let valueArrayExpression (value: Expression) =
            Expression.NewArrayInit(typeof<value>, Expression.Convert(value, typeof<value>)) :> Expression

        let rec compileInternal (ast: IAstItem<expr>): Expression =

            let compileConstant constant =
                let result =
                    match constant with
                    | Number n -> Expression.Constant(n) :> Expression
                    | Boolean b -> Expression.Constant(b) :> Expression
                    | Text t -> Expression.Constant(t) :> Expression
                    | Nothing -> Expression.Call(typeof<value>.GetMethod("Empty", BindingFlags.Static ||| BindingFlags.NonPublic)) :> Expression

                Expression.NewArrayInit(typeof<value>, Expression.Convert(result, typeof<value>)) :> Expression

            let compileVariable variable range index =
                match variable with
                | Identifier id ->
                    match range with
                    | Some (a, b) ->
                        let valueA = Expression.ArrayIndex(compileInternal a, Expression.Constant(0))
                        let valueB = Expression.ArrayIndex(compileInternal b, Expression.Constant(0))
                        Expression.Call(variableProvider, typeof<IVariableProvider>.GetMethod("LookupRange", [| typeof<String>; typeof<value>; typeof<value> |]), Expression.Constant(id), valueA, valueB) :> Expression
                    | None ->
                        match index with
                        | Some i ->
                            let value = Expression.ArrayIndex(compileInternal i, Expression.Constant(0))
                            Expression.NewArrayInit(typeof<value>, Expression.Call(variableProvider, typeof<IVariableProvider>.GetMethod("LookupIndex", [| typeof<String>; typeof<value> |]), Expression.Constant(id), value)) :> Expression
                        | None ->
                            Expression.NewArrayInit(typeof<value>, Expression.Call(variableProvider, typeof<IVariableProvider>.GetMethod("Lookup", [| typeof<String> |]), Expression.Constant(id))) :> Expression

            let compileNegation negation = 
                let value = castToDoubleExpression(compileInternal negation)
                Expression.Condition(isSomeExpression value, valueArrayExpression (Expression.Negate(getSomeValueExpression value)), nothingExpression) :> Expression

            let compileArithmetic a op b =
                let valueA = castToDoubleExpression(compileInternal a)
                let valueB = castToDoubleExpression(compileInternal b)
                let result =
                    match op with
                    | Add ->
                        Expression.Add(getSomeValueExpression valueA, getSomeValueExpression valueB)
                    | Subtract ->
                        Expression.Subtract(getSomeValueExpression valueA, getSomeValueExpression valueB)
                    | Multiply ->
                        Expression.Multiply(getSomeValueExpression valueA, getSomeValueExpression valueB)
                    | Divide ->           
                        Expression.Divide(getSomeValueExpression valueA, getSomeValueExpression valueB)
                    | Modulus ->
                        Expression.Modulo(getSomeValueExpression valueA, getSomeValueExpression valueB)
                    | Power ->
                        Expression.Power(getSomeValueExpression valueA, getSomeValueExpression valueB)
                
                Expression.Condition(Expression.AndAlso(isSomeExpression valueA, isSomeExpression valueB), valueArrayExpression result, nothingExpression) :> Expression

            let compileInversion inversion =
                let value = castToBoolExpression(compileInternal inversion)
                Expression.NewArrayInit(typeof<value>, Expression.Convert(Expression.Not(value), typeof<value>)) :> Expression

            let compileComparison a op b =
                let valueA = compileInternal a
                let valueB = compileInternal b
                let result =
                    match op with
                    | Equal ->
                        equalityExpression(valueA, valueB)
                    | NotEqual ->
                        inequalityExpression(valueA, valueB)
                    | GreaterThan ->
                        greaterThanExpression(valueA, valueB)
                    | LessThan ->
                        lessThanExpression(valueA, valueB)
                    | GreaterThanEqual ->
                        greaterThanOrEqualExpression(valueA, valueB)
                    | LessThanEqual ->
                        lessThanOrEqualExpression(valueA, valueB)

                Expression.NewArrayInit(typeof<value>, Expression.Convert(result, typeof<value>)) :> Expression

            let compileLogical a op b =
                let valueA = castToBoolExpression(compileInternal a)
                let valueB = castToBoolExpression(compileInternal b)
                let result =
                    match op with
                    | And ->
                        Expression.And(valueA, valueB)
                    | Or ->
                        Expression.Or(valueA, valueB)

                Expression.NewArrayInit(typeof<value>, Expression.Convert(result, typeof<value>)) :> Expression

            let compileFunction f args =
                match f with
                | Identifier id ->
                    let compileArg arg = compileInternal arg
                    let compiledArgs = args |> List.map compileArg
                    let argExpression = arrayConcatExpression(compiledArgs)
                    let imp = Expression.Call(functionProvider, typeof<IFunctionProvider>.GetMethod("Lookup"), Expression.Constant(id))
                    let result = Expression.Call(imp, typeof<IFunctionImplementation>.GetMethod("Execute"), argExpression)
                    Expression.NewArrayInit(typeof<value>, result) :> Expression

            let compileBranch cond a b =
                let valueCond = castToBoolExpression(compileInternal cond)
                Expression.Condition(valueCond, (compileInternal a), (compileInternal b)) :> Expression
                
            let compileIntermediate (ex: Expression) =
                let inter = Expression.Lambda(ex, variableProvider, functionProvider).Compile()
                Expression.Invoke(Expression.Constant(inter), variableProvider, functionProvider) :> Expression

            match ast.Item with
            | Constant c ->
                compileConstant c.Item
            | Variable (v, r, i) ->
                compileVariable v.Item r i
            | Negation n ->
                compileNegation n
            | Arithmetic (a, op, b) ->
                compileIntermediate (compileArithmetic a op.Item b)
            | Inversion i ->
                compileInversion i
            | Comparison (a, op, b) ->
                compileComparison a op.Item b
            | Logical (a, op, b) ->
                compileLogical a op.Item b
            | Function (f, args) ->
                compileFunction f.Item args
            | Branch (cond, a, b) ->
                compileBranch cond a b

        let outputExpression =
            match typeof<'a> with
            | t when t = typeof<Nullable<double>> -> castToNullableDoubleExpression(compileInternal ast)
            | _ -> castToDoubleExpression(compileInternal ast)

        Expression.Lambda(outputExpression, variableProvider, functionProvider).Compile() :?> Func<IVariableProvider, IFunctionProvider, 'a>

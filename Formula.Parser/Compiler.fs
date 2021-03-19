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
            Expression.Call(typeof<Helpers>.GetMethod("castToBool", [| typeof<value[]> |]), value) :> Expression

        let castToDoubleExpression (value: Expression) =
            Expression.Call(typeof<Helpers>.GetMethod("castToDouble", [| typeof<value[]> |]), value) :> Expression

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

        let rec compileInternal ast: Expression =

            let compileConstant constant =
                let result =
                    match constant with
                    | Number n -> Expression.Constant(n)
                    | Boolean b -> Expression.Constant(b)
                    | Text t -> Expression.Constant(t)

                Expression.NewArrayInit(typeof<value>, Expression.Convert(result, typeof<value>)) :> Expression

            let compileVariable variable range =
                match variable with
                | Identifier id ->
                    match range with
                    | Some (a, b) ->
                        let valueA = Expression.ArrayIndex(compileInternal a, Expression.Constant(0))
                        let valueB = Expression.ArrayIndex(compileInternal b, Expression.Constant(0))
                        Expression.Call(variableProvider, typeof<IVariableProvider>.GetMethod("LookupRange", [| typeof<String>; typeof<value>; typeof<value> |]), Expression.Constant(id), valueA, valueB) :> Expression
                    | None ->
                        Expression.NewArrayInit(typeof<value>, Expression.Call(variableProvider, typeof<IVariableProvider>.GetMethod("Lookup", [| typeof<String> |]), Expression.Constant(id))) :> Expression

            let compileNegation negation = 
                let value = castToDoubleExpression(compileInternal negation)
                Expression.NewArrayInit(typeof<value>, Expression.Convert(Expression.Negate(value), typeof<value>)) :> Expression

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
                
                Expression.NewArrayInit(typeof<value>, Expression.Convert(result, typeof<value>)) :> Expression

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
                    Expression.NewArrayInit(typeof<value>, Expression.Convert(result, typeof<value>)) :> Expression

            let compileBranch cond a b =
                let valueCond = castToBoolExpression(compileInternal cond)
                Expression.Condition(valueCond, (compileInternal a), (compileInternal b)) :> Expression

            match ast with
            | { item = Constant c } ->
                compileConstant c.item
            | { item = Variable (v, r) } ->
                compileVariable v.item r
            | { item = Negation n } ->
                compileNegation n
            | { item = Arithmetic (a, op, b) } ->
                compileArithmetic a op.item b
            | { item = Inversion i } ->
                compileInversion i
            | { item = Comparison (a, op, b) } ->
                compileComparison a op.item b
            | { item = Logical (a, op, b) } ->
                compileLogical a op.item b
            | { item = Function (f, args) } ->
                compileFunction f.item args
            | { item = Branch (cond, a, b) } ->
                compileBranch cond a b

        Expression.Lambda(castToDoubleExpression(compileInternal ast), variableProvider, functionProvider).Compile() :?> Func<IVariableProvider, IFunctionProvider, double>

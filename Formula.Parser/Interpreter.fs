//-----------------------------------------------------------------------
// <copyright file="Interpreter.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module Interpreter =

    open Formula.Parser.Ast

    let interpretFormula ast (vars: IVariableProvider) (functions: IFunctionProvider) =

        let rec interpretFormulaInternal (ast: IAstItem<expr>) (vars: IVariableProvider) (functions: IFunctionProvider): value[] =

            let interpretConstant constant =
                [| constant |]

            let interpretVariable variable range index =
                match variable with
                | Identifier id ->
                    match range with
                    | Some (a, b) ->
                        let valueA = interpretFormulaInternal a vars functions
                        let valueB = interpretFormulaInternal b vars functions
                        let t = vars.LookupRange (id, valueA.[0], valueB.[0])
                        t
                    | None ->
                        match index with
                        | Some i ->
                            let value = interpretFormulaInternal i vars functions
                            [| vars.LookupIndex (id, value.[0]) |]
                        | None -> [| vars.Lookup id |]

            let interpretNegation negation = 
                let value = Helpers.castToDouble(interpretFormulaInternal negation vars functions)
                [| Number(-value) |]

            let interpretArithmetic a op b =
                let valueA = Helpers.castToDouble(interpretFormulaInternal a vars functions)
                let valueB = Helpers.castToDouble(interpretFormulaInternal b vars functions)
                match op with
                | Add ->
                    [| Number(valueA + valueB) |]
                | Subtract ->
                    [| Number(valueA - valueB) |]
                | Multiply ->
                    [| Number(valueA * valueB) |]
                | Divide ->
                    [| Number(valueA / valueB) |]
                | Modulus ->
                    [| Number(valueA % valueB) |]
                | Power ->
                    [| Number(valueA ** valueB) |]

            let interpretInversion inversion =
                let value = Helpers.castToBool(interpretFormulaInternal inversion vars functions)
                [| Boolean(not value) |]

            let interpretComparison a op b =
                let valueA = interpretFormulaInternal a vars functions
                let valueB = interpretFormulaInternal b vars functions
                match op with
                | Equal ->
                    [| Boolean(valueA = valueB) |]
                | NotEqual ->
                    [| Boolean(valueA <> valueB) |]
                | GreaterThan ->
                    [| Boolean(valueA > valueB) |]
                | LessThan ->
                    [| Boolean(valueA < valueB) |]
                | GreaterThanEqual ->
                    [| Boolean(valueA >= valueB) |]
                | LessThanEqual ->
                    [| Boolean(valueA <= valueB) |]

            let interpretLogical a op b =
                let valueA = Helpers.castToBool (interpretFormulaInternal a vars functions)
                let valueB = lazy (Helpers.castToBool (interpretFormulaInternal b vars functions))
                match op with
                | And ->
                    [| Boolean(valueA && valueB.Force()) |]
                | Or ->
                    [| Boolean(valueA || valueB.Force()) |]

            let interpretFunction f args =
                match f with
                | Identifier id ->
                    let interpretArg arg = interpretFormulaInternal arg vars functions
                    let interpretedArgs = args |> List.map interpretArg |> Array.concat
                    let imp = functions.Lookup id
                    [| Number(imp.Execute(interpretedArgs)) |]

            let interpretBranch cond a b =
                let valueCond = Helpers.castToBool(interpretFormulaInternal cond vars functions)
                match valueCond with
                | true ->
                    interpretFormulaInternal a vars functions
                | false ->
                    interpretFormulaInternal b vars functions

            match ast.Item with
            | Constant c ->
                interpretConstant c.Item
            | Variable (v, r, i) ->
                interpretVariable v.Item r i
            | Negation n ->
                interpretNegation n
            | Arithmetic (a, op, b) ->
                interpretArithmetic a op.Item b
            | Inversion i ->
                interpretInversion i
            | Comparison (a, op, b) ->
                interpretComparison a op.Item b
            | Logical (a, op, b) ->
                interpretLogical a op.Item b
            | Function (f, args) ->
                interpretFunction f.Item args
            | Branch (cond, a, b) ->
                interpretBranch cond a b

        let result = interpretFormulaInternal ast vars functions
        Helpers.castToDouble result
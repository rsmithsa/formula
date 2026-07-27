//-----------------------------------------------------------------------
// <copyright file="Interpreter.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module Interpreter =

    open Formula.Parser.Ast

    let interpretFormula ast (vars: IVariableProvider) (functions: IFunctionProvider) =

        let rec interpretFormulaInternal (ast: IAstItem<expr>) (vars: IVariableProvider) (functions: IFunctionProvider) =

            let interpretConstant constant =
                constant

            let interpretVariable variable range index =
                match range with
                | Some (a, b) ->
                    let valueA = interpretFormulaInternal a vars functions
                    let valueB = interpretFormulaInternal b vars functions
                    match variable with
                    | Identifier id -> vars.LookupRange (id, valueA, valueB)
                    | WildcardIdentifier pattern -> Helpers.expandWildcardRange vars pattern valueA valueB
                | None ->
                    match index with
                    | Some i ->
                        let value = interpretFormulaInternal i vars functions
                        match variable with
                        | Identifier id -> vars.LookupIndex (id, value)
                        | WildcardIdentifier pattern -> Helpers.expandWildcardIndex vars pattern value
                    | None ->
                        match variable with
                        | Identifier id -> vars.Lookup id
                        | WildcardIdentifier pattern -> Helpers.expandWildcard vars pattern

            let interpretCoalesce a b =
                let valueA = interpretFormulaInternal a vars functions
                match valueA with
                | Nothing -> interpretFormulaInternal b vars functions
                | _ -> valueA

            let interpretNegation negation = 
                match Helpers.castToDouble(interpretFormulaInternal negation vars functions) with
                | Some value -> Number(-value)
                | None -> Nothing

            let interpretArithmetic a op b =
                match (Helpers.castToDouble(interpretFormulaInternal a vars functions), Helpers.castToDouble(interpretFormulaInternal b vars functions)) with
                | (Some valueA, Some valueB) ->
                    match op with
                    | Add ->
                        Number(valueA + valueB)
                    | Subtract ->
                        Number(valueA - valueB)
                    | Multiply ->
                        Number(valueA * valueB)
                    | Divide ->
                        Number(valueA / valueB)
                    | Modulus ->
                        Number(valueA % valueB)
                    | Power ->
                        Number(valueA ** valueB)
                | _ -> Nothing

            let interpretInversion inversion =
                let value = Helpers.castToBool(interpretFormulaInternal inversion vars functions)
                Boolean(not value)

            let interpretComparison a op b =
                let valueA = interpretFormulaInternal a vars functions
                let valueB = interpretFormulaInternal b vars functions
                match op with
                | Equal ->
                    Boolean(valueA = valueB)
                | NotEqual ->
                    Boolean(valueA <> valueB)
                | GreaterThan ->
                    Boolean(valueA > valueB)
                | LessThan ->
                    Boolean(valueA < valueB)
                | GreaterThanEqual ->
                    Boolean(valueA >= valueB)
                | LessThanEqual ->
                    Boolean(valueA <= valueB)

            let interpretLogical a op b =
                let valueA = Helpers.castToBool (interpretFormulaInternal a vars functions)
                let valueB = lazy (Helpers.castToBool (interpretFormulaInternal b vars functions))
                match op with
                | And ->
                    Boolean(valueA && valueB.Force())
                | Or ->
                    Boolean(valueA || valueB.Force())

            let interpretFunction (f: identifier) args =
                let id = f.IdentifierValue
                let interpretArg arg = interpretFormulaInternal arg vars functions
                let interpretedArgs = args |> List.map interpretArg// |> Array.concat
                let imp = functions.Lookup id
                imp.Execute(List.toArray interpretedArgs)

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
            | Coalesce (a, b) ->
                interpretCoalesce a b
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
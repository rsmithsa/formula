//-----------------------------------------------------------------------
// <copyright file="ILCompiler.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

module ILCompiler =

    open System;
    open System.Reflection
    open System.Reflection.Emit;

    open Formula.Parser
    open Formula.Parser.Ast

    [<AbstractClass; Sealed>]
    type CompiledFormulae private() = class end
    
    let compileFormula<'a> (ast: IAstItem<expr>) =
        
        let invalidOperationEx = typeof<InvalidOperationException>.GetConstructor([| typeof<string> |])
        
        let empty = typeof<value>.GetMethod("Empty", BindingFlags.NonPublic ||| BindingFlags.Static)
        let number = typeof<value>.GetMethod("NewNumber", BindingFlags.Public ||| BindingFlags.Static)
        let boolean = typeof<value>.GetMethod("NewBoolean", BindingFlags.Public ||| BindingFlags.Static)
        let text = typeof<value>.GetMethod("NewText", BindingFlags.Public ||| BindingFlags.Static)
        
        let pow = typeof<Math>.GetMethod("Pow", [| typeof<double>; typeof<double> |])
        
        let nullableDoubleHasValue = typeof<Nullable<double>>.GetProperty("HasValue").GetGetMethod()
        let nullableDoubleGetValueOrDefault = typeof<Nullable<double>>.GetMethod("GetValueOrDefault", [||])
        
        let castToBool = typeof<Helpers>.GetMethod("castToBool", [| typeof<value> |])
        let castToDouble = typeof<Helpers>.GetMethod("castToDouble", [| typeof<value> |])
                    
        let castToNullableDouble = typeof<Helpers>.GetMethod("castToNullableDouble", [| typeof<value> |])

        let equality = typeof<Helpers>.GetMethod("fsEquality").MakeGenericMethod(typeof<value>)
        let inequality = typeof<Helpers>.GetMethod("fsInequality").MakeGenericMethod(typeof<value>)
        let lessThanOrEqual = typeof<Helpers>.GetMethod("fsLessThanOrEqual").MakeGenericMethod(typeof<value>)
        let greaterThanOrEqual = typeof<Helpers>.GetMethod("fsGreaterThanOrEqual").MakeGenericMethod(typeof<value>)
        let lessThan = typeof<Helpers>.GetMethod("fsLessThan").MakeGenericMethod(typeof<value>)
        let greaterThan = typeof<Helpers>.GetMethod("fsGreaterThan").MakeGenericMethod(typeof<value>)

        let lookup = typeof<IVariableProvider>.GetMethod("Lookup", [| typeof<String> |])
        let lookupIndex = typeof<IVariableProvider>.GetMethod("LookupIndex", [| typeof<String>; typeof<value> |])
        let lookupRange = typeof<IVariableProvider>.GetMethod("LookupRange", [| typeof<String>; typeof<value>; typeof<value> |])
        
        let funcLookup = typeof<IFunctionProvider>.GetMethod("Lookup")
        let funcExecute = typeof<IFunctionImplementation>.GetMethod("Execute")
        
        let returnType =
            match typeof<'a> with
            | t when t = typeof<Nullable<double>> -> typeof<Nullable<double>>
            | _ -> typeof<float option>

        let parameterTypes = [| typeof<IVariableProvider>; typeof<IFunctionProvider> |]
        
        let method = DynamicMethod("CompiledFormula", returnType, parameterTypes, typeof<CompiledFormulae>.Module)

        let il = method.GetILGenerator(1024)
        
        let valueImm = il.DeclareLocal(typeof<value>)
        let nullableA = il.DeclareLocal(typeof<Nullable<double>>)
        let nullableB = il.DeclareLocal(typeof<Nullable<double>>)
        
        let rec compileInternal (ast: IAstItem<expr>) =
            
            let compileConstant constant =
                match constant with
                | Number n ->
                    il.Emit(OpCodes.Ldc_R8, n)
                    il.EmitCall(OpCodes.Call, number, null)
                | Boolean b ->
                    il.Emit(OpCodes.Ldc_I4, if b then 1 else 0)
                    il.EmitCall(OpCodes.Call, boolean, null)
                | Text t ->
                    il.Emit(OpCodes.Ldstr, t)
                    il.EmitCall(OpCodes.Call, text, null)
                | Nothing ->
                    il.EmitCall(OpCodes.Call, empty, null)
                | ValueArray a ->
                    il.Emit(OpCodes.Ldstr, "Array constants are not supported.")
                    il.Emit(OpCodes.Newobj, invalidOperationEx)
                    il.Emit(OpCodes.Throw)

            let compileVariable variable range index =
                match variable with
                | Identifier id ->
                    match range with
                    | Some (a, b) ->
                        il.Emit(OpCodes.Ldarg_0)
                        il.Emit(OpCodes.Ldstr, id)
                        
                        compileInternal (a)
                        compileInternal (b)
                        
                        il.EmitCall(OpCodes.Callvirt, lookupRange, null)
                    | None ->
                        match index with
                        | Some i ->
                            il.Emit(OpCodes.Ldarg_0)
                            il.Emit(OpCodes.Ldstr, id)
                            
                            compileInternal (i)
                            
                            il.EmitCall(OpCodes.Callvirt, lookupIndex, null)
                        | None ->
                            il.Emit(OpCodes.Ldarg_0)
                            il.Emit(OpCodes.Ldstr, id)
                            il.EmitCall(OpCodes.Callvirt, lookup, null)

            let compileCoalesce a b = 
                let ret = il.DefineLabel()
                let nullish = il.DefineLabel()
                
                compileInternal (a)
                il.Emit(OpCodes.Stloc, valueImm)
                il.Emit(OpCodes.Ldloc, valueImm)

                il.EmitCall(OpCodes.Call, empty, null)
                il.Emit(OpCodes.Beq, nullish)

                il.Emit(OpCodes.Ldloc, valueImm)
                il.Emit(OpCodes.Br, ret)
        
                il.MarkLabel(nullish)
                compileInternal (b)

                il.MarkLabel(ret)
            
            let compileNegation negation = 
                let ret = il.DefineLabel()
                let notNull = il.DefineLabel()
                
                compileInternal (negation)
                il.EmitCall(OpCodes.Call, castToNullableDouble, null)
                il.Emit(OpCodes.Stloc, nullableA)

                il.Emit(OpCodes.Ldloca, nullableA)
                il.EmitCall(OpCodes.Call, nullableDoubleHasValue, null)
                il.Emit(OpCodes.Brtrue, notNull)
                
                il.EmitCall(OpCodes.Call, empty, null)
                il.Emit(OpCodes.Br, ret)
                
                il.MarkLabel(notNull)
                il.Emit(OpCodes.Ldloca, nullableA)
                il.EmitCall(OpCodes.Call, nullableDoubleGetValueOrDefault, null)
                il.Emit(OpCodes.Neg)
                il.EmitCall(OpCodes.Call, number, null)
                
                il.MarkLabel(ret)

            let compileArithmetic a op b =
                let ret = il.DefineLabel()
                let notNull = il.DefineLabel()
                let nullCase = il.DefineLabel()                
                
                compileInternal (a)
                il.EmitCall(OpCodes.Call, castToNullableDouble, null)

                compileInternal (b)
                il.EmitCall(OpCodes.Call, castToNullableDouble, null)

                il.Emit(OpCodes.Stloc, nullableB)
                il.Emit(OpCodes.Stloc, nullableA)

                il.Emit(OpCodes.Ldloca, nullableA)
                il.EmitCall(OpCodes.Call, nullableDoubleHasValue, null)
                il.Emit(OpCodes.Brfalse, nullCase)
                il.Emit(OpCodes.Ldloca, nullableB)
                il.EmitCall(OpCodes.Call, nullableDoubleHasValue, null)
                il.Emit(OpCodes.Brtrue, notNull)

                il.MarkLabel(nullCase)
                il.EmitCall(OpCodes.Call, empty, null)
                il.Emit(OpCodes.Br, ret)
                
                il.MarkLabel(notNull)
                il.Emit(OpCodes.Ldloca, nullableA)
                il.EmitCall(OpCodes.Call, nullableDoubleGetValueOrDefault, null)
                il.Emit(OpCodes.Ldloca, nullableB)
                il.EmitCall(OpCodes.Call, nullableDoubleGetValueOrDefault, null)
                
                match op with
                | Add -> il.Emit(OpCodes.Add)
                | Subtract -> il.Emit(OpCodes.Sub)
                | Multiply -> il.Emit(OpCodes.Mul)
                | Divide -> il.Emit(OpCodes.Div)
                | Modulus -> il.Emit(OpCodes.Rem)
                | Power -> il.EmitCall(OpCodes.Call, pow, null)
                
                il.EmitCall(OpCodes.Call, number, null)
                
                il.MarkLabel(ret)

            let compileInversion inversion =
                compileInternal (inversion)

                il.EmitCall(OpCodes.Call, castToBool, null)
                
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Ceq)
                il.EmitCall(OpCodes.Call, boolean, null)

            let compileComparison a op b =
                compileInternal (a)
                compileInternal (b)
                
                match op with
                | Equal -> il.EmitCall(OpCodes.Call, equality, null)
                | NotEqual -> il.EmitCall(OpCodes.Call, inequality, null)
                | GreaterThan -> il.EmitCall(OpCodes.Call, greaterThan, null)
                | LessThan -> il.EmitCall(OpCodes.Call, lessThan, null)
                | GreaterThanEqual -> il.EmitCall(OpCodes.Call, greaterThanOrEqual, null)
                | LessThanEqual -> il.EmitCall(OpCodes.Call, lessThanOrEqual, null)
                
                il.EmitCall(OpCodes.Call, boolean, null)

            let compileLogical a op b =
                compileInternal (b)
                il.EmitCall(OpCodes.Call, castToBool, null)
                
                compileInternal (a)
                il.EmitCall(OpCodes.Call, castToBool, null)
                
                match op with
                | And -> il.Emit(OpCodes.And)
                | Or -> il.Emit(OpCodes.Or)
                il.EmitCall(OpCodes.Call, boolean, null)

            let compileFunction f (args: IAstItem<expr> list) =
                match f with
                | Identifier id ->
                    il.Emit(OpCodes.Ldarg_1)
                    il.Emit(OpCodes.Ldstr, id)
                    il.EmitCall(OpCodes.Callvirt, funcLookup, null)

                    il.Emit(OpCodes.Ldc_I4, args.Length)
                    il.Emit(OpCodes.Newarr, typeof<value>)
                    args |> List.iteri (
                        fun i x ->
                            il.Emit(OpCodes.Dup)
                            il.Emit(OpCodes.Ldc_I4, i)
                            compileInternal(x)
                            il.Emit(OpCodes.Stelem_Ref)
                    )

                    il.EmitCall(OpCodes.Callvirt, funcExecute, null)

            let compileBranch cond a b =
                let ret = il.DefineLabel()
                let truthy = il.DefineLabel()
                
                compileInternal (cond)

                il.EmitCall(OpCodes.Call, castToBool, null)
                il.Emit(OpCodes.Brtrue, truthy)

                compileInternal (b)
                il.Emit(OpCodes.Br, ret)
                                
                il.MarkLabel(truthy)
                compileInternal (a)
                il.MarkLabel(ret)

            match ast.Item with
            | Constant c ->
                compileConstant c.Item
            | Variable (v, r, i) ->
                compileVariable v.Item r i
            | Coalesce (a, b) ->
                compileCoalesce a b
            | Negation n ->
                compileNegation n
            | Arithmetic (a, op, b) ->
                compileArithmetic a op.Item b
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

        compileInternal(ast)
        
        match typeof<'a> with
        | t when t = typeof<Nullable<double>> -> il.EmitCall(OpCodes.Call, castToNullableDouble, null)
        | _ -> il.EmitCall(OpCodes.Call, castToDouble, null)
        
        il.Emit(OpCodes.Ret)
        
        //let bytes = il.GetType().GetMethod("BakeByteArray", BindingFlags.NonPublic ||| BindingFlags.Instance).Invoke(il, null) :?> byte[]
        //System.IO.File.WriteAllBytes(@"C:\Users\Richard\Source\formula\temp.dll", bytes)
                
        match typeof<'a> with
        | t when t = typeof<Nullable<double>> -> method.CreateDelegate(typeof<Func<IVariableProvider, IFunctionProvider, Nullable<double>>>) :?> Func<IVariableProvider, IFunctionProvider, 'a>
        | _ -> method.CreateDelegate(typeof<Func<IVariableProvider, IFunctionProvider, float option>>) :?> Func<IVariableProvider, IFunctionProvider, 'a>

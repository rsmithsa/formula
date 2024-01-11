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
        
        let empty = typeof<value>.GetMethod("Empty", BindingFlags.NonPublic ||| BindingFlags.Static)
        let number = typeof<value>.GetMethod("NewNumber", BindingFlags.Public ||| BindingFlags.Static)
        let boolean = typeof<value>.GetMethod("NewBoolean", BindingFlags.Public ||| BindingFlags.Static)
        let text = typeof<value>.GetMethod("NewText", BindingFlags.Public ||| BindingFlags.Static)
        
        let pow = typeof<Math>.GetMethod("Pow", [| typeof<double>; typeof<double> |])
        
        let valueProperty = typeof<float option>.GetProperty("Value")
        
        let castToBool = typeof<Helpers>.GetMethod("castToBool", [| typeof<value[]> |])
        let castToDouble = typeof<Helpers>.GetMethod("castToDouble", [| typeof<value[]> |])
                    
        let castToNullableDouble = typeof<Helpers>.GetMethod("castToNullableDouble", [| typeof<value[]> |])
        
        let equality = typeof<Helpers>.GetMethod("fsEquality").MakeGenericMethod(typeof<value[]>)
        let inequality = typeof<Helpers>.GetMethod("fsInequality").MakeGenericMethod(typeof<value[]>)
        let lessThanOrEqual = typeof<Helpers>.GetMethod("fsLessThanOrEqual").MakeGenericMethod(typeof<value[]>)
        let greaterThanOrEqual = typeof<Helpers>.GetMethod("fsGreaterThanOrEqual").MakeGenericMethod(typeof<value[]>)
        let lessThan = typeof<Helpers>.GetMethod("fsLessThan").MakeGenericMethod(typeof<value[]>)
        let greaterThan = typeof<Helpers>.GetMethod("fsGreaterThan").MakeGenericMethod(typeof<value[]>)

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
        let fTemp = il.DeclareLocal(typeof<float>)
        let vTemp = il.DeclareLocal(typeof<value>)
        let aImm = il.DeclareLocal(typeof<float option>)
        let bImm = il.DeclareLocal(typeof<float option>)
        let aBoolImm = il.DeclareLocal(typeof<bool>)
        let bBoolImm = il.DeclareLocal(typeof<bool>)
        let arrImm = il.DeclareLocal(typeof<value[]>)
        
        let i = il.DeclareLocal(typeof<int>)
        let j = il.DeclareLocal(typeof<int>)
        let a = il.DeclareLocal(typeof<int>)
        let argArray = il.DeclareLocal(typeof<value[]>)
        let curArray = il.DeclareLocal(typeof<value[]>)
        let arrTmp = il.DeclareLocal(typeof<value[]>)
        let argLen = il.DeclareLocal(typeof<int>)
        let nestedArgArray = il.DeclareLocal(typeof<value[][]>)
        
        let rec compileInternal (ast: IAstItem<expr>) =
            
            let compileConstant constant =
                il.Emit(OpCodes.Ldc_I4_1)
                il.Emit(OpCodes.Newarr, typeof<value>)
                il.Emit(OpCodes.Stloc, curArray)
                il.Emit(OpCodes.Ldloc, curArray)
                il.Emit(OpCodes.Ldc_I4_0)
                
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
                
                il.Emit(OpCodes.Stelem_Ref)
                il.Emit(OpCodes.Ldloc, curArray)

            let compileVariable variable range index =
                match variable with
                | Identifier id ->
                    match range with
                    | Some (a, b) ->
                        il.Emit(OpCodes.Ldarg_0)
                        il.Emit(OpCodes.Ldstr, id)
                        
                        compileInternal (a)
                        il.Emit(OpCodes.Ldc_I4_0)
                        il.Emit(OpCodes.Ldelem_Ref)
                        
                        compileInternal (b)
                        il.Emit(OpCodes.Ldc_I4_0)
                        il.Emit(OpCodes.Ldelem_Ref)
                        
                        il.EmitCall(OpCodes.Callvirt, lookupRange, null)
                    | None ->
                        il.Emit(OpCodes.Ldc_I4_1)
                        il.Emit(OpCodes.Newarr, typeof<value>)
                        il.Emit(OpCodes.Stloc, curArray)
                        il.Emit(OpCodes.Ldloc, curArray)
                        il.Emit(OpCodes.Ldc_I4_0)
                        
                        match index with
                        | Some i ->
                            il.Emit(OpCodes.Ldarg_0)
                            il.Emit(OpCodes.Ldstr, id)
                            
                            compileInternal (i)
                            il.Emit(OpCodes.Ldc_I4_0)
                            il.Emit(OpCodes.Ldelem_Ref)
                            
                            il.EmitCall(OpCodes.Callvirt, lookupIndex, null)
                        | None ->
                            il.Emit(OpCodes.Ldarg_0)
                            il.Emit(OpCodes.Ldstr, id)
                            il.EmitCall(OpCodes.Callvirt, lookup, null)
                        
                        il.Emit(OpCodes.Stelem_Ref)
                        il.Emit(OpCodes.Ldloc, curArray)

            let compileNegation negation = 
                let store = il.DefineLabel()
                let notNull = il.DefineLabel()
                
                compileInternal (negation)

                il.Emit(OpCodes.Stloc, curArray)
                il.Emit(OpCodes.Ldloc, curArray)
                il.EmitCall(OpCodes.Call, castToDouble, null)
                il.Emit(OpCodes.Stloc, aImm)
                il.Emit(OpCodes.Ldloc, aImm)
                
                il.Emit(OpCodes.Brtrue_S, notNull)
                
                il.Emit(OpCodes.Ldloc, curArray)
                il.Emit(OpCodes.Ldc_I4_0)
                il.EmitCall(OpCodes.Call, empty, null)
                il.Emit(OpCodes.Br_S, store)
                                
                il.MarkLabel(notNull)
                il.Emit(OpCodes.Ldloc, aImm)
                il.EmitCall(OpCodes.Call, valueProperty.GetMethod, null)
                il.Emit(OpCodes.Neg)
                il.Emit(OpCodes.Stloc, fTemp)
                il.Emit(OpCodes.Ldloc, curArray)
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Ldloc, fTemp)
                il.EmitCall(OpCodes.Call, number, null)
                
                il.MarkLabel(store)
                il.Emit(OpCodes.Stelem_Ref)
                il.Emit(OpCodes.Ldloc, curArray)

            let compileArithmetic a op b =
                let store = il.DefineLabel()
                let notNull = il.DefineLabel()
                
                compileInternal (b)
                il.Emit(OpCodes.Stloc, curArray)
                il.Emit(OpCodes.Ldloc, curArray)
                il.EmitCall(OpCodes.Call, castToDouble, null)
                il.Emit(OpCodes.Stloc, bImm)
                il.Emit(OpCodes.Ldloc, curArray)
                il.Emit(OpCodes.Ldloc, bImm)

                compileInternal (a)
                il.EmitCall(OpCodes.Call, castToDouble, null)
                il.Emit(OpCodes.Stloc, aImm)
                il.Emit(OpCodes.Stloc, bImm)
                il.Emit(OpCodes.Stloc, curArray)
                il.Emit(OpCodes.Ldloc, aImm)
                il.Emit(OpCodes.Ldloc, bImm)
                
                il.Emit(OpCodes.And)
                il.Emit(OpCodes.Brtrue_S, notNull)
                
                il.Emit(OpCodes.Ldloc, curArray)
                il.Emit(OpCodes.Ldc_I4_0)
                il.EmitCall(OpCodes.Call, empty, null)
                il.Emit(OpCodes.Br_S, store)
                
                il.MarkLabel(notNull)
                il.Emit(OpCodes.Ldloc, aImm)
                il.EmitCall(OpCodes.Call, valueProperty.GetMethod, null)
                il.Emit(OpCodes.Ldloc, bImm)
                il.EmitCall(OpCodes.Call, valueProperty.GetMethod, null)
                
                match op with
                | Add -> il.Emit(OpCodes.Add)
                | Subtract -> il.Emit(OpCodes.Sub)
                | Multiply -> il.Emit(OpCodes.Mul)
                | Divide -> il.Emit(OpCodes.Div)
                | Modulus -> il.Emit(OpCodes.Rem)
                | Power -> il.EmitCall(OpCodes.Call, pow, null)
                
                il.Emit(OpCodes.Stloc, fTemp)
                il.Emit(OpCodes.Ldloc, curArray)
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Ldloc, fTemp)
                il.EmitCall(OpCodes.Call, number, null)
                
                il.MarkLabel(store)
                il.Emit(OpCodes.Stelem_Ref)
                il.Emit(OpCodes.Ldloc, curArray)

            let compileInversion inversion =
                compileInternal (inversion)

                il.Emit(OpCodes.Dup)
                il.Emit(OpCodes.Dup)
                il.EmitCall(OpCodes.Call, castToBool, null)
                il.Emit(OpCodes.Stloc, aBoolImm)
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Ldloc, aBoolImm)
                
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Ceq)
                il.EmitCall(OpCodes.Call, boolean, null)

                il.Emit(OpCodes.Stelem_Ref)

            let compileComparison a op b =
                compileInternal (a)
                
                il.Emit(OpCodes.Dup)
                il.Emit(OpCodes.Dup)
                il.Emit(OpCodes.Stloc, arrImm)
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Ldloc, arrImm)
                                
                compileInternal (b)
                
                match op with
                | Equal -> il.EmitCall(OpCodes.Call, equality, null)
                | NotEqual -> il.EmitCall(OpCodes.Call, inequality, null)
                | GreaterThan -> il.EmitCall(OpCodes.Call, greaterThan, null)
                | LessThan -> il.EmitCall(OpCodes.Call, lessThan, null)
                | GreaterThanEqual -> il.EmitCall(OpCodes.Call, greaterThanOrEqual, null)
                | LessThanEqual -> il.EmitCall(OpCodes.Call, lessThanOrEqual, null)
                
                il.EmitCall(OpCodes.Call, boolean, null)

                il.Emit(OpCodes.Stelem_Ref)

            let compileLogical a op b =
                compileInternal (b)

                il.Emit(OpCodes.Dup)
                il.Emit(OpCodes.Dup)
                il.EmitCall(OpCodes.Call, castToBool, null)
                il.Emit(OpCodes.Stloc, bBoolImm)
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Ldloc, bBoolImm)
                
                compileInternal (a)
                il.EmitCall(OpCodes.Call, castToBool, null)
                
                match op with
                | And -> il.Emit(OpCodes.And)
                | Or -> il.Emit(OpCodes.Or)
                il.EmitCall(OpCodes.Call, boolean, null)

                il.Emit(OpCodes.Stelem_Ref)

            let compileFunction f (args: IAstItem<expr> list) =
                let loop = il.DefineLabel()
                let condition = il.DefineLabel()
                let innerLoop = il.DefineLabel()
                let innerCondition = il.DefineLabel()
                let execute = il.DefineLabel()
                
                match f with
                | Identifier id ->
                    il.Emit(OpCodes.Ldc_I4, args.Length)
                    il.Emit(OpCodes.Newarr, typeof<value[]>)
                    il.Emit(OpCodes.Stloc, nestedArgArray)
                    il.Emit(OpCodes.Ldc_I4_0)
                    il.Emit(OpCodes.Stloc, argLen)
                    args |> List.iteri (
                        fun i x ->
                            il.Emit(OpCodes.Ldloc, nestedArgArray)
                            il.Emit(OpCodes.Ldloc, argLen)
                            compileInternal (x)
                            il.Emit(OpCodes.Stloc, arrTmp)
                            il.Emit(OpCodes.Stloc, argLen)
                            il.Emit(OpCodes.Stloc, nestedArgArray)
                            
                            il.Emit(OpCodes.Ldloc, arrTmp)
                            il.Emit(OpCodes.Ldlen)
                            il.Emit(OpCodes.Conv_I4)
                            il.Emit(OpCodes.Ldloc, argLen)
                            il.Emit(OpCodes.Add)
                            il.Emit(OpCodes.Stloc, argLen)
                            
                            il.Emit(OpCodes.Ldloc, nestedArgArray)
                            il.Emit(OpCodes.Ldc_I4, i)
                            il.Emit(OpCodes.Ldloc, arrTmp)
                            il.Emit(OpCodes.Stelem_Ref)
                    )
                    il.Emit(OpCodes.Ldloc, argLen)
                    il.Emit(OpCodes.Newarr, typeof<value>)
                    il.Emit(OpCodes.Stloc, argArray)
                    il.Emit(OpCodes.Ldloc, argLen)
                    il.Emit(OpCodes.Ldc_I4_0)
                    il.Emit(OpCodes.Beq_S, execute)
                    
                    il.Emit(OpCodes.Ldc_I4_0)
                    il.Emit(OpCodes.Stloc, i)
                    il.Emit(OpCodes.Ldc_I4_0)
                    il.Emit(OpCodes.Stloc, a)
                    il.MarkLabel(loop)
      
                    il.Emit(OpCodes.Ldloc, nestedArgArray)
                    il.Emit(OpCodes.Ldloc, a)
                    il.Emit(OpCodes.Ldelem_Ref)
                    il.Emit(OpCodes.Stloc, arrTmp)
                    
                    il.Emit(OpCodes.Ldc_I4_0)
                    il.Emit(OpCodes.Stloc, j)
                    
                    il.Emit(OpCodes.Br_S, innerCondition)
                    il.MarkLabel(innerLoop)
                    
                    il.Emit(OpCodes.Ldloc, argArray)
                    il.Emit(OpCodes.Ldloc, i)
                    il.Emit(OpCodes.Ldloc, arrTmp)
                    il.Emit(OpCodes.Ldloc, j)
                    il.Emit(OpCodes.Ldelem_Ref)
                    il.Emit(OpCodes.Stelem_Ref)
                    
                    il.Emit(OpCodes.Ldloc, j)
                    il.Emit(OpCodes.Ldc_I4_1)
                    il.Emit(OpCodes.Add)
                    il.Emit(OpCodes.Stloc, j)
                    
                    il.Emit(OpCodes.Ldloc, i)
                    il.Emit(OpCodes.Ldc_I4_1)
                    il.Emit(OpCodes.Add)
                    il.Emit(OpCodes.Stloc, i)
                    
                    il.MarkLabel(innerCondition)
                    il.Emit(OpCodes.Ldloc, j)
                    il.Emit(OpCodes.Ldloc, arrTmp)
                    il.Emit(OpCodes.Ldlen)
                    il.Emit(OpCodes.Conv_I4)
                    il.Emit(OpCodes.Blt_S, innerLoop)
                    
                    il.Emit(OpCodes.Ldloc, a)
                    il.Emit(OpCodes.Ldc_I4_1)
                    il.Emit(OpCodes.Add)
                    il.Emit(OpCodes.Stloc, a)
                   
                    il.MarkLabel(condition)
                    il.Emit(OpCodes.Ldloc, i)
                    il.Emit(OpCodes.Ldloc, argLen)
                    il.Emit(OpCodes.Blt_S, loop)
                    
                    il.MarkLabel(execute)
                    il.Emit(OpCodes.Ldarg_1)
                    il.Emit(OpCodes.Ldstr, id)
                    il.EmitCall(OpCodes.Callvirt, funcLookup, null)
                    il.Emit(OpCodes.Ldloc, argArray)
                    il.EmitCall(OpCodes.Callvirt, funcExecute, null)
                    il.Emit(OpCodes.Stloc, vTemp)
                    
                    il.Emit(OpCodes.Ldc_I4_1)
                    il.Emit(OpCodes.Newarr, typeof<value>)
                    il.Emit(OpCodes.Stloc, curArray)
                    il.Emit(OpCodes.Ldloc, curArray)
                    il.Emit(OpCodes.Ldc_I4_0)
                    il.Emit(OpCodes.Ldloc, vTemp)
                    il.Emit(OpCodes.Stelem_Ref)
                    il.Emit(OpCodes.Ldloc, curArray)

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

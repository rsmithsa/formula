//-----------------------------------------------------------------------
// <copyright file="Program.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

open System
open System.Diagnostics

open FParsec.CharParsers

open Formula.Parser.Parser
open Formula.Parser.ConstantFolder
open Formula.Parser.Interpreter

let waitToClose (returnCode: int) =
    match System.Diagnostics.Debugger.IsAttached with  
    | true ->  
        printfn "\nPress any key to continue..."  
        System.Console.ReadKey(true) |> ignore
        returnCode  
    | false -> returnCode

[<EntryPoint>]
let main argv =

    if argv.Length < 1 then
        printfn "usage: Formula.Parser.exe <formula> [<variable> <value>]..."
        exit (waitToClose 1)

    printfn "Formula to parse:\n%s\n" argv.[0]

    // The parser is run on the formula in argv.[0].
    // The parser result will be the abstract syntax tree of the input formula.
    let result = parseFormulaString argv.[0]
    match result with
    | Success (v, _, _) ->
        // Print AST
        printfn "The AST of the input formula is:\n%A\n" v

        let fold = foldConstants v
        printfn "The constant folded AST of the input formula is:\n%A\n" fold

        // Build variable lookup from argv.[1..]
        let variableMap =
            argv.[1..]
            |> Seq.chunkBySize 2
            |> Seq.map (fun a -> a.[0], float a.[1])
            |> Map.ofSeq

        // Interpret and print result
        let evalResult = interpretFormula v variableMap
        printfn "Function result:\n%f\n" evalResult

        let evalFoldResult = interpretFormula fold variableMap
        printfn "Folded function result:\n%f\n" evalFoldResult

        waitToClose 0
    | Failure (msg, err, _) ->
        printfn "%s" msg
        waitToClose 1

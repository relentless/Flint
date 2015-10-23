open Parser
open Interpreter
open CoreFunctions

[<EntryPoint>]
let main _ = 

    printfn "Welcome to Flint, the F# Lisp Interpreter, v0.2.0"

    printfn "Type exit to quit\n"

    let rec inputLoop evaluationRecord =
        printf "\n> "
        match System.Console.ReadLine() with
        | "exit" | "Exit" -> ()
        | input -> 
            let evaluated = {evaluationRecord with Expression = input |> parse} |> evaluate
            print evaluated.Expression
            inputLoop evaluated

    loadCoreLib() |> inputLoop

    printfn "(Goodbye!)"

    0

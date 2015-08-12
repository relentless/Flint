open Parser
open Interpreter

[<EntryPoint>]
let main _ = 

    printfn "Welcome to Flint, the F# Lisp Interpreter, v0.1.1"

    printfn "Type exit to quit\n"

    let rec inputLoop environment functions =
        printf "\n> "
        match System.Console.ReadLine() with
        | "exit" | "Exit" -> ()
        | input -> 
            let evaluated = input |> parse |> integrate environment functions |> evaluate
            print evaluated.Expression
            inputLoop evaluated.Environment evaluated.Functions

    inputLoop initialEnvironment initialFunctions

    printfn "Goodbye"

    0

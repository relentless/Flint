open Parser
open Interpreter

let execute environment functions text =
    text
    |> parse
    |> evaluate environment functions

[<EntryPoint>]
let main _ = 

    printfn "Welcome to Flint, the F# Lisp Interpreter, v0.1.1"

    printfn "Type exit to quit\n"

    let rec inputLoop environment functions =
        printf "\n> "
        match System.Console.ReadLine() with
        | "exit" | "Exit" -> ()
        | input -> 
            let environment, functions, result = input |> parse |> evaluate environment functions
            print result
            inputLoop environment functions

    inputLoop initialEnvironment initialFunctions

    printfn "Goodbye"

    0

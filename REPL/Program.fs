open Parser
open Interpreter

let execute environment text =
    text
    |> parse
    |> evaluate environment

[<EntryPoint>]
let main _ = 

    printfn "Welcome to Flint, the F# Lisp Interpreter, v0.1.1"

    printfn "Type exit to quit\n"

    let rec inputLoop environment =
        printf "\n> "
        match System.Console.ReadLine() with
        | "exit" | "Exit" -> ()
        | input -> 
            let environment, result = input |> parse |> evaluate environment
            print result
            inputLoop environment

    inputLoop initialEnvironment

    printfn "Goodbye"

    0

open Parser
open Interpreter

let execute text =
    text
    |> parse
    |> evaluate
    |> print 

[<EntryPoint>]
let main _ = 

    printfn "Welcome to Flint v0.1"

    printfn "\nEnter X to exit\n"

    let rec inputLoop() =
        printf "\n> "
        match System.Console.ReadLine() with
        | "X" | "x" -> ()
        | input -> 
            execute input
            inputLoop()

    inputLoop()

    printfn "Goodbye"

    0

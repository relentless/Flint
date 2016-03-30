open Integrator

[<EntryPoint>]
let main _ = 

    printfn "Welcome to Flint, the F# Lisp Interpreter, v0.3.0"

    printfn "Type exit to quit\n"

    let rec inputLoop evaluationRecord =
        printf "\n> "
        match System.Console.ReadLine() with
        | "exit" | "Exit" -> ()
        | input -> 
            let evaluated = evaluationRecord |> process input
            print evaluated.Expression
            inputLoop evaluated

    loadCoreLib() |> inputLoop

    printfn "(Goodbye!)"

    0

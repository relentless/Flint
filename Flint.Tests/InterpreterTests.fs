module InterpreterTests

open NUnit.Framework
open Swensen.Unquote
open SyntaxTree
open Interpreter

let evalInitial expression = 
    evaluate initialEnvironment expression
    |> result

[<Test>]
let ``Intrepreting numeric atom evaluates to itself`` () =
    test <@ evalInitial <| Number(99) = Number(99) @>

[<Test>]
let ``Interpreting symbolic atom not in environment is an error`` () =
    try
        evalInitial <| Symbol("Hi Mum") |> ignore
        failwith "exception not thrown as expected"
    with
    | ex -> test <@ ex.Message = "Symbol 'Hi Mum' not found."  @>

[<Test>]
let ``Interpreting symbolic atom in environment evaluates to value in environment`` () =
    test <@ evalInitial <| SeparateExpressions( [ExpList([Symbol("define");Symbol("x");Number(99)]); Symbol("x") ]) = SeparateExpressions( [Nil; Number(99)]) @>

[<Test>]
let ``Interpreting quoted list evaluates to list`` () =
    test <@ evalInitial <| ExpList( [Symbol("quote"); Number(1); Number(2)]) = ExpList( [Number(1); Number(2)]) @>

[<Test>]
let ``Interpreting separate expressions evaluates to separate expressions`` () =
    test <@ evalInitial <| SeparateExpressions( [Number(3); Number(4); Number(5)]) = SeparateExpressions( [Number(3); Number(4); Number(5)]) @>
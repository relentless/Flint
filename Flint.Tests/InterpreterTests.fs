module InterpreterTests

open NUnit.Framework
open Swensen.Unquote
open SyntaxTree
open Interpreter

let evalInitial expression = 
    evaluate initialEnvironment expression
    |> result

[<Test>]
let ``Numeric atom evaluates to itself`` () =
    test <@ evalInitial <| Number(99) = Number(99) @>

[<Test>]
let ``Symbolic atom evaluates to itself`` () =
    test <@ evalInitial <| Symbol("Hi Mum") = Symbol("Hi Mum") @>

[<Test>]
let ``Quoted list evaluates to list`` () =
    test <@ evalInitial <| ExpList( [Symbol("quote"); Number(1); Number(2)]) = ExpList( [Number(1); Number(2)]) @>

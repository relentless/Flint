module InterpreterTests

open NUnit.Framework
open Swensen.Unquote
open SyntaxTree
open Interpreter

[<Test>]
let ``Numeric atom evaluates to itself`` () =
    test <@ evaluate <| Number(99) = Number(99) @>

[<Test>]
let ``Symbolic atom evaluates to itself`` () =
    test <@ evaluate <| Symbol("Hi Mum") = Symbol("Hi Mum") @>

[<Test>]
let ``Quoted list evaluates to list`` () =
    test <@ evaluate <| ExpList( [Symbol("quote"); Number(1); Number(2)]) = ExpList( [Number(1); Number(2)]) @>

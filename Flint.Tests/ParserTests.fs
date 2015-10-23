module Flint.Tests

open NUnit.Framework
open Swensen.Unquote
open SyntaxTree
open Parser

let parseFirst text = parse text |> (fun (SeparateExpressions(first::rest)) -> first)

[<Test>]
let ``Parsing single number`` () =
    test <@ parseFirst "3" = Number(3) @>

[<Test>]
let ``Parsing quote symbol`` () =
    test <@ parseFirst "quote" = Symbol("quote") @>

[<Test>]
let ``Parsing quoted list`` () =
    test <@ parseFirst "(quote 1 2)" = ExpList( [Symbol("quote"); Number(1); Number(2)]) @>

[<Test>]
let ``Parsing quote operator list`` () =
    test <@ parseFirst "'(1 2)" = ExpList( [Symbol("quote"); ExpList([Number(1); Number(2)])]) @>

[<Test>]
let ``Parsing define`` () =
    test <@ parseFirst "(define myVal 99)" = ExpList( [Symbol("define"); Symbol("myVal"); Number(99)]) @>

[<Test>]
let ``Parsing separate expressions`` () =
    test <@ parse "1 2 3" = SeparateExpressions( [Number(1); Number(2); Number(3)]) @>

[<Test>]
let ``Parsing lambda`` () =
    test <@ parseFirst "(lambda (x) (+ x x))" = ExpList([Symbol("lambda"); ExpList([Symbol("x")]); ExpList([Symbol("+"); Symbol("x"); Symbol("x")])]) @>
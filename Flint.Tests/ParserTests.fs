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

[<Test>]
let ``TO FIX: lists don't need a space between them`` () =
    test <@ parseFirst "(+ (+ 1 1)(+ 2 2))" = ExpList([Symbol("+");ExpList([Symbol("+");Number(1);Number(1)]);ExpList([Symbol("+");Number(2);Number(2)])])  @>

[<Test>]
let ``Parsing all valid extended alphanumeric characters work in a symbol name`` () =
    test <@ parseFirst "(define abc!$%&*+-./:<=>?@^_~ 123)" = ExpList( [Symbol("define"); Symbol("abc!$%&*+-./:<=>?@^_~"); Number(123)])  @>

[<Test>]
let ``Parsing open paren with spaces after`` () =
    test <@ parseFirst "( + 1 2)" = ExpList( [Symbol("+"); Number(1); Number(2)])  @>

[<Test>]
let ``TO FIX: Parsing close paren with spaces before`` () =
    test <@ parseFirst "(+ 1 2 )" = ExpList( [Symbol("+"); Number(1); Number(2)])  @>

[<Test>]
let ``Parsing multi-line statements`` () =
    test <@ parseFirst """(+ 
    1 
    2)""" = ExpList( [Symbol("+"); Number(1); Number(2)])  @>

[<Test>]
let ``Parsing expressions with a newline before`` () =
    test <@ parseFirst """
    (+ 
    1
    2)""" = ExpList( [Symbol("+"); Number(1); Number(2)])  @>

[<Test>]
let ``Parsing expressions with spaces before`` () =
    test <@ parseFirst """   (+ 1 2)""" = ExpList( [Symbol("+"); Number(1); Number(2)])  @>

[<Test>]
let ``Parsing single-line comments on the same line as code`` () =
    test <@ parseFirst """(+ 
    1 ; hi mum
    2)""" = ExpList( [Symbol("+"); Number(1); Number(2)])  @>

[<Test>]
let ``Parsing single-line comments in the middle of a list`` () =
    test <@ parseFirst """(+ 
    1
    ; hi mum
    2)""" = ExpList( [Symbol("+"); Number(1); Number(2)])  @>

[<Test>]
let ``Parsing single-line comments before a list`` () =
    test <@ parseFirst """
    ; some code:
    (+ 
    1
    ; hi mum
    2)""" = ExpList( [Symbol("+"); Number(1); Number(2)])  @>
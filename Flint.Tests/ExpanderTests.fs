module ExpanderTests

open NUnit.Framework
open Swensen.Unquote
open SyntaxTree
open Expander

[<Test>]
let ``Expanding Quote symbol ExpList results in QuotedList`` () =
    test <@ expand <| ExpList([Symbol("quote");ExpList([Number(1)])]) = QuotedList([Number(1)]) @>

// (let ((x 10)) x) -> ((lambda (x) x) 10)
[<Test>]
let ``Expanding let with single assignment results in lambda`` () =
    let letExpression = ExpList([Symbol("let");ExpList([ExpList([Symbol("x");Number(10)])]); Symbol("x")])
    let lambdaExpression = ExpList([ExpList([Symbol("lambda"); ExpList([Symbol("x")]); Symbol("x")]); Number(10)])
    test <@ expand <| letExpression = lambdaExpression @>

// (let ((x 10)(y 5)) (+ x y)) -> ((lambda (x) ((lambda (y) (+ x y)) 5)) 10)
[<Test>]
let ``Expanding let with multiple assignment results in lambda`` () =
    let letExpression = ExpList([Symbol("let");ExpList([ExpList([Symbol("x");Number(10)]);ExpList([Symbol("y");Number(5)])]); ExpList([Symbol("+");Symbol("x");Symbol("y")])])
    let lambdaExpression = ExpList([ExpList([Symbol("lambda"); ExpList([Symbol("x")]);ExpList([ExpList([Symbol("lambda"); ExpList([Symbol("y")]); ExpList([Symbol("+");Symbol("x");Symbol("y")])]); Number(5)])]); Number(10)])
    test <@ expand <| letExpression = lambdaExpression @>

[<Test>]
let ``Expanding ExpList with eval and QuotedExpression results in plan ExpList`` () =
    test <@ expand <| ExpList([Symbol("eval");QuotedList([Symbol("+");Number(1);Number(2)])]) = ExpList([Symbol("+");Number(1);Number(2)]) @>
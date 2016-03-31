module ExpanderTests

open NUnit.Framework
open Swensen.Unquote
open SyntaxTree
open Expander

[<Test>]
let ``Expanding Quote symbol ExpList results in QuotedList`` () =
    test <@ expand <| ExpList([Symbol("quote");ExpList([Number(1)])]) = QuotedList([Number(1)]) @>

// (let ((x 10))
//  x)

//((lambda (x)
//   x)
// 10)

[<Test>]
let ``Expanding let with single assignment results in lambda`` () =
    let letExpression = ExpList([Symbol("let");ExpList([ExpList([Symbol("x");Number(10)])]); Symbol("x")])
    let lambdaExpression = ExpList([ExpList([Symbol("lambda"); ExpList([Symbol("x")]); Symbol("x")]); Number(10)])
    test <@ expand <| letExpression = lambdaExpression @>
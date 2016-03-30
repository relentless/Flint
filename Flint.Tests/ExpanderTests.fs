module ExpanderTests

open NUnit.Framework
open Swensen.Unquote
open SyntaxTree
open Expander

[<Test>]
let ``Expanding Quote symbol ExpList results in QuotedList`` () =
    test <@ expand <| ExpList([Symbol("quote");ExpList([Number(1)])]) = QuotedList([Number(1)]) @>

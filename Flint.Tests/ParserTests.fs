module Flint.Tests

open NUnit.Framework
open Swensen.Unquote
open SyntaxTree
open Parser

[<Test>]
let ``Single number parses successfully`` () =
    test <@ parse "3" = Number(3) @>

[<Test>]
let ``Quote symbol parses successfully`` () =
    test <@ parse "quote" = Symbol("quote") @>

[<Test>]
let ``Quoted list parses successfully`` () =
    test <@ parse "(quote 1 2)" = ExpList( [Symbol("quote"); Number(1); Number(2)]) @>

[<Test>]
let ``Quote operator list parses successfully`` () =
    test <@ parse "'(1 2)" = ExpList( [Symbol("quote"); Number(1); Number(2)]) @>
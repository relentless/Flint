module Flint.Tests

open FsUnit
open NUnit.Framework
open Swensen.Unquote
open SyntaxTree
open Parser

[<Test>]
let ``Single number parses successfully`` () =
    test <@ parse "3" = Number(3) @>


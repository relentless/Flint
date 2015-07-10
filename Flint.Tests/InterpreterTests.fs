module InterpreterTests

open NUnit.Framework
open Swensen.Unquote
open SyntaxTree
open Interpreter

[<Test>]
let ``Atom evaluates to itself`` () =
    test <@ evaluate <| Number(99) = Number(99) @>

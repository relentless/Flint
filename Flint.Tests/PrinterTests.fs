module PrinterTests

open NUnit.Framework
open Swensen.Unquote
open Printer
open SyntaxTree

[<Test>]
let ``Printing an ExpList puts in in parens`` () =
    test <@ ExpList([Number(1);Number(2)]) |> toString = "(1 2)"  @>
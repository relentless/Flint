﻿module InterpreterTests

open NUnit.Framework
open Swensen.Unquote
open SyntaxTree
open Interpreter

let evalInitial expression = 
    evaluate initialEnvironment expression
    |> result

[<Test>]
let ``Intrepreting numeric atom evaluates to itself`` () =
    test <@ evalInitial <| Number(99) = Number(99) @>

[<Test>]
let ``Interpreting symbolic atom not in environment is an error`` () =
    try
        evalInitial <| Symbol("Hi Mum") |> ignore
        failwith "exception not thrown as expected"
    with
    | ex -> test <@ ex.Message = "Symbol 'Hi Mum' not found."  @>

[<Test>]
let ``Interpreting symbolic atom in environment evaluates to value in environment`` () =
    test <@ evalInitial <| SeparateExpressions( [ExpList([Symbol("define");Symbol("x");Number(99)]); Symbol("x") ]) = SeparateExpressions( [Nil; Number(99)]) @>

[<Test>]
let ``Interpreting quoted list evaluates to list`` () =
    test <@ evalInitial <| ExpList( [Symbol("quote"); Number(1); Number(2)]) = ExpList( [Number(1); Number(2)]) @>

[<Test>]
let ``Interpreting separate expressions evaluates to separate expressions`` () =
    test <@ evalInitial <| SeparateExpressions( [Number(3); Number(4); Number(5)]) = SeparateExpressions( [Number(3); Number(4); Number(5)]) @>

[<Test>]
let ``Interpreting lambda evaluates to Procedure`` () =
    test <@ evalInitial <| ExpList([Symbol("lambda"); ExpList([Symbol("x")]); ExpList([Symbol("+"); Symbol("x"); Symbol("x")])]) = Procedure(formals = [Symbol("x")], body = [Symbol("+"); Symbol("x"); Symbol("x")]) @>

[<Test>]
let ``Interpreting applied procedure evaluates procedure using supplied arguments`` () =
    test <@ evalInitial <| ExpList([Procedure(formals = [Symbol("x")], body = [Symbol("+"); Symbol("x"); Symbol("x")]); Number(3)]) = Number(6) @>
    
//[<Test>]
//let ``TODO: Interpreting a lambda results in a Procedure`` () =
//    test <@ evalInitial <| ExpList([Symbol("lambda"); ExpList([]); Number(0)]) = Procedure() @>

[<Test>]
let ``TODO: Interpreting an applied lambda without params works`` () =
    test <@ evalInitial <| ExpList([ExpList([Symbol("lambda"); ExpList([]); Number(0)])]) = Number(0) @>
    
[<Test>]
let ``TODO: Interpreting Complex expression that evaluates to function works as first expression in a list`` () =
    test <@ evalInitial <| ExpList([ExpList([Symbol("if"); Boolean(true); Symbol("+"); Symbol("-")]); Number(1); Number(2)]) = Number(3) @>
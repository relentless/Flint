module InterpreterTests

open NUnit.Framework
open Swensen.Unquote
open SyntaxTree
open Interpreter
open CoreFunctions

let evalInitial expression = 
    expression
    |> integrate initialEnvironment initialFunctions 
    |> evaluate 
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
let ``Interpreting symbol evaluates to value in environment`` () =
    test <@ evalInitial <| Symbol("+") = Lambda("+") @>

[<Test>]
let ``Interpreting built-in lambda evaluates it against the arguments`` () =
    test <@ evalInitial <| ExpList([Symbol("+"); Number(5); Number(1)]) = Number(6) @>

[<Test>]
let ``Interpreting symbolic atom in environment evaluates to value in environment`` () =
    test <@ evalInitial <| SeparateExpressions( [ExpList([Symbol("define");Symbol("x");Number(99)]); Symbol("x") ]) = SeparateExpressions( [Nil; Number(99)]) @>

[<Test>]
let ``Interpreting separate expressions evaluates to separate expressions`` () =
    test <@ evalInitial <| SeparateExpressions( [Number(3); Number(4); Number(5)]) = SeparateExpressions( [Number(3); Number(4); Number(5)]) @>

[<Test>]
let ``Interpreting lambda adds a function to the function list`` () =
    let initialFunctionCount = initialFunctions.Count
    let result = ExpList([Symbol("lambda"); ExpList([]); Number(0)]) |> integrate initialEnvironment initialFunctions |> evaluate
    test <@ result.Functions.Count = initialFunctionCount + 1 @>

[<Test>]
let ``Interpreting an applied lambda without params works`` () =
    test <@ evalInitial <| ExpList([ExpList([Symbol("lambda"); ExpList([]); Number(0)])]) = Number(0) @>
    
[<Test>]
let ``Interpreting Complex expression that evaluates to function works as first expression in a list`` () =
    test <@ evalInitial <| ExpList([ExpList([Symbol("if"); Boolean(true); Symbol("+"); Symbol("-")]); Number(1); Number(2)]) = Number(3) @>

[<Test>]
let ``Interpreting an ExpList with a quote symbol gives a QuotedList`` () =
    test <@ evalInitial <| ExpList([Symbol("quote"); ExpList([Number(0)])]) = QuotedList([Number(0)]) @>
    
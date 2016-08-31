module InterpreterTests

open NUnit.Framework
open Swensen.Unquote
open SyntaxTree
open Evaluator
open Integrator
open CoreFunctions

[<Test>]
let ``Intrepreting numeric atom evaluates to itself`` () =
    test <@ evalInitial <| Number(99) = Number(99) @>

[<Test>]
let ``Evaluating symbolic atom not in environment is an error`` () =
    try
        evalInitial <| Symbol("Hi Mum") |> ignore
        failwith "exception not thrown as expected"
    with
    | ex -> test <@ ex.Message = "Symbol 'Hi Mum' not found."  @>

[<Test>]
let ``Evaluating symbol evaluates to value in environment`` () =
    test <@ evalInitial <| Symbol("+") = Lambda("+") @>

[<Test>]
let ``Evaluating built-in lambda evaluates it against the arguments`` () =
    test <@ evalInitial <| ExpList([Symbol("+"); Number(5); Number(1)]) = Number(6) @>

[<Test>]
let ``Evaluating symbolic atom in environment evaluates to value in environment`` () =
    test <@ evalInitial <| SeparateExpressions( [ExpList([Symbol("define");Symbol("x");Number(99)]); Symbol("x") ]) = Number(99) @>

[<Test>]
let ``Evaluating separate expressions evaluates to separate expressions`` () =
    test <@ evalInitial <| SeparateExpressions( [Number(3); Number(4); Number(5)]) = SeparateExpressions( [Number(3); Number(4); Number(5)]) @>

[<Test>]
let ``Evaluating lambda adds a function to the function list`` () =
    let initialFunctionCount = initialFunctions.Count
    let result = ExpList([Symbol("lambda"); ExpList([]); Number(0)]) |> integrate initialEnvironment initialFunctions |> evaluate
    test <@ result.Functions.Count = initialFunctionCount + 1 @>

[<Test>]
let ``Evaluating an applied lambda without params works`` () =
    test <@ evalInitial <| ExpList([ExpList([Symbol("lambda"); ExpList([]); Number(0)])]) = Number(0) @>
    
[<Test>]
let ``Evaluating Complex expression that evaluates to function works as first expression in a list`` () =
    test <@ evalInitial <| ExpList([ExpList([Symbol("if"); Boolean(true); Symbol("+"); Symbol("-")]); Number(1); Number(2)]) = Number(3) @>

[<Test>]
let ``Evaluating an ExpList with a quote symbol gives a QuotedList`` () =
    test <@ evalInitial <| ExpList([Symbol("quote"); ExpList([Number(0)])]) = QuotedList([Number(0)]) @>
    
[<Test>]
let ``Evaluating cond with single expression true`` () =
    test <@ evalInitial <| ExpList([Symbol("cond"); ExpList([Boolean(true);Number(1)])]) = Number(1) @>

[<Test>]
let ``Evaluating cond with later expression true`` () =
    let conditionalTree = ExpList([Symbol("cond"); ExpList([Boolean(false);Number(1)]); ExpList([Boolean(false);Number(2)]); ExpList([Boolean(true);Number(3)])])
    test <@ evalInitial <| conditionalTree = Number(3)  @>

[<Test>]
let ``Evaluating cond uses else if nothing else true`` () =
    let conditionalTree = ExpList([Symbol("cond"); ExpList([Boolean(false);Number(1)]); ExpList([Boolean(false);Number(2)]); ExpList([Symbol("else");Number(3)])])
    test <@ evalInitial <| conditionalTree = Number(3)  @>

[<Test>]
let ``Evaluating define adds value to environment`` () =
    let defineTree = ExpList([Symbol("define");Symbol("x");Number(10)])
    let evaluated = defineTree |> integrate CoreFunctions.initialEnvironment CoreFunctions.initialFunctions |> evaluate
    test <@ evaluated.Environment.["x"] = Number(10) @>

[<Test>]
let ``Evaluating separate expressions with leading Nils removes Nils`` () =
    test <@ evalInitial <| SeparateExpressions([Nil; Number(1); Number(2)]) = SeparateExpressions([Number(1); Number(2)])  @>
    test <@ evalInitial <| SeparateExpressions([Nil; Nil; Number(1); Number(2)]) = SeparateExpressions([Number(1); Number(2)])  @>

[<Test>]
let ``Evaluating separate expressions with only one trailing non-Nil expression gives the single non-Nil expression`` () =
    test <@ evalInitial <| SeparateExpressions([Nil; Number(1)]) = Number(1)  @>
    test <@ evalInitial <| SeparateExpressions([Number(2)]) = Number(2)  @>
    test <@ evalInitial <| SeparateExpressions([Nil; Nil; Number(3)]) = Number(3)  @>
    test <@ evalInitial <| SeparateExpressions([Nil; SeparateExpressions([Nil; Number(1)])]) = Number(1)  @>

[<Test>]
let ``Evaluating nested lambdas works`` () =
    let lambdaTree = 
        SeparateExpressions([
            ExpList([
                Symbol("define"); 
                Symbol("add15"); 
                ExpList([
                    Symbol("lambda")
                    ExpList([
                        Symbol("x")
                    ]);
                    SeparateExpressions([
                        ExpList([
                            Symbol("define");
                            Symbol("add10");
                            ExpList([
                                Symbol("lambda");
                                ExpList([
                                    Symbol("y")
                                ]);
                                ExpList([
                                    Symbol("+");
                                    Number(10);
                                    Symbol("y")
                                ])
                            ])
                        ])
                        ExpList([
                            Symbol("+");
                            Symbol("x");
                            ExpList([
                                Symbol("add10");
                                Number(5)
                            ])
                        ])
                    ])
                ])
            ]);
            ExpList([Symbol("add15");Number(1)])
        ])

    test <@ evalInitial <| lambdaTree = Number(16)  @>
    
﻿module EndToEndTests

open NUnit.Framework
open Swensen.Unquote
open Parser
open Interpreter
open Printer
open SyntaxTree

let execute text =
    text
    |> parse
    |> evaluate initialEnvironment initialFunctions
    |> result
    |> toString 

[<Test>]
let ``Addition`` () =
    test <@ execute "(+ 7 8 9)" = "24"  @>

[<Test>]
let ``Subtraction`` () =
    test <@ execute "(- 20 2 6)" = "12"  @>

[<Test>]
let ``Multiplication`` () =
    test <@ execute "(* 3 4 10)" = "120"  @>
[<Test>]
let ``Division`` () =
    test <@ execute "(/ 100 2 5)" = "10"  @>

[<Test>]
let ``Quoted list`` () =
    test <@ execute "(quote #t #f 99)" = "(#t #f 99)"  @>

[<Test>]
let ``Quotation shortcut operator`` () =
    test <@ execute "'(2 3 4)" = "(2 3 4)"  @>

[<Test>]
let ``Cons`` () =
    test <@ execute "(cons 1 '(2 3 4))" = "(1 2 3 4)"  @>

[<Test>]
let ``Car`` () =
    test <@ execute "(car '(4 5 6))" = "4"  @>

[<Test>]
let ``Cdr`` () =
    test <@ execute "(cdr '(4 5 6))" = "(5 6)"  @>

[<Test>]
let ``If true`` () =
    test <@ execute """(if #t "Hi Mum" "Hi Dad")""" = "\"Hi Mum\""  @>

[<Test>]
let ``If false`` () =
    test <@ execute """(if #f "Hi Mum" "Hi Dad")""" = "\"Hi Dad\""  @>

[<Test>]
let ``Greater than numeric`` () =
    test <@ execute "(> 3 4)" = "#f"  @>

[<Test>]
let ``Less than numeric`` () =
    test <@ execute "(< 3 4)" = "#t"  @>

[<Test>]
let ``Equal numeric`` () =
    test <@ execute "(= 3 4)" = "#f"  @>

[<Test>]
let ``Multiple levels of expression get evaluated`` () =
    test <@ execute """(if (< (+ 3 4) (- 4 3)) "never happen" (cons (car '(1 2 3)) (cdr '(3 4 5))))""" = "(1 4 5)"  @>

[<Test>]
let ``Define adds value to environment`` () =
    let environment, functions, result = "(define x 10)" |> parse |> evaluate initialEnvironment initialFunctions
    test <@ environment.["x"] = Number(10) @>

[<Test>]
let ``Built-in procedures evaluate to a Procedure`` () =
    test <@ execute "+"  = "#<procedure:+>" @>

[<Test>]
let ``Lambdas evaluate to a Procedure`` () =
    test <@ execute "(lambda () 0)"  = "#<procedure:lambda1>" @>

[<Test>]
let ``Complex expression that evaluates to function works as first expression in a list`` () =
    test <@ execute "((if #t + -) 1 2)"  = "3" @>

[<Test>]
let ``Lambda is evaluated using the following parameters`` () =
    test <@ execute "((lambda (x) (+ x x)) 1)"  = "2" @>

[<Test>]
let ``Lambda can use value declared previously`` () =
    test <@ execute "(define z 10) ((lambda (x) (+ x z)) 5)"  = "\n15" @>

[<Test>]
let ``Lambda can use value declared previously when defines as a value`` () =
    test <@ execute "(define z 10) (define addToZ (lambda (x) (+ x z))) (addToZ 3)"  = "\n\n13" @>
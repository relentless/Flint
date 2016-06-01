module EndToEndTests

open NUnit.Framework
open Swensen.Unquote
open Integrator

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
let ``Cons`` () =
    test <@ execute "(cons 1 '(2 3 4))" = "'(1 2 3 4)"  @>

[<Test>]
let ``Car`` () =
    test <@ execute "(car '(4 5 6))" = "4"  @>

[<Test>]
let ``Cdr`` () =
    test <@ execute "(cdr '(4 5 6))" = "'(5 6)"  @>

[<Test>]
let ``If true`` () =
    test <@ execute """(if #t "Hi Mum" "Hi Dad")""" = "\"Hi Mum\""  @>

[<Test>]
let ``If false`` () =
    test <@ execute """(if #f "Hi Mum" "Hi Dad")""" = "\"Hi Dad\""  @>

[<Test>]
let ``If true works without false case`` () =
    test <@ execute """(if #t "I speak the truth")""" = "\"I speak the truth\""  @>

[<Test>]
let ``If false without false case does nothing`` () =
    test <@ execute """(if #f "I speak the truth")""" = ""  @>

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
    test <@ execute """(if (< (+ 3 4) (- 4 3)) "never happen" (+ (+ (/ 20 2) (* 2 3)) (- (+ 2 2) (- 4 1))))""" = "17"  @>

[<Test>]
let ``Built-in procedures evaluate to a Procedure`` () =
    test <@ execute "+"  = "#<procedure:+>" @>

[<Test>]
let ``Lambdas evaluate to a Procedure`` () =
    test <@ (execute "(lambda () 0)").[0..17]  = "#<procedure:lambda" @>

[<Test>]
let ``Complex expression that evaluates to function works as first expression in a list`` () =
    test <@ execute "((if #t (if #f - +) -) 1 2)"  = "3" @>

[<Test>]
let ``Lambda is evaluated using the following parameters`` () =
    test <@ execute "((lambda (x) (+ x x)) 1)"  = "2" @>

[<Test>]
let ``Lambda can use value declared previously`` () =
    test <@ execute "(define z 10) ((lambda (x) (+ x z)) 5)"  = "15" @>

[<Test>]
let ``Lambda can use value declared previously when defines as a value`` () =
    test <@ execute "(define z 10) (define addToZ (lambda (x) (+ x z))) (addToZ 3)"  = "13" @>

[<Test>]
let ``Lambda can be recursive`` () =
    test <@ execute "(define fib (lambda (n) (if (< n 3) n (+ (fib (- n 1)) (fib (- n 2)))))) (fib 5)"  = "8" @>

[<Test>]
let ``Eq? works with booleans`` () =
    test <@ execute "(eq? #t #f)"  = "#f" @>

[<Test>]
let ``Eq? works with empty lists`` () =
    test <@ execute "(eq? (cdr '(1)) '())"  = "#t" @>

[<Test>]
let ``Multiple lambdas work`` () =
    test <@ execute "((lambda (y) (* y y)) ((lambda (x) (+ x x)) 5))"  = "100" @>

[<Test>]
let ``VarArgs turn the arguments into a Quoted List`` () =
    test <@ execute "((lambda args args) 1 2 3)"  = "'(1 2 3)" @>

[<Test>]
let ``VarArgs work in a lambda`` () =
    test <@ execute "((lambda args (car args)) 1 2 3)"  = "1" @>

[<Test>]
let ``Quoted list results in quoted list`` () =
    test <@ execute "(quote (#t #f 99))" = "'(#t #f 99)"  @>

[<Test>]
let ``Quotation shortcut operator results in same thing returned`` () =
    test <@ execute "'(2 3 4)" = "'(2 3 4)"  @>

[<Test>]
let ``Quote does not evaluate expression`` () =
    test <@ execute "(define x (lambda (y) (* y y))) '(x 3)" = "'(x 3)"  @>

[<Test>]
let ``When a lambda contains more than one expression, the last expression is returned`` () =
    test <@ execute "((lambda () 2 3 4))" = "4"  @>

[<Test>]
let ``Variable defined within a lambda works`` () =
    test <@ execute "((lambda (b) (define c 5) (+ c b)) 10)" = "15"  @>

[<Test>]
let ``Lambda defined within a lambda works`` () =
    test <@ execute "(define a (lambda (b) (define c (lambda (d) d)) (c b))) (a 10)" = "10"  @>

[<Test>]
let ``Shortcut for defining a lambda works`` () =
    test <@ execute "(define (square x) (* x x)) (square 10)" = "100"  @>

[<Test>]
let ``cond with single expression true`` () =
    test <@ execute "(cond (#t 99))" = "99"  @>

[<Test>]
let ``cond with later expression true`` () =
    test <@ execute "(cond (#f 99) (#f 99) (#t 1))" = "1"  @>

[<Test>]
let ``cond uses else clause when others false`` () =
    test <@ execute "(cond (#f 99) (else 3))" = "3"  @>

[<Test>]
let ``cond with else not last causes error`` () =
    try
        execute "(cond (#f 1) (else 2) (#t 3))" |> ignore
        failwith "Test didn't fail in expected way"
    with
        | :? System.Exception as evaluationException -> test <@ evaluationException.Message = "Invalid 'cond' statement - 'else' expression must be last" @>

[<Test>]
let ``single let assignment works`` () =
    test <@ execute "(let ((x 10)) (+ x 3))" = "13"  @>

[<Test>]
let ``multiple let assignment works`` () =
    test <@ execute "(let ((x 10) (y 5)) (+ x y))" = "15"  @>

[<Test>]
let ``Comments are ignored`` () =
    test <@ execute """
    ; here's a comment
    (+ 
    1 ; hi mum
    2)""" = "3"  @>

[<Test>]
let ``type checking predicates work for complex expressions`` () =
    test <@ execute "(boolean? (car (cdr '(1 #f 3))))" = "#t"  @>

[<Test>]
let ``non-functions applied to arguments gives error`` () =
    try
        execute "(1 #f 3)" = "#t" |> ignore
        failwith "Test didn't fail in expected way"
    with
        | :? System.Exception as evaluationException -> test <@ evaluationException.Message = "function application expected" @>

[<Test>]
let ``eval works with quoted expression`` () =
    test <@ execute "(eval '(+ 1 2 3))" = "6"  @>

//[<Test>]
//let ``eval works with non-quoted expression`` () =
//    test <@ execute "(eval (+ 1 2 3))" = "6"  @>
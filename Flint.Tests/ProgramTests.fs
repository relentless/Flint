module ProgramTests

open NUnit.Framework
open Swensen.Unquote
open Integrator

[<Test>]
[<Ignore>]
let ``TODO: FizzBuzz`` () =
    test <@ execute "(define (fizz number)
  
  (define (divisible-by? dividend) 
    (eq? (modulo number dividend) 0))
  
  (cond 
    ((divisible-by? 15) \"FizzBuzz\")
    ((divisible-by? 5) \"Buzz\")
    ((divisible-by? 3) \"Fizz\")
    (else (number->string number))))

(fizz 1)
(fizz 2)
(fizz 3)
(fizz 4)
(fizz 5)" = "\"1\"
\"2\"
\"Fizz\"
\"4\"
\"Buzz\""  @>

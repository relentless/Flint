﻿module CoreLibTests

open NUnit.Framework
open Swensen.Unquote
open Parser
open Interpreter
open Printer

[<Test>]
let ``not true`` () =
    test <@ execute "(not #t)" = "#f"  @>

[<Test>]
let ``not false`` () =
    test <@ execute "(not #f)" = "#t"  @>

[<Test>]
let ``and multiple args is true`` () =
    test <@ execute "(and #t #t #t #t)" = "#t"  @>

[<Test>]
let ``and multiple args is false`` () =
    test <@ execute "(and #t #t #f #t)" = "#f"  @>

[<Test>]
let ``TODO: cond with single expression true`` () =
    test <@ execute "(cond (#t 99))" = "99"  @>
module CoreLibTests

open NUnit.Framework
open Swensen.Unquote
open Interpreter

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
let ``null? true when null`` () =
    test <@ execute "(null? '())" = "#t"  @>

[<Test>]
let ``null? false when non-empty list`` () =
    test <@ execute "(null? '(1))" = "#f"  @>

[<Test>]
let ``null? false when primitive`` () =
    test <@ execute "(null? 1)" = "#f"  @>
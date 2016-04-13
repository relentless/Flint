module BuiltInFunctionTests

open NUnit.Framework
open Swensen.Unquote
open Integrator

[<Test>]
let ``boolean? works for booleans`` () =
    test <@ execute "(boolean? #t)" = "#t"  @>
    test <@ execute "(boolean? #f)" = "#t"  @>

[<Test>]
let ``boolean? works for non-booleans`` () =
    test <@ execute """(boolean? "hello")""" = "#f"  @>
    test <@ execute "(boolean? 3)" = "#f"  @>

[<Test>]
let ``number? works for numbers`` () =
    test <@ execute "(number? 3)" = "#t"  @>

[<Test>]
let ``number? works for non-numbers`` () =
    test <@ execute """(number? "hello")""" = "#f"  @>
    test <@ execute "(number? #t)" = "#f"  @>

[<Test>]
let ``string? works for strings`` () =
    test <@ execute """(string? "hi mum")""" = "#t"  @>
    test <@ execute """(string? "")""" = "#t"  @>

[<Test>]
let ``string? works for non-strings`` () =
    test <@ execute "(string? 3)" = "#f"  @>
    test <@ execute "(string? #t)" = "#f"  @>
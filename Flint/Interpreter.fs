module Interpreter

open SyntaxTree

let rec toString = function
    | Number(n) -> sprintf "%i" n
    | String(s) -> sprintf "%A" s
    | Symbol(s) -> sprintf "%s" s
    | Boolean(b) -> sprintf "%s" (if b then "#t" else "#f")
    | ExpList(expressions) -> sprintf "(%s)" (expressions |> expressionsToString)
and expressionsToString expressions =
    expressions |> List.map toString |> String.concat " "

let getNumber (Number(n)) = n

let numberReduction func args =
    args |> List.map getNumber |> (List.reduce func) |> Number

let environment =
    Map.empty
        .Add("+", (numberReduction (+)))
        .Add("-", (numberReduction (-)))
        .Add("/", (numberReduction (/)))
        .Add("*", (numberReduction (*)))
        .Add("cons", function head::ExpList(tail)::[] -> ExpList(head::tail))
        .Add("car", function head::_ -> head)
        .Add("cdr", function head::tail -> ExpList(tail))
        .Add(">", function Number(arg1)::Number(arg2)::[] -> Boolean(arg1 > arg2))
        .Add("<", function Number(arg1)::Number(arg2)::[] -> Boolean(arg1 < arg2))
        .Add("=", function Number(arg1)::Number(arg2)::[] -> Boolean(arg1 = arg2))
        .Add("r", function [] -> Number(10))

let rec evaluate = function
    | ExpList(Symbol("quote")::ExpList(rest)::[]) -> ExpList(rest)
    | ExpList(Symbol("if")::condition::trueCase::falseCase::[]) -> if (evaluate condition) = Boolean(true) then evaluate trueCase else evaluate falseCase
    | ExpList(Symbol(func)::arguments) -> environment.[func] (List.map evaluate arguments)
    | ExpList(expressions) -> failwith "list without application: %s" (expressions |> expressionsToString)
    | Symbol(s) -> if environment.ContainsKey(s) then environment.[s] [] else Symbol(s)
    | value -> value

let print expression = printfn "%s" (expression |> toString)
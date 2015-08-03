module Interpreter

open SyntaxTree
open Printer

let getNumber (Number(n)) = n

let numberReduction func args =
    args |> List.map getNumber |> (List.reduce func) |> Number

let initialEnvironment =
    Map.empty
        .Add("+", (numberReduction (+)))
        .Add("-", (numberReduction (-)))
        .Add("/", (numberReduction (/)))
        .Add("*", (numberReduction (*)))
        .Add("cons", function head::ExpList(tail)::[] -> ExpList(head::tail))
        .Add("car", function ExpList(head::_)::[] -> head)
        .Add("cdr", function ExpList(_::tail)::[]  -> ExpList(tail))
        .Add(">", function Number(arg1)::Number(arg2)::[] -> Boolean(arg1 > arg2))
        .Add("<", function Number(arg1)::Number(arg2)::[] -> Boolean(arg1 < arg2))
        .Add("=", function Number(arg1)::Number(arg2)::[] -> Boolean(arg1 = arg2))
        .Add("r", function [] -> Number(10))

let result (envirnment, expression) = expression

let rec evaluate (environment:Map<string, Expression List -> Expression>) = function
    | ExpList(Symbol("quote")::rest) -> environment, ExpList(rest)
    | ExpList(Symbol("define")::Symbol(name)::expression::_) -> environment.Add(name, (fun _ -> expression)), Nil
    | ExpList(Symbol("if")::condition::trueCase::falseCase::[]) -> 
        if (evaluate environment condition) |> result = Boolean(true) then 
            environment, (evaluate environment trueCase |> result) 
        else 
            environment, (evaluate environment falseCase |> result)
    | ExpList(Symbol(func)::arguments) -> 
        if environment.ContainsKey(func) then
            environment, environment.[func] (List.map (fun exp -> evaluate environment exp |> result) arguments)
        else
            failwith (sprintf "Function '%s' not found." func)
    | ExpList(expressions) -> failwith "list without application: %s" (expressions |> expressionsToString " ")
    | Symbol(s) -> 
        if environment.ContainsKey(s) then 
            environment, environment.[s] [] 
        else 
            failwith (sprintf "Symbol '%s' not found." s)
    | SeparateExpressions( expressions ) -> 
        expressions 
        |> List.fold (fun (environment,expressions) expression -> 
            let newEnv, result = evaluate environment expression
            newEnv,expressions@[result])
            (environment,[])
        |> (fun (env, expressions) -> env, SeparateExpressions(expressions))
    | value -> environment, value

let print expression = printfn "%s" (expression |> toString)
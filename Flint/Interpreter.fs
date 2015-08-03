module Interpreter

open SyntaxTree
open Printer

type EnvironmentDictionary = Map<string,(Expression list -> Expression)>

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

let rec evaluate (environment:EnvironmentDictionary) = function
    | ExpList(Symbol("if")::condition::trueCase::falseCase::[]) -> 
        if (evaluate environment condition) |> result = Boolean(true) then 
            environment, (evaluate environment trueCase |> result) 
        else 
            environment, (evaluate environment falseCase |> result)
    | ExpList(Symbol("quote")::rest) -> environment, ExpList(rest)
    | ExpList(Symbol("define")::Symbol(name)::expression::_) -> environment.Add(name, (fun _ -> expression)), Nil
    | ExpList(Symbol("lambda")::ExpList(formals)::ExpList(body)::_) -> environment, Procedure(formals, body)
    | ExpList(Procedure(formals, body)::arguments) ->

        // todo: evaluate arguments

        // add formals to temp environment, with values as provided by arguments
        let environmentWithArguments =
            List.zip formals arguments
            |> List.fold (fun (env:EnvironmentDictionary) (Symbol(formal),argument) -> 
                    env.Add( formal, (fun [] -> argument)))
                environment

        // evaluate procedure with temp environment
        evaluate environmentWithArguments <| ExpList(body)
    | ExpList(Symbol(func)::arguments) -> 
        if environment.ContainsKey(func) then
            environment, environment.[func] (List.map (fun exp -> evaluate environment exp |> result) arguments)
        else
            failwith (sprintf "Function '%s' not found." func)
    | ExpList(expression::[]) -> evaluate environment expression
    | ExpList(first::rest) -> 
        let updatedEnvironment, result = evaluate environment first
        evaluate updatedEnvironment <| ExpList(rest)
    //| ExpList(expressions) -> failwith (sprintf "list without application: %s" (expressions |> expressionsToString " "))
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
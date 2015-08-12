module Interpreter

open SyntaxTree
open Printer

type EnvironmentDictionary = Map<string,Expression>
type FunctionDictionary = Map<string,(Expression list -> EnvironmentDictionary -> Expression)>

let getNumber (Number(n)) = n

let numberReduction func args env =
    args |> List.map getNumber |> (List.reduce func) |> Number

let initialEnvironment =
    Map.empty
        .Add("+", Lambda("+"))
        .Add("-", Lambda("-"))
        .Add("/", Lambda("/"))
        .Add("*", Lambda("*"))
        .Add("cons", Lambda("cons"))
        .Add("car", Lambda("car"))
        .Add("cdr", Lambda("cdr"))
        .Add(">", Lambda(">"))
        .Add("<", Lambda("<"))
        .Add("=", Lambda("="))
        .Add("testValue", Number(10))

let (initialFunctions:FunctionDictionary) =
    Map.empty
        .Add("+", (numberReduction (+)))
        .Add("-", (numberReduction (-)))
        .Add("/", (numberReduction (/)))
        .Add("*", (numberReduction (*)))
        .Add("cons", fun args env -> match args with head::ExpList(tail)::[] -> ExpList(head::tail))
        .Add("car", fun args env -> match args with ExpList(head::_)::[] -> head)
        .Add("cdr", fun args env -> match args with ExpList(_::tail)::[] -> ExpList(tail))
        .Add(">", fun args env -> match args with Number(arg1)::Number(arg2)::[] -> Boolean(arg1 > arg2))
        .Add("<", fun args env -> match args with Number(arg1)::Number(arg2)::[] -> Boolean(arg1 < arg2))
        .Add("=", fun args env -> match args with Number(arg1)::Number(arg2)::[] -> Boolean(arg1 = arg2))

let result (envirnment, functions, expression) = expression

let rec evaluate (environment:EnvironmentDictionary) (functions:FunctionDictionary) = function
    | Symbol(symbolName) ->
        //printfn "SYmbol: %s" symbolName
        if environment.ContainsKey(symbolName) then 
            environment, functions, environment.[symbolName]
        else 
            failwith (sprintf "Symbol '%s' not found." symbolName)
    | ExpList(Symbol("if")::condition::trueCase::falseCase::[]) -> 
        if (evaluate environment functions condition) |> result = Boolean(true) then 
            environment, functions, (evaluate environment functions trueCase |> result) 
        else 
            environment, functions, (evaluate environment functions falseCase |> result)
//    | ExpList(Symbol("quote")::rest) -> environment, functions, ExpList(rest) // TODO: work out how to stop evaluation with quoted lists.  Make quoted an AST concept?
    | ExpList(Symbol("define")::Symbol(name)::expression::_) -> environment.Add(name, expression), functions, Nil
    | ExpList(Symbol("lambda")::ExpList(formals)::body::[]) -> 
        environment, 
        functions.Add("lambda1", 
            fun args env -> 
                let environmentWithArguments =
                    List.zip formals args
                    |> List.fold (fun (lambdaEnv:EnvironmentDictionary) (Symbol(name), value) -> lambdaEnv.Add(name, value)) env
                evaluate environmentWithArguments functions body |> result), 
        Lambda("lambda1")
    | ExpList(Lambda(lambdaName)::arguments) -> 
        if functions.ContainsKey(lambdaName) then
            //printfn "Lambda: %s %A" lambdaName arguments
            environment, functions, functions.[lambdaName] arguments environment
        else
            failwith (sprintf "Function '%s' not found." lambdaName)
    | SeparateExpressions( expressions ) -> 
        expressions 
        |> List.fold (fun (environment,functions,expressions) expression -> 
            let newEnv, newFunc, result = evaluate environment functions expression
            newEnv,newFunc,expressions@[result])
            (environment,functions,[])
        |> (fun (env, func, expressions) -> env, func, SeparateExpressions(expressions))
    | ExpList(expressions) -> 
        //printfn "ExpList: %A" expressions

        let updatedEnvironment, updatedFunctions, evaluatedExpressions = 
            expressions 
            |> List.fold (fun (environment,functions,expressions) expression -> 
                let newEnv, newFunc, result = evaluate environment functions expression
                newEnv,newFunc,expressions@[result])
                (environment,functions,[])
            |> (fun (env, func, expressions) -> env, func, ExpList(expressions))

        evaluatedExpressions |> evaluate updatedEnvironment updatedFunctions
    | value -> 
        //printfn "Value: %A" value
        environment, functions, value

let print expression = printfn "%s" (expression |> toString)

// ** TODO **

// - Get quotations working
// - Implement core library in scheme
// - IDE
// - Logging
// - Better way of handling evaluation errors (error as AST concept?)
// - Refactor evaluation code

// ** To Try **

// - Eager vs Lazy evaluation
// - types
// - Minimal primitives
// - Write parser by hand
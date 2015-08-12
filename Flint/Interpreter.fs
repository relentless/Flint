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
        if not (environment |> Map.containsKey symbolName) then failwith (sprintf "Symbol '%s' not found." symbolName)
        environment, functions, environment.[symbolName]
    | ExpList(Symbol("if")::condition::trueCase::falseCase::[]) -> 
        let caseToUse = selectCase environment functions condition trueCase falseCase
        environment, functions, (evaluate environment functions caseToUse |> result)
//    | ExpList(Symbol("quote")::rest) -> environment, functions, ExpList(rest) // TODO: work out how to stop evaluation with quoted lists.  Make quoted an AST concept?
    | ExpList(Symbol("define")::Symbol(name)::expression::[]) -> environment.Add(name, expression), functions, Nil
    | ExpList(Symbol("lambda")::ExpList(formals)::body::[]) -> 
        environment, functions.Add("lambda1", createLambdaFunction functions formals body), Lambda("lambda1")
    | ExpList(Lambda(lambdaName)::arguments) -> 
        if not (functions |> Map.containsKey lambdaName) then failwith (sprintf "Lambda '%s' not found." lambdaName)
        environment, functions, functions.[lambdaName] arguments environment
    | SeparateExpressions( expressions ) -> expressions |> foldExpressionList environment functions SeparateExpressions
    | ExpList(expressions) -> 
        let updatedEnvironment, updatedFunctions, evaluatedExpressions = expressions |> foldExpressionList environment functions ExpList
        evaluatedExpressions |> evaluate updatedEnvironment updatedFunctions
    | value -> environment, functions, value

and foldExpressionList environment functions expressionContainer expressions =
    expressions 
    |> List.fold (fun (environment,functions,expressions) expression -> 
        let newEnv, newFunc, result = evaluate environment functions expression
        newEnv,newFunc,expressions@[result])
        (environment,functions,[])
    |> (fun (env, func, expressions) -> env, func, expressionContainer expressions)

and selectCase environment functions condition trueCase falseCase =
    match (evaluate environment functions condition) |> result with
        | Boolean(true) -> trueCase
        | Boolean(false) -> falseCase
        | _ -> failwith "invalid If case"

and createLambdaFunction functions formals body =
    fun args env -> 
        let environmentWithArguments =
            List.zip formals args
            |> List.fold (fun (lambdaEnv:EnvironmentDictionary) (Symbol(name), value) -> lambdaEnv.Add(name, value)) env
        evaluate environmentWithArguments functions body |> result

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
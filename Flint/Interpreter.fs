module Interpreter

open SyntaxTree
open Printer

type EnvironmentDictionary = Map<string,Expression>
type FunctionDictionary = Map<string,(Expression list -> EnvironmentDictionary -> Expression)>

type EvaluationRecord = { Expression: Expression; Environment: EnvironmentDictionary; Functions: FunctionDictionary }

let integrate environment functions expression =
    {Expression = expression; Environment = environment; Functions = functions }

let numberReduction func args env =
    let getNumber (Number(n)) = n
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

let result evaluationRecord = evaluationRecord.Expression

let rec evaluate evaluationRecord =

    match evaluationRecord.Expression with
    | Symbol(symbolName) ->
        if not (evaluationRecord.Environment |> Map.containsKey symbolName) then failwith (sprintf "Symbol '%s' not found." symbolName)
        {evaluationRecord with Expression = evaluationRecord.Environment.[symbolName]}
    | ExpList(Symbol("if")::condition::trueCase::falseCase::[]) -> 
        let caseToUse = selectCase evaluationRecord.Environment evaluationRecord.Functions condition trueCase falseCase
        evaluate {evaluationRecord with Expression = caseToUse}
//    | ExpList(Symbol("quote")::rest) -> environment, functions, ExpList(rest) // TODO: work out how to stop evaluation with quoted lists.  Make quoted an AST concept?
    | ExpList(Symbol("define")::Symbol(name)::expression::[]) -> 
        {evaluationRecord with Expression = Nil; Environment = evaluationRecord.Environment.Add(name, expression) }
    | ExpList(Symbol("lambda")::ExpList(formals)::body::[]) -> 
        {evaluationRecord with Expression = Lambda("lambda1"); Functions = evaluationRecord.Functions.Add("lambda1", createLambdaFunction evaluationRecord.Functions formals body)}
    | ExpList(Lambda(lambdaName)::arguments) -> 
        if not (evaluationRecord.Functions |> Map.containsKey lambdaName) then failwith (sprintf "Lambda '%s' not found." lambdaName)
        {evaluationRecord with Expression = evaluationRecord.Functions.[lambdaName] arguments evaluationRecord.Environment}
    | SeparateExpressions( expressions ) -> expressions |> foldExpressionList evaluationRecord.Environment evaluationRecord.Functions SeparateExpressions
    | ExpList(expressions) -> 
        expressions 
        |> foldExpressionList evaluationRecord.Environment evaluationRecord.Functions ExpList
        |> evaluate
    | value -> evaluationRecord

and foldExpressionList environment functions expressionContainer expressions =
    expressions 
    |> List.fold (fun (environment,functions,expressions) expression -> 
        let newRecord = evaluate {Expression = expression; Environment = environment; Functions = functions }
        newRecord.Environment,newRecord.Functions,expressions@[newRecord.Expression])
        (environment,functions,[])
    |> (fun (env, func, expressions) -> {Expression = expressionContainer expressions; Environment = env; Functions = func} )

and selectCase environment functions condition trueCase falseCase =
    match (evaluate {Expression = condition; Environment = environment; Functions = functions}).Expression with
        | Boolean(true) -> trueCase
        | Boolean(false) -> falseCase
        | _ -> failwith "invalid If case"

and createLambdaFunction functions formals body =
    fun args env -> 
        let environmentWithArguments =
            List.zip formals args
            |> List.fold (fun (lambdaEnv:EnvironmentDictionary) (Symbol(name), value) -> lambdaEnv.Add(name, value)) env
        (evaluate {Expression = body; Environment = environmentWithArguments; Functions = functions}).Expression

let print expression = printfn "%s" (expression |> toString)

// ** TODO **

// - Get quotations working.  (Have Application and QuotedList as AST concepts?  Could output from parser.)
// - Implement core library in scheme
// - IDE
// - Logging
// - Better way of handling evaluation errors (error as AST concept?)
// - Make printer not output newlines for Nil expressions
// - Implement numbering for multiple lambdas

// ** To Try **

// - Eager vs Lazy evaluation
// - types
// - Minimal primitives
// - Write parser by hand
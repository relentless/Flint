module Interpreter

open Parser
open SyntaxTree
open Printer
open CoreFunctions
open System.IO

let integrate environment functions expression =
    {Expression = expression; Environment = environment; Functions = functions }

let result evaluationRecord = evaluationRecord.Expression

let rec evaluate evaluationRecord =

    match evaluationRecord.Expression with
    | Symbol(symbolName) ->
        if not (evaluationRecord.Environment |> Map.containsKey symbolName) 
        then failwith (sprintf "Symbol '%s' not found." symbolName)
        {evaluationRecord with Expression = evaluationRecord.Environment.[symbolName]}
    | ExpList(Symbol("if")::condition::trueCase::falseCase::[]) -> 
        let caseToUse = selectCase evaluationRecord.Environment evaluationRecord.Functions condition trueCase falseCase
        evaluate {evaluationRecord with Expression = caseToUse}
    | ExpList(Symbol("quote")::ExpList(expressions)::[]) -> 
        {evaluationRecord with Expression = QuotedList(expressions)}
    | ExpList(Symbol("define")::Symbol(name)::expression::[]) -> 
        {evaluationRecord with Expression = Nil; Environment = evaluationRecord.Environment.Add(name, expression) }
    | ExpList(Symbol("lambda")::formals::body) -> 
        evaluationRecord |> createLambda formals body
    | ExpList(Lambda(lambdaName)::arguments) -> 
        if not (evaluationRecord.Functions |> Map.containsKey lambdaName) 
        then failwith (sprintf "Lambda '%s' not found." lambdaName)
        {evaluationRecord with Expression = evaluationRecord.Functions.[lambdaName] arguments evaluationRecord.Environment}
    | SeparateExpressions( expressions ) -> 
        expressions |> foldExpressionList evaluationRecord.Environment evaluationRecord.Functions SeparateExpressions
    | MultipleExpressions( expressions ) -> 
        expressions |> getLastExpression evaluationRecord.Environment evaluationRecord.Functions
    | ExpList(expressions) -> 
        expressions 
        |> foldExpressionList evaluationRecord.Environment evaluationRecord.Functions ExpList
        |> evaluate
    | _ -> evaluationRecord

and foldExpressionList environment functions expressionContainer expressions =
    expressions 
    |> List.fold (fun (environment,functions,expressions) expression -> 
        let newRecord = evaluate {Expression = expression; Environment = environment; Functions = functions }
        newRecord.Environment,newRecord.Functions,expressions@[newRecord.Expression])
        (environment,functions,[])
    |> (fun (env, func, expressions) -> {Expression = expressionContainer expressions; Environment = env; Functions = func} )

and getLastExpression environment functions expressions =
    expressions 
    |> List.fold (fun (environment,functions,expressions) expression -> 
        let newRecord = evaluate {Expression = expression; Environment = environment; Functions = functions }
        newRecord.Environment,newRecord.Functions,newRecord.Expression)
        (environment,functions,Nil)
    |> (fun (env, func, expressions) -> {Expression = expressions; Environment = env; Functions = func} )

and selectCase environment functions condition trueCase falseCase =
    match (evaluate {Expression = condition; Environment = environment; Functions = functions}).Expression with
        | Boolean(true) -> trueCase
        | Boolean(false) -> falseCase
        | _ -> failwith "invalid If case"

and createLambda formals body evaluationRecord =
    let lambdaId = "lambda-" + System.Guid.NewGuid().ToString()
    let evaluationFunction = createLambdaFunction formals evaluationRecord.Functions (MultipleExpressions(body))
    {evaluationRecord with Expression = Lambda(lambdaId); Functions = evaluationRecord.Functions.Add(lambdaId, evaluationFunction)}

and createLambdaFunction formals functions body =
    fun args env -> 
        let environmentWithArguments =
            match formals with
            | Symbol(formalArgs) -> 
                env.Add(formalArgs, QuotedList(args))
            | ExpList(formalArgs) ->
                List.zip formalArgs args
                |> List.fold (fun (lambdaEnv:EnvironmentDictionary) (Symbol(name), value) -> lambdaEnv.Add(name, value)) env
        (evaluate {Expression = body; Environment = environmentWithArguments; Functions = functions}).Expression

let print expression = printfn "%s" (expression |> toString)

let loadCoreLib() =
    File.ReadAllText("CoreLib.flint")
    |> parse
    |> integrate initialEnvironment initialFunctions
    |> evaluate

let execute text =
    {loadCoreLib() with Expression = text |> parse}
    |> evaluate 
    |> result
    |> toString 

// ** TODO **

// - Add cond
// - Add let
// - Check closures
// - Implement core library in scheme
// - Add comments
// - Better way of handling evaluation errors (error as AST concept?)
// - Add Define syntax sugar
// - Add VarArgs syntax sugar

// ** To Try **

// - Eager vs Lazy evaluation
// - types
// - Minimal primitives
// - Write parser by hand
// - Code Golf version
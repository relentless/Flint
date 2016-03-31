﻿module Evaluator

open SyntaxTree
open CoreFunctions

// Evaluates an expression in the context of an environment
let rec evaluate evaluationRecord =

    match evaluationRecord.Expression with
    | Symbol(symbolName) ->
        if not (evaluationRecord.Environment |> Map.containsKey symbolName) then failwith (sprintf "Symbol '%s' not found." symbolName)
        {evaluationRecord with Expression = evaluationRecord.Environment.[symbolName]}
    | ExpList([Symbol("if");condition;trueCase;falseCase]) -> 
        let caseToUse = selectCase evaluationRecord.Environment evaluationRecord.Functions condition trueCase falseCase
        evaluate {evaluationRecord with Expression = caseToUse}
    | ExpList([Symbol("define");Symbol(name);expression]) -> 
        {evaluationRecord with Expression = Nil; Environment = evaluationRecord.Environment.Add(name, expression) }
    | ExpList(Symbol("lambda")::formals::body) -> 
        let lambdaId = "lambda-" + System.Guid.NewGuid().ToString()
        let evaluationFunction = createLambdaFunction formals evaluationRecord.Functions (MultipleExpressions(body))
        {evaluationRecord with Expression = Lambda(lambdaId); Functions = evaluationRecord.Functions.Add(lambdaId, evaluationFunction)}
    | ExpList(Lambda(lambdaName)::arguments) -> 
        if not (evaluationRecord.Functions |> Map.containsKey lambdaName) then failwith (sprintf "Lambda '%s' not found." lambdaName)
        {evaluationRecord with Expression = evaluationRecord.Functions.[lambdaName] arguments evaluationRecord.Environment}
    | SeparateExpressions( expressions ) -> 
        expressions |> evaluateExpressionList evaluationRecord.Environment evaluationRecord.Functions SeparateExpressions
    | MultipleExpressions( expressions ) -> 
        expressions |> getLastExpression evaluationRecord.Environment evaluationRecord.Functions
    | ExpList(expressions) -> 
        expressions |> evaluateExpressionList evaluationRecord.Environment evaluationRecord.Functions ExpList |> evaluate
    | _ -> evaluationRecord

// evaluates each expression in a list and returns them in the provided container
and evaluateExpressionList environment functions expressionContainer expressions =
    expressions 
    |> List.fold (fun (environment,functions,expressions) expression -> 
        let newRecord = evaluate {Expression = expression; Environment = environment; Functions = functions }
        newRecord.Environment,newRecord.Functions,expressions@[newRecord.Expression])
        (environment,functions,[])
    |> (fun (env, func, expressions) -> {Expression = expressionContainer expressions; Environment = env; Functions = func} )

// evaluates each expression in a list but returns only the last one
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

and createLambdaFunction formals functions body =
    fun args env -> 
        let environmentWithArguments =
            match formals with
            | Symbol(formalArgs) -> env.Add(formalArgs, QuotedList(args))
            | ExpList(formalArgs) ->
                List.zip formalArgs args
                |> List.fold (fun (lambdaEnv:EnvironmentDictionary) (Symbol(name), value) -> lambdaEnv.Add(name, value)) env
        (evaluate {Expression = body; Environment = environmentWithArguments; Functions = functions}).Expression


// ** TODO **

// - Fix parser (space between lists)
// - Implement core library in scheme
// - Add comments
// - Better way of handling evaluation errors (error as AST concept?)
// - Add VarArgs syntax sugar for Define
// - Try active pattern matching in interpreter
// - Split interpreter into separate step for transformations and evaluation? (e.g. cond -> if, then evaluate if)

// ** To Try **


// - Separate parse tree
// - Separate evaluation stages
// - Eager vs Lazy evaluation
// - Typed Scheme
// - Minimal primitives
// - Write parser by hand
// - Code Golf version
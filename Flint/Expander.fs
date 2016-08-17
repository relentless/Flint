module Expander

open SyntaxTree

// Converts expressions in AST to expanded versions, e.g cond -> ifs
let rec expand x = match x with
    | ExpList([Symbol("if");condition;trueCase]) -> ExpList([Symbol("if");condition;trueCase;Nil]) |> expand
    | ExpList(Symbol("cond")::parts) -> condToIf(parts) |> expand
    | ExpList([Symbol("quote");ExpList(expressions)]) -> QuotedList(expressions) |> expand
    | ExpList([Symbol("define");ExpList(Symbol(lambdaName)::lambdaFormals);expression]) -> 
        let longhandLambda = ExpList([Symbol("define");Symbol(lambdaName);ExpList([Symbol("lambda");ExpList(lambdaFormals);expression])])
        longhandLambda |> expand
    | ExpList([Symbol("let");ExpList(assignments); expressionsUsingValues]) -> letToLambda assignments expressionsUsingValues
    | ExpList([Symbol("eval");ExpList([Symbol("quote");ExpList(expressions)])]) -> ExpList(expressions) |> expand
    | ExpList([Symbol("eval");ExpList([Symbol("quote");ExpList(expressions)])]) -> ExpList(expressions) |> expand
    | SeparateExpressions(expressions) -> expressions |> expandExpressionList SeparateExpressions
    | MultipleExpressions(expressions) -> expressions |> expandExpressionList MultipleExpressions
    | ExpList(expressions) -> expressions |> expandExpressionList ExpList
    | expandedExpression -> expandedExpression

and condToIf = function
    | ExpList([Symbol("else");value])::[] -> ExpList([Symbol("if");Boolean(true);value])
    | ExpList([Symbol("else");value])::somethingElse -> failwith "Invalid 'cond' statement - 'else' expression must be last"
    | ExpList([cond;value])::[] -> ExpList([Symbol("if");cond;value])
    | ExpList([cond;value])::rest -> ExpList([Symbol("if");cond;value;condToIf(rest)])
    | [] -> failwith "Error expanding 'cond' statement"

and letToLambda assignments expressionsUsingValues =
    match assignments with
    | ExpList([label;value])::furtherAssignments -> ExpList([ExpList([Symbol("lambda"); ExpList([label]); letToLambda furtherAssignments expressionsUsingValues]); value])
    | [] -> expressionsUsingValues

and expandExpressionList expressionContainer expressions =
    expressions 
    |> List.fold (fun expandedExpressions expression -> expandedExpressions@[expand expression]) []
    |> (fun expressions -> expressionContainer expressions)
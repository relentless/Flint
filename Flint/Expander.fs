module Expander

open SyntaxTree

// Converts expressions in AST to expanded versions, e.g cond -> ifs
let rec expand = function
    | ExpList([Symbol("if");condition;trueCase]) -> ExpList([Symbol("if");condition;trueCase;Nil]) |> expand
    | ExpList(Symbol("cond")::parts) -> condToIf(parts) |> expand
    | ExpList([Symbol("quote");ExpList(expressions)]) -> QuotedList(expressions) |> expand
    | ExpList([Symbol("define");ExpList(Symbol(lambdaName)::lambdaFormals);expression]) -> 
        let longhandLambda = ExpList([Symbol("define");Symbol(lambdaName);ExpList([Symbol("lambda");ExpList(lambdaFormals);expression])])
        longhandLambda |> expand
    | ExpList([Symbol("let");ExpList(assignments); expressionsUsingValues]) -> letToLambda assignments expressionsUsingValues
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
    | ExpList([label;value])::[] -> ExpList([ExpList([Symbol("lambda"); ExpList([label]); expressionsUsingValues]); value])

and expandExpressionList expressionContainer expressions =
    expressions 
    |> List.fold (fun expandedExpressions expression -> expandedExpressions@[expand expression]) []
    |> (fun expressions -> expressionContainer expressions)

// Let implemented as lambda:

// (let ((x 10))
//  (+ x 3))

//((lambda (x)
//   (+ x 3))
// 10)
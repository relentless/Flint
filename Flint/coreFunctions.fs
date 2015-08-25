module CoreFunctions

open SyntaxTree

type EnvironmentDictionary = Map<string,Expression>
type FunctionDictionary = Map<string,(Expression list -> EnvironmentDictionary -> Expression)>

type EvaluationRecord = { Expression: Expression; Environment: EnvironmentDictionary; Functions: FunctionDictionary }

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
        .Add("eq?", Lambda("eq?"))

let (initialFunctions:FunctionDictionary) =
    Map.empty
        .Add("+", (numberReduction (+)))
        .Add("-", (numberReduction (-)))
        .Add("/", (numberReduction (/)))
        .Add("*", (numberReduction (*)))
        .Add("cons", fun args env -> match args with head::QuotedList(tail)::[] -> QuotedList(head::tail))
        .Add("car", fun args env -> match args with QuotedList(head::_)::[] -> head)
        .Add("cdr", fun args env -> match args with QuotedList(_::tail)::[] -> QuotedList(tail))
        .Add(">", fun args env -> match args with Number(arg1)::Number(arg2)::[] -> Boolean(arg1 > arg2))
        .Add("<", fun args env -> match args with Number(arg1)::Number(arg2)::[] -> Boolean(arg1 < arg2))
        .Add("=", fun args env -> match args with Number(arg1)::Number(arg2)::[] -> Boolean(arg1 = arg2))
        .Add("eq?", fun args env -> match args with arg1::arg2::[] -> Boolean(arg1 = arg2))

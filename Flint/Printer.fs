module Printer

open SyntaxTree

let rec toString = function
    | Number(n) -> sprintf "%i" n
    | String(s) -> sprintf "%A" s
    | Symbol(s) -> sprintf "%s" s
    | Boolean(b) -> sprintf "%s" (if b then "#t" else "#f")
    | ExpList(expressions) -> sprintf "(%s)" (expressions |> expressionsToString)
    | QuotedList(expressions) -> sprintf "'(%s)" (expressions |> expressionsToString)
    | SeparateExpressions(expressions) -> sprintf "%s" (expressions |> separateExpressionsToString)
    | Lambda(l) -> sprintf "#<procedure:%s>" l
    | Nil -> ""
and expressionsToString expressions =
    expressions |> List.map toString |> String.concat " "
and separateExpressionsToString expressions =
    expressions 
    |> List.fold
        (fun output exp -> 
            let stringExpression = exp |> toString 
            match output with 
            | "" -> stringExpression
            | _ -> output + "\n" + stringExpression)
        ""
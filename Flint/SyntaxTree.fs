module SyntaxTree

type Expression =
    | Number of int
    | String of string
    | Symbol of string
    | Boolean of bool
    | ExpList of Expression list
    | SeparateExpressions of Expression list
    | QuotedList of Expression list
    | Lambda of name: string
    | Nil
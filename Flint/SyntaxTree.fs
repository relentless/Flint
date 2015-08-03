module SyntaxTree

type Expression =
    | Number of int
    | String of string
    | Symbol of string
    | Boolean of bool
    | ExpList of Expression list
    | SeparateExpressions of Expression list
    | Procedure of formals: Expression list * body: Expression list
    | Nil
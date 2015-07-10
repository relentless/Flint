module SyntaxTree

type Expression =
    | Number of int
    | String of string
    | Symbol of string
    | Boolean of bool
    | ExpList of Expression list
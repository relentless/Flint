module SyntaxTree

type Expression =
    | Number of int
    | String of string
    | Symbol of string
    | Boolean of bool
    | ExpList of Expression list
    | SeparateExpressions of Expression list
    | Procedure of formals: Arguments * body: ProcedureBody
    //| CompiledProcedure of formals: Arguments * call: (Expression list -> Expression)
    | Nil
and Arguments = Expression list
and ProcedureBody = Expression list
//and ProcedureFunction = (Expression list -> Expression)
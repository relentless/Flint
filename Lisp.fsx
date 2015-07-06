#r "lib/fparseccs.dll"
#r "lib/fparsec.dll"

open FParsec

// AST
type AtomType =
    | Number of float
    | String of string
    // Symbol
    // Boolean

type Expression =
    | Atom of AtomType
    | Pair of Expression * Expression
    | Exp of Expression list

// Interpreter
let rec evaluate = function
    | Atom(value) -> Atom(value)
    | Pair(exp1, exp2) -> Pair(evaluate exp1, evaluate exp2)
    | Exp([]) -> Exp([])
    | Exp(first::rest) -> Exp(evaluate first::[evaluate <| Exp(rest)])

let print expression =
    let rec toString = function
        | Atom(value) -> 
            match value with
            | Number(n) -> sprintf "%f" n
            | String(s) -> sprintf "%A" s
        | Pair(exp1, exp2) -> sprintf "(%s . %s)" (exp1 |> toString) (exp2 |> toString)
        | Exp([]) -> sprintf "()"
        | Exp(first::rest) -> sprintf "(%s %s)" (first |> toString) (Exp(rest) |> toString)
        // don't print all brackets for expression lists

    printfn "%s" (expression |> toString)

Exp([Atom(String("hi")); Atom(Number(3.0))])
//Pair(Atom(Number(3.0)), Atom(String("Hi")))
|> evaluate
|> print

// Parser
let eq = pstring "(eq" >>. spaces1 >>. pfloat
//>>. spaces1 >>. pfloat >>. pstring ")"
let peq = eq |>> fun x -> Func("eq", Sexp(Atom(Number(x)), Atom(Number(x))))

let parse code =
    match run peq code with
    | Success(result,_,_) -> result
    | Failure(msg,_,_) -> failwith msg

execute (parse "(eq 3")

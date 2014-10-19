type Value =
| Number of int
| Text of string
| List of Value * Value
| Bool of bool

let rec print = function
| Number(i) -> i.ToString()
| Text(t) -> t
| List(val1,val2) -> "(" + print val1 + " " + print val2 + ")"
| Bool(b) -> if b then "T" else "NIL"

type Exp =
| Atom of Value
| Sexp of Exp * Exp
| Func of string * Exp

let rec exec = function
| Atom x -> x
| Sexp(exp1,exp2) -> List(exec exp1, exec exp2)
| Func(name, exp) -> 
    match name, exp with
    | "car", Sexp(exp1,exp2) -> exec exp1
    | "cdr", Sexp(exp1,exp2) -> exec exp2
    | "cons", Sexp(exp1,exp2) -> exec <| Sexp(exp1,exp2)
    | "eq", Sexp(exp1,exp2) -> if exp1 = exp2 then Bool(true) else Bool(false)
    | "atom", Atom(_) -> Bool(true)
    | "atom", _ -> Bool(false)
    | x, exp -> failwith ("Function '" + x + "' not recognised, or incorrect parameters used")

let execute expression = 
    printfn "%s" <| print(exec expression)

execute <| Func("eq", Sexp(Atom(Number(3)), Atom(Number(3))))

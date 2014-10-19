type Value = int

type Exp =
| Atom of Value
| Sexp of Exp * Exp
| Car of Exp * Exp
| Cdr of Exp * Exp
| Cons of Exp * Exp
| Eq of Exp * Exp
| IsAtom of Exp

let rec execute = function
| Atom x -> x.ToString()
| Sexp(exp1,exp2) -> "(" + execute exp1 + " " + execute exp2 + ")"
| Car(exp1,exp2) -> execute exp1
| Cdr(exp1,exp2) -> execute exp2
| Cons(exp1,exp2) -> execute <| Sexp(exp1,exp2)
| Eq(exp1,exp2) -> if exp1 = exp2 then "T" else "NIL"
| IsAtom(Atom(_)) -> "T"
| IsAtom(_) -> "NIL"

execute <| Sexp( Atom(5), Car( Cons( Atom(6), Atom(50) ), Atom(3)) )

execute <| Sexp( Eq( Atom(6), Atom(6)), Eq( Atom(6), Atom(7)))

execute <| Sexp( IsAtom(Atom(5)), IsAtom(Sexp(Atom(8), Atom(7))))

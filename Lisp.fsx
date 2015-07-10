#r "lib/fparseccs.dll"
#r "lib/fparsec.dll"

open FParsec

// AST
type Expression =
    | Number of int
    | String of string
    | Symbol of string
    | Boolean of bool
    | ExpList of Expression list

// Interpreter
let rec toString = function
    | Number(n) -> sprintf "%i" n
    | String(s) -> sprintf "%A" s
    | Symbol(s) -> sprintf "%s" s
    | Boolean(b) -> sprintf "%s" (if b then "#t" else "#f")
    | ExpList(expressions) -> sprintf "(%s)" (expressions |> expressionsToString)
and expressionsToString expressions =
    expressions |> List.map toString |> String.concat " "

let getNumber (Number(n)) = n

let numberReduction func args =
    args |> List.map getNumber |> (List.reduce func) |> Number

let environment =
    Map.empty
        .Add("+", (numberReduction (+)))
        .Add("-", (numberReduction (-)))
        .Add("/", (numberReduction (/)))
        .Add("*", (numberReduction (*)))
        .Add("cons", function head::ExpList(tail)::[] -> ExpList(head::tail))
        .Add("car", function head::_ -> head)
        .Add("cdr", function head::tail -> ExpList(tail))
        .Add(">", function Number(arg1)::Number(arg2)::[] -> Boolean(arg1 > arg2))
        .Add("<", function Number(arg1)::Number(arg2)::[] -> Boolean(arg1 < arg2))
        .Add("=", function Number(arg1)::Number(arg2)::[] -> Boolean(arg1 = arg2))
        .Add("r", function [] -> Number(10))

let rec evaluate = function
    | ExpList(Symbol("quote")::ExpList(rest)::[]) -> ExpList(rest)
    | ExpList(Symbol("if")::condition::trueCase::falseCase::[]) -> if (evaluate condition) = Boolean(true) then evaluate trueCase else evaluate falseCase
    | ExpList(Symbol(func)::arguments) -> environment.[func] (List.map evaluate arguments)
    | ExpList(expressions) -> failwith "list without application: %s" (expressions |> expressionsToString)
    | Symbol(s) -> if environment.ContainsKey(s) then environment.[s] [] else Symbol(s)
    | value -> value

let print expression = printfn "%s" (expression |> toString)

// Parser
let parseExpression, parseExpressionRef = createParserForwardedToRef()

let chr c = skipChar c
let symbol = anyOf "!$%&|*+-/:<=>?@^_~#"

let parseTrue = pstring "#t" |>> (fun _ -> Boolean(true))
let parseFalse = pstring "#f" |>> (fun _ -> Boolean(false))

let parseSymbol = parse {
        let! first = letter <|> symbol
        let! rest = manyChars (letter <|> symbol <|> digit)
        return Symbol(first.ToString() + rest)
}

let parseString = parse {
    do! chr '"'
    let! xs = manyChars (noneOf "\"")
    do! chr '"'
    return String(xs)
}

let parseNumber = many1Chars digit |>> (fun num -> Number(System.Int32.Parse num))
let parseQuoted = chr '\'' >>. parseExpression |>> fun expr -> ExpList([Symbol("quote"); expr])
let parseList = chr '(' >>. sepBy parseExpression spaces1 .>> chr ')' |>> ExpList

do parseExpressionRef := 
    parseTrue <|> 
    parseFalse <|> 
    parseSymbol <|> 
    parseString <|> 
    parseNumber <|> 
    parseQuoted <|> 
    parseList

let parse text =
    match run parseExpression text with
    | Success(result,_,_) -> result
    | Failure(msg,_,_) -> failwith msg

// tests

//ExpList([String("hi"); Number(3)])
//Pair(Atom(Number(3.0)), Atom(String("Hi")))
//ExpList([Symbol("+"); Number(3); Number(5)])
//|> evaluate
//|> print

"(/ 100 2 5)"
"(quote #t #f 99 \"Hi Mum\")"
"""(cons "Hi Mum" '(1 2 3))"""
"(cdr 1 2 3)"
"""(if (> 3 4) "Hi Mum" "Hi Dad")"""
"r"
|> parse
|> evaluate
|> print


"(quote #t #f 99 \"Hi Mum\")"
|> parse
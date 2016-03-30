module Parser

open FParsec
open SyntaxTree

let parseExpression, parseExpressionRef = createParserForwardedToRef()
let parseSingleExpression, parseSingleExpressionRef = createParserForwardedToRef()

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
let parseList = chr '(' >>. sepBy parseSingleExpression spaces1 .>> chr ')' |>> ExpList
let parseQuoted = chr '\'' >>. parseList |>> (fun expList -> ExpList(Symbol("quote")::[expList]))

do parseSingleExpressionRef :=
    parseQuoted <|>
    parseList <|>
    parseTrue <|> 
    parseFalse <|> 
    parseSymbol <|> 
    parseString <|> 
    parseNumber
    
do parseExpressionRef := sepBy parseSingleExpression spaces1 |>> SeparateExpressions

// converts text into an Expression
let parse text =
    match run parseExpression text with
    | Success(result,_,_) -> result
    | Failure(msg,_,_) -> failwith msg
﻿module Parser

open FParsec
open SyntaxTree

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
#nowarn "40"

namespace FScheme.Core

open System.IO
open FParsec

type LispParser = Parser<Lisp, unit>

module Parser =
    let number = attempt (pint32 .>> notFollowedBy (pchar '.') |>> Integer) <|>  (pfloat |>> Float) |>> Lisp.Number

    let normalIdentifier = many1Chars (choice [letter; pchar '/'; pchar '-'; pchar '?'; pchar '.'])

    let identifier =
        let specialOperator = ["+"; "-"; "*"; "/"; ">"; "<"; ">="; "<="; "=="] |> List.map pstring |> choice
        attempt normalIdentifier  <|> specialOperator

    let pSingleLineComment:Parser<string, unit> =
        spaces >>. pstring ";" >>. many1CharsTill anyChar newline .>> newline

    let nil =
        pstring "'()" >>. preturn Lisp.Nil

    let atom =
        identifier |>> Lisp.Atom

    let stringLiteral: Parser<Lisp, unit> =
        let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
        let unescape c = match c with
                         | 'n' -> '\n'
                         | 'r' -> '\r'
                         | 't' -> '\t'
                         | c   -> c
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
        let s = between (pstring "\"") (pstring "\"")
                        (manyChars (normalChar <|> escapedChar))
        s |>> Lisp.Text

    let hashVal =
            pchar '#' >>. choice [
                pchar 't' >>. preturn (Lisp.Bool true);
                pchar 'f' >>. preturn (Lisp.Bool false);
            ]

    let rec lispVal = parse.Delay (fun () ->
        choice [
            hashVal;
            nil;
            number;
            atom;
            stringLiteral;
            quotedLispVal;
            listVal;
        ])

    and parens = between (pchar '(') (pchar ')')

    and quoted p = pchar '\'' >>? p

    and quotedLispVal = quoted lispVal |>> fun x -> Lisp.List [Lisp.Atom "quote"; x]

    and listVal = parens (spaces >>. (sepEndBy lispVal spaces)) |>> Lisp.List

    and application : Parser<Application, unit> = spaces >>. (many (listVal .>> spaces)) .>> spaces .>> eof

    and readExpr source =
        match run (spaces >>. lispVal .>> spaces .>> eof) source with
        | Success (ast, _, _) -> ast
        | Failure (err, _, _) -> PError err |> throwException

    and readContent source =
        match run application source with
        | Success (ast, _, _) -> ast
        | Failure (err, _, _) -> PError err |> throwException

    and readFile (filepath: string) =
        use stream = new StreamReader(filepath)
        let content = stream.ReadToEnd()
        readContent content
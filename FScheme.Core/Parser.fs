#nowarn "40"

namespace FScheme.Core

open FParsec

type LispParser = Parser<Lisp, unit>

module Parser =
    let number = attempt (pint32 .>> notFollowedBy (pchar '.') |>> Integer) <|>  (pfloat |>> Float) |>> Lisp.Number

    let normalIdentifier: Parser<string, unit> =
        let specialChar = pstring "/" <|> pstring "."
        many1Chars letter .>>. opt (many1 (specialChar .>>. many1Chars letter))
            |>> (fun (prefix, suffix) ->
                    let mutable s = prefix
                    if not suffix.IsNone then
                        for (splitter, rest) in suffix.Value do
                            s <- s + splitter + rest
                    s
                )

    let identifier =
        let specialOperator = ["+"; "-"; "*"; "/"; ">"; "<"; ">="; "<="] |> List.map pstring |> choice
        normalIdentifier <|> specialOperator

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

    and application = spaces >>. (many lispVal) .>> spaces .>> eof

    and readExpr source =
        run lispVal source

    and readContent source =
        run application source
#nowarn "40"

namespace FScheme.Core

open System
open System.IO
open FParsec

type LispParser = Parser<Lisp, unit>

module Parser =
    let digit2 = many1 (anyOf "01")

    let digit8 = many1 (anyOf "01234567")

    let digit16 = many1 (digit <|> anyOf "abcdefABCDEF")

    let suffix = opt (anyOf "+-")

    let toInt c =
        match int(c) with
        | n when n >= 48 && n < 58 -> n - 48
        | n when n >= 65 && n < 71 -> n - 65 + 10
        | n when n >= 97 && n < 103  -> n - 97 + 10
        | _ -> failwith "Not a valid number"

    let radix unit ns =
        ns |> List.map toInt |> List.fold (fun s n -> s * unit + n) 0

    let integer = suffix .>>. many1 digit
                    |>> fun (sign, nums) ->
                            let dec = radix 10 nums
                            match sign with
                            | Some '-' -> 0 - dec
                            | _ -> dec

    let decimal =
        suffix .>>. many1 digit .>>. (pchar '.' >>. many1 digit)
            |>> fun ((sign, intPart), decimalPart) ->
                let n = radix 10 intPart
                let dec = radix 10 decimalPart
                let result = (double)n + (double)dec * Math.Pow(0.1, (float)(decimalPart.Length))
                match sign with
                | Some '-' -> 0.0 - result
                | _ -> result

    let number = attempt (decimal |>> Float) <|> attempt (integer |>> Integer) |>> Number

    let pComment = pstring ";" >>. restOfLine true

    let pNestedComment = between (pstring "#|") (pstring "|#") (manyChars anyChar)

    let nil = pstring "'()" >>. preturn Lisp.Nil
        
    let symbols = anyOf "!$%&|*+-/:<=>?@^_~"

    let atom = many1Chars (letter <|> digit <|> symbols) <|> pstring "..." |>> Atom

    let spacesOrComment = spaces >>. opt (pComment <|> pNestedComment) >>. spaces

    let stringLiteral: Parser<Lisp, unit> =
        let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
        let unescape c = match c with
                         | 'n' -> '\n'
                         | 'r' -> '\r'
                         | 't' -> '\t'
                         | 'b' -> '\b'
                         | 'a' -> '\a'
                         | c   -> c
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
        let s = between (pstring "\"") (pstring "\"")
                        (manyChars (normalChar <|> escapedChar))
        s |>> Lisp.String

    let parens = between (pchar '(') (pchar ')')

    let rec lispVal = parse.Delay (fun () ->
        choice [
            hashVal;
            nil;
            number;
            atom;
            stringLiteral;
            pQuotedLispVal;
            pUnquotedLispVal;
            pQuasiquotedLispVal;
            pListVal;
        ])

    and hashVal =
            pchar '#' >>. choice [
                pchar 't' >>. preturn (Lisp.Bool true);
                pchar 'f' >>. preturn (Lisp.Bool false);
                pstring "true" >>. preturn (Lisp.Bool true);
                pstring "false" >>. preturn (Lisp.Bool false);
                pchar 'b' >>. digit2 |>> radix 2 |>> Integer |>> Number
                pchar 'o' >>. digit8 |>> radix 8 |>> Integer |>> Number
                pchar 'd' >>. many1 digit |>> radix 10 |>> Integer |>> Number
                pchar 'x' >>. digit16 |>> radix 16 |>> Integer |>> Number
                pchar '\\' >>. anyChar |>> Lisp.Char
                pchar '(' >>. (manyLispVal .>> pchar ')') |>> Array.ofList |>> Vector
            ]

    and pQuotedLispVal = pchar '\'' >>? lispVal |>> fun x -> Lisp.List [Lisp.Atom "quote"; x]

    and pUnquotedLispVal = pchar ',' >>. choice [
                                            pchar '@' >>. lispVal |>> fun x -> Lisp.List[Lisp.Atom "unquote-slicing"; x]
                                            lispVal |>> fun x -> Lisp.List[Lisp.Atom "unquote"; x]
                                         ]

    and pQuasiquotedLispVal = pchar '`' >>? lispVal |>> fun x -> Lisp.List[Lisp.Atom "quasiquote"; x]

    and manyLispVal = spacesOrComment >>. (sepEndBy lispVal spacesOrComment)

    and pListVal = parens (manyLispVal .>>. opt (pchar '.' >>. spaces >>. lispVal)) 
                |>> fun (head, tail) -> 
                    match tail with
                    | Some t -> DottedList (head, t)
                    | None -> List head

    and application : Parser<Application, unit> = spacesOrComment >>. (sepEndBy pListVal (spacesOrComment)) .>> eof

    and readExpr source =
        match run (spaces >>. lispVal .>> spaces .>> eof) source with
        | Success (ast, _, _) -> ast
        | Failure (err, _, _) -> PErrorException err |> raise

    and readContent source =
        match run application source with
        | Success (ast, _, _) -> ast
        | Failure (err, _, _) -> PErrorException err |> raise

    and readFile (filepath: string) =
        use stream = new StreamReader(filepath)
        let content = stream.ReadToEnd()
        readContent content
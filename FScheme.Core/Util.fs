namespace FScheme.Core

open System.IO;
open System.Reflection
open System.Text;
open Microsoft.Extensions.FileProviders

type private AssemblyLocator = AssemblyLocator

[<AutoOpen>]
module Util =
    let id x = x

    let unwords : (seq<string> -> string) = String.concat " "

    let escape s =
        s 
        |> Seq.toList 
        |> List.map (fun c -> 
                        match c with
                        | '\n' -> "\\n"
                        | '\r' -> "\\r"
                        | '\t' -> "\\t"
                        | '\b' -> "\\b"
                        | '\a' -> "\\a"
                        | '\\' -> "\\\\"
                        | a -> sprintf "%c" a)
        |> Seq.ofList
        |> String.concat ""

    let unescape c = match c with
                        | 'n' -> '\n'
                        | 'r' -> '\r'
                        | 't' -> '\t'
                        | 'b' -> '\b'
                        | 'a' -> '\a'
                        | c   -> c

    let getFileContent (filepath: string) =
        if File.Exists(filepath) then
            File.ReadAllText(filepath)
        else
            failwith "File does not exist."

    let loadFileContent filename =
        let assembly = (typeof<AssemblyLocator>).GetTypeInfo().Assembly;
        let embeddedProvider = EmbeddedFileProvider(assembly, "FScheme.Core");
        let file = embeddedProvider.GetFileInfo(Path.Combine("lib", filename));
        if file.Exists then
            let stream = file.CreateReadStream();
            use reader = new StreamReader(stream, Encoding.UTF8)
            reader.ReadToEnd()
        else
            getFileContent filename

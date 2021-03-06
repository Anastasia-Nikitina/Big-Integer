module Program
open System.Numerics
open BigInt
open MyList
open ToDot
open Argu
open FSharp.Text.Lexing

let SystemToMyBigInt (x: BigInteger) =
        let y = x |> string
        let newX = if y.[0] = '-' then y.[1..] else y
        let list = newX |> List.ofSeq |> List.map (string >> int) |> systemToMyList
        NumberWithSign(list, (x >= BigInteger 0))
        
type CLIArguments =
        | InputFile of file:string
        | InputString of code:string
        | Compute
        | ToDot of output:string
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | InputFile _ -> "File with code"
                | InputString _ -> "String of code" 
                | Compute -> "Return the result of interptetation of given code"
                | ToDot _ -> "Generates dot code of syntax tree to the given file"

[<EntryPoint>]
let main (argv: string array) =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "BigInteger interpreter")
    let results = parser.Parse(argv)
    let p = parser.ParseCommandLine argv
    if argv.Length = 0 || results.IsUsageRequested then parser.PrintUsage() |> printfn "%s"
    else 
        let input =
            if p.Contains(InputFile) then System.IO.File.ReadAllText (results.GetResult InputFile)
            elif p.Contains(InputString) then results.GetResult InputString
            else failwith "No input code given"
        let ast = Interpreter.parse input
        if p.Contains(Compute)
        then
            let pD = Interpreter.runPrint ast
            printfn "%s" pD
        if p.Contains(ToDot) then drawTree ast (results.GetResult ToDot)
    0     
module Interpreter

open System
open System.Collections.Generic
open BigInt
open FSharp.Text.Lexing

let outputBuffer = "print"
let private newDataToConsole = Event<string>()

let printed = newDataToConsole.Publish

type Dicts = { VariablesDictionary: Dictionary<string, string>; InterpretedDictionary: Dictionary<AST.VName, AST.Expression> }

let rec processExpr (vDict:Dictionary<AST.VName,AST.Expression>) expr =
    match expr with
    | AST.Num n -> n
    | AST.NVar nv ->
        let data =
            try
                vDict.[nv]
            with
            | _ -> failwithf "Variable %A is not declared." nv
        processExpr vDict data
    | AST.Sum (x, y) -> addition (processExpr vDict x) (processExpr vDict y)
    | AST.Sub (x, y) -> subtraction (processExpr vDict x) (processExpr vDict y)
    | AST.Mul (x, y) -> multiplication (processExpr vDict x) (processExpr vDict y)
    | AST.Div (x, y) -> division (processExpr vDict x) (processExpr vDict y)
    | AST.Rem (x, y) -> remainder (processExpr vDict x) (processExpr vDict y)
    | AST.Pow (x, y) -> power (processExpr vDict x) (processExpr vDict y)
    | AST.Abs x -> absNWS (processExpr vDict x)
    | AST.Bin x -> toBin (processExpr vDict x)

let processStmt (vDict:Dictionary<AST.VName,AST.Expression>) printString stmt =
    match stmt with
    | AST.Print v ->
        let data =
            try
                vDict.[v]
            with
            | _ -> failwithf "Variable %A is not declared." v
        match data with
        | AST.Num n ->
                let num = NWSToString n
                let str = (if num.[0] = '+' then num.[1..] else num) + "\n"
                newDataToConsole.Trigger str
                vDict, printString + str
            | _ ->
                failwithf "Num expected, got: %A" data
    | AST.VDecl(v,e) ->
            if vDict.ContainsKey v
            then vDict.[v] <- AST.Num (processExpr vDict e)
            else vDict.Add(v, AST.Num (processExpr vDict e))
            vDict, printString

let runVariables (startDicts: Dicts) ast =
    let startDict = startDicts.VariablesDictionary
    let variableDict = startDicts.InterpretedDictionary
    let vD, _ = List.fold (fun (d1, ps) stmt -> processStmt d1 ps stmt) (variableDict, "") ast
    for i in vD.Keys do
        match vD.[i] with
        | AST.Num n ->
            match i with
            | AST.Var k -> startDict.[k] <- NWSToString n
        | _ -> failwith "impossible case"
    let dicts = { VariablesDictionary = startDict; InterpretedDictionary = vD }
    dicts
    
let runPrint ast =
    let vDict = Dictionary<_,_>()
    let varDict = Dictionary<_,_>()
    let vD, printString = List.fold (fun (d1, ps) stmt -> processStmt d1 ps stmt) (vDict, "") ast
    for i in vD.Keys do
        match vD.[i] with
        | AST.Num n -> varDict.[string i] <- NWSToString n
        | _ -> failwith "impossible case"
    printString

let calculate (ast:AST.Stmt list) =
    match ast.[0] with
    | AST.VDecl (_, e) -> processExpr (Dictionary<_,_>()) e
    | _ -> failwithf "Unexpected statement %A" ast.[0]

let parse text =
    let lexbuf = LexBuffer<char>.FromString text
    try
        let parsed =
            lexbuf
            |> Parser.start Lexer.tokenStream
        parsed
    with errorMsg -> 
        let pos = lexbuf.EndPos
        let line = pos.Line
        let column = pos.Column
        let message = errorMsg.Message
        let lastToken = String(lexbuf.Lexeme)
        printf "Parsing failed at: "
        printfn "line %A, column %A;" (line + 1) (column + 1)
        printfn "Last token %A" lastToken
        printfn "Message %A" message
        let errorString = (sprintf "Execution failed at: line %A, column %A. Last token %A" (line + 1) (column + 1) lastToken) 
        failwithf "%A" errorString   
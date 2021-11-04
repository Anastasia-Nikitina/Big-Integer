module ToDot
open BigInt
open MyList
open AST

    let rec drawTree ast output =
        let createStr label counter = [(sprintf "\"x%A\"" counter)] @ [sprintf " [shape = ellipse, label = %A]; \n" label] 
        let addLabel res label prevLabel c = res @ createStr label c @ [sprintf "\"x%A\"" prevLabel] @ [ "->" ] @ [sprintf "\"x%A\"" c] @ [";\n"]
        let mutable c = 0
        let rec expToDot expr res prevLabel =
            c <- c + 1
            match expr with
            | Num n -> addLabel res (NWSToString n) prevLabel c
            | NVar (Var v) ->
                let newRes = addLabel res "NVar" prevLabel c
                c <- c + 1
                newRes @ createStr (sprintf "%s" v) c @ [sprintf "\"x%A\"" (c - 1)] @ [" -> "] @ [sprintf "\"x%A\"" c] @ [";\n"]
            | Sum (x, y) ->
                let newRes = addLabel res "Sum" prevLabel c  
                let currC = c
                expToDot y (newRes @ expToDot x [""] currC) currC
            | Sub (x, y) ->
                let newRes = addLabel res "Sub" prevLabel c
                let currC = c
                expToDot y (newRes @ expToDot x [""] currC) currC
            | Mul (x, y) ->
                let newRes = addLabel res "Mul" prevLabel c
                let currC = c
                expToDot y (newRes @ expToDot x [""] currC) currC
            | Div (x, y) ->
                let newRes = addLabel res "Div" prevLabel c
                let currC = c
                expToDot y (newRes @ expToDot x [""] currC) currC
            | Rem (x, y) ->
                let newRes = addLabel res "Rem" prevLabel c
                let currC = c
                expToDot y (newRes @ expToDot x [""] currC) currC
            | Pow (x, y) ->
                let newRes = addLabel res "Pow" prevLabel c
                let currC = c
                expToDot y (newRes @ expToDot x [""] currC) currC
            | Abs x ->
                let newRes = addLabel res "Abs" prevLabel c
                expToDot x newRes c
            | Bin x ->
                let newRes = addLabel res "Bin" prevLabel c
                expToDot x newRes c

        let rec go ast res =
            c <- c + 1
            match ast with
            | [] -> res
            | hd :: tl ->
                match hd with
                | VDecl (Var v, e) ->
                    let newRes = addLabel res "VDecl" 0 c
                    c <- c + 1
                    let newRes2 = addLabel newRes (sprintf "%s" v) (c - 1) c
                    c <- c + 1
                    let newRes3 = newRes2 @ (expToDot e [""] (c - 2))
                    go tl newRes3
                | Print (Var v) ->
                    let newRes = addLabel res "Print" 0 c
                    c <- c + 1
                    let newRes2 = addLabel newRes (sprintf "%s" v) (c - 1) c
                    go tl newRes2

        let firstLabel = createStr "list<Stmt>" 0

        System.IO.File.WriteAllText (output, listToString (["digraph G {\n"] @ (go ast firstLabel) @ ["}"]))
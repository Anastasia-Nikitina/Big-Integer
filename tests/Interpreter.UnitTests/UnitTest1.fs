module InterpreterTests
open Interpreter
open Program
open Expecto
open BigInt
let example =
    """
    let x = 10
    let y = 7
    let z = x/10 + y * 3
    print x
    print y
    print z
    """
let _, _, pD = run (parse example)
printfn "%s" pD.["print"]
printfn "Expected \n10\n7\n22"

[<Tests>]
let tests =
    testList "Tests for calculator" [
        testCase "Test for brackets" <| fun _ ->
            let input = "let x = ((10 + 20 + 30) / (3 * 2 + 6)) * (7 - 3) "
            Expect.equal "20" (NWSToString (calculate (parse input))) ""
        testCase "Tests for power" <| fun _ ->
            let input = "let x = 10^2 + 2^3"
            Expect.equal "108" (NWSToString (calculate (parse input))) ""
        testCase "Test for abs" <| fun _ ->
            let input =  "let x = | -20 * | -3||"    
            Expect.equal "60" (NWSToString (calculate (parse input))) ""
        testCase "Test for unary minus" <| fun _ ->
            let input = "let x = 14 * -2 + -15 * -2"
            Expect.equal "2" (NWSToString (calculate (parse input))) ""
        testCase "Common test 1" <| fun _ ->
            let input = "let x = (14 + (-2)^3 + 4/2) * -2"
            Expect.equal "-16" (NWSToString (calculate (parse input))) ""  
        testCase "Test for zero division" <| fun _ ->                
            let input = "let x = 75 / 0"
            Expect.throws (fun _ -> (calculate (parse input)) |> ignore) ""
        testCase "Wrong number test" <| fun _ ->                
            let input = "let x = 00 + 2"
            Expect.throws (fun _ -> (calculate (parse input)) |> ignore) ""
]
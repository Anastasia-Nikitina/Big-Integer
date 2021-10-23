module InterpreterTests

open Expecto

[<Tests>]
let Tests1 =
    testList
        "Tests for helping functions"
        [ testCase "Test for comparision"
          <| fun _ -> Expect.equal (2 * 2) (2 + 2) "tests not implemented yet ¯\_(ツ)_/¯" ]
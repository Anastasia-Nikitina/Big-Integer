module BigIntTest

open Expecto
open System.Numerics
open MyList
open BigInt

let isEqualNWS  (a: NumberWithSign) (b: NumberWithSign) = 
    if a.sign <> b.sign 
    then false 
    else isEqual a.number b.number

let rand = System.Random()

let genRandomList l =
    let rnd = System.Random()
    List.init (rnd.Next(1, l)) (fun _ -> rnd.Next(9))

let genRandomBigInteger l =
    fst (genRandomList l |> List.fold (fun (i, p) x -> (i + (BigInteger x)*p, p*(BigInteger 10))) (BigInteger 0, BigInteger 1))

let SystemToMyBigInt (x: BigInteger) =
    let y = string x
    let newX = if y.[0] = '-' then y.[1..] else y
    let list = newX |> List.ofSeq |> List.map (string >> int) |> SystemToMyList
    NumberWithSign(list, (if x >= BigInteger 0 then true else false))

let intToBinary x =
    let mutable r = if x = 0 then "0" else ""
    let mutable c = x
    while c > 0 do
        r <- string (c % 2) + r
        c <- c / 2
    int64 r               

[<Tests>]
let Tests1 =
    testList "Tests for helping functions"
        [testCase "Test for comparision" <| fun _  ->
            let a1 = Cons(1, Cons(5, Single 3))
            let a2 = Cons(1, Cons(5, Single 3))
            let a3 = Cons(1, Cons(7, Cons(8, Single 4)))
            let a4 = Cons(1, Cons(5, Single 0))
            Expect.isTrue (fstGreaterThanSec a1 a4) ""
            Expect.isFalse (fstGreaterThanSec a1 a2) ""
            Expect.isTrue (fstGreaterThanSec a3 a1)  ""
         testCase "Test for slice" <| fun _  ->
            let a = Cons(1, Cons(2, Cons(3, Cons(4, Single 5))))
            let a1 = Cons(1, Cons(2, Single 3))
            let a2 = Single 1
            Expect.equal (slice a 0 2) a1 ""
            Expect.equal (slice a 0 0) a2 "" 
         testCase "Test for isEqual" <| fun _ ->
            let a1 = Cons(1, Cons(5, Single 3))
            let a2 = Cons(1, Cons(5, Single 3))
            let a3 = Cons(1, Cons(7, Cons(8, Single 4)))
            Expect.equal a1 a2 ""
            Expect.notEqual a1 a3 ""
         testProperty "Test for addZeros" <| fun _ ->
            let a1 = SystemToMyList(genRandomList 10)
            let a2 = SystemToMyList (genRandomList 3)
            let (res1, res2) = addZeros a1 a2
            Expect.equal (Length res1) (Length res2) ""
         testCase "Test for removeZeros" <| fun _ ->
             let a1 = Cons(1, Cons(5, Single 3))
             let a2 = Cons(0, Cons(0, Cons(0, Cons(1, Cons(5, Single 3)))))
             let a3 = Cons(1, Cons(5, Cons(3, Cons(0, Cons(0, Single 0)))))
             Expect.equal (removeZeros a2) a1 ""
        ]

[<Tests>]
let Tests2 =
    testList "Tests for BigInt"
        [testProperty "Test for sum" <| fun _ ->
            let a1 = genRandomBigInteger 30
            let a2 = genRandomBigInteger 30
            let exp = SystemToMyBigInt (a1 + a2)
            let res = addition (SystemToMyBigInt a1)  (SystemToMyBigInt a2)
            Expect.isTrue (isEqualNWS exp res)
         testProperty "Test for mult" <| fun _ ->
            let a1 = genRandomBigInteger 10
            let a2 = genRandomBigInteger 10
            let exp = SystemToMyBigInt (a1 * a2)
            let res = multiplication (SystemToMyBigInt a1)  (SystemToMyBigInt a2)
            Expect.isTrue (isEqualNWS exp res)
         testProperty "Test for sub" <| fun _ ->
            let a1 = genRandomBigInteger 30
            let a2 = genRandomBigInteger 30
            let exp = SystemToMyBigInt (a1 - a2)
            let res = subtraction (SystemToMyBigInt a1)  (SystemToMyBigInt a2)
            Expect.isTrue (isEqualNWS exp res)
         testProperty "Test for division" <| fun _ ->
            let a1 = genRandomBigInteger 10
            let a2 = genRandomBigInteger 10
            if a2 = BigInteger 0
            then Expect.equal 0 0
            else
                let exp = SystemToMyBigInt (a1 / a2)
                let res = division (SystemToMyBigInt a1)  (SystemToMyBigInt a2)
                Expect.isTrue (isEqualNWS exp res)
         testProperty "Test for remainder" <| fun _ ->
             let a1 = genRandomBigInteger 10
             let a2 = genRandomBigInteger 10             
             if a2 = BigInteger 0
             then Expect.equal 0 0
             else
                 let exp = SystemToMyBigInt (a1 % a2)
                 let res = remainder (SystemToMyBigInt a1)  (SystemToMyBigInt a2)
                 Expect.isTrue (isEqualNWS exp res)
         testProperty "Test for unMinus" <| fun _ ->
            let a1 = genRandomBigInteger 20
            let exp = SystemToMyBigInt (a1 * BigInteger (-1))
            let res = unMinus (SystemToMyBigInt a1)
            Expect.isTrue (isEqualNWS res exp)
         testProperty "Test for power" <| fun _ ->
             let a1 = genRandomBigInteger 5
             let a2 = rand.Next(5)
             let exp = SystemToMyBigInt (a1 ** int(a2))
             let res = power (SystemToMyBigInt a1) (SystemToMyBigInt (BigInteger(a2)))
             Expect.isTrue (isEqualNWS res exp) 
         testProperty "Test for abs" <| fun _ ->
              let a = (genRandomBigInteger 20)*BigInteger(-1)
              let exp = SystemToMyBigInt (a * BigInteger(-1))
              let res = abs (SystemToMyBigInt a)
              Expect.isTrue (isEqualNWS res exp) "" 
         testProperty "toBinary test" <| fun _ ->
            let x = System.Random().Next(1000) 
            let s = BigInteger (intToBinary x) 
            let x1 = SystemToMyBigInt (BigInteger x)
            let sb = toBin x1
            Expect.isTrue (isEqualNWS (SystemToMyBigInt s) sb)     
        ]
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

let systemToMyBigInt (x: BigInteger) =
    let y = string x
    let newX = if y.[0] = '-' then y.[1..] else y
    let list = newX |> List.ofSeq |> List.map (string >> int) |> systemToMyList
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
        [testCase "Test for greater" <| fun _  ->
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
         testProperty "Test for addZeros" <| fun m ->
            let a1 = systemToMyList (genRandomList (abs m + 1))
            let a2 = systemToMyList (genRandomList (abs m + 1))
            let (res1, res2) = addZeros a1 a2
            Expect.equal (length res1) (length res2) ""
         testCase "Test for removeZeros" <| fun _ ->
             let a1 = Cons(1, Cons(5, Single 3))
             let a2 = Cons(0, Cons(0, Cons(0, Cons(1, Cons(5, Single 3)))))
             Expect.equal (removeZeros a2) a1 ""
        ]

[<Tests>]
let Tests2 =
    testList "Tests for BigInt"
        [testProperty "Test for sum" <| fun m ->
            let a1 = genRandomBigInteger (abs m + 1)
            let a2 = genRandomBigInteger (abs m + 1)
            let exp = systemToMyBigInt (a1 + a2)
            let res = addition (systemToMyBigInt a1)  (systemToMyBigInt a2)
            Expect.isTrue (isEqualNWS exp res)
         testProperty "Test for mult" <| fun m ->
            let a1 = genRandomBigInteger (abs m % 10 + 1)
            let a2 = genRandomBigInteger (abs m % 10 + 1)
            let exp = systemToMyBigInt (a1 * a2)
            let res = multiplication (systemToMyBigInt a1)  (systemToMyBigInt a2)
            Expect.isTrue (isEqualNWS exp res)
         testProperty "Test for sub" <| fun m ->
            let a1 = genRandomBigInteger (abs m + 1)
            let a2 = genRandomBigInteger (abs m + 1)
            let exp = systemToMyBigInt (a1 - a2)
            let res = subtraction (systemToMyBigInt a1)  (systemToMyBigInt a2)
            Expect.isTrue (isEqualNWS exp res)
         testProperty "Test for division" <| fun m ->
            let a1 = genRandomBigInteger (abs m % 10 + 1)
            let a2 = genRandomBigInteger (abs m % 10 + 1)
            if a2 = BigInteger 0
            then Expect.equal 0 0
            else
                let exp = systemToMyBigInt (a1 / a2)
                let res = division (systemToMyBigInt a1)  (systemToMyBigInt a2)
                Expect.isTrue (isEqualNWS exp res)
         testProperty "Test for remainder" <| fun m ->
             let a1 = genRandomBigInteger (abs m % 10 + 1)
             let a2 = genRandomBigInteger (abs m % 10 + 1)         
             if a2 = BigInteger 0
             then Expect.equal 0 0
             else
                 let exp = systemToMyBigInt (a1 % a2)
                 let res = remainder (systemToMyBigInt a1)  (systemToMyBigInt a2)
                 Expect.isTrue (isEqualNWS exp res)
         testProperty "Test for unMinus" <| fun m ->
            let a1 = genRandomBigInteger (abs m + 1)
            let exp = systemToMyBigInt (a1 * BigInteger (-1))
            let res = unMinus (systemToMyBigInt a1)
            Expect.isTrue (isEqualNWS res exp)
         testProperty "Test for power" <| fun m ->
             let a1 = genRandomBigInteger (abs (m % 5) + 1)
             let a2 = rand.Next(5)
             let exp = systemToMyBigInt (a1 ** int(a2))
             let res = power (systemToMyBigInt a1) (systemToMyBigInt (BigInteger(a2)))
             Expect.isTrue (isEqualNWS res exp) 
         testProperty "Test for abs" <| fun m ->
              let a = (genRandomBigInteger (abs m + 1)) * BigInteger(-1)
              let exp = systemToMyBigInt (a * BigInteger(-1))
              let res = absNWS (systemToMyBigInt a)
              Expect.isTrue (isEqualNWS res exp) "" 
         testProperty "toBinary test" <| fun m ->
            let x = System.Random().Next(abs m) 
            let s = BigInteger (intToBinary x) 
            let x1 = systemToMyBigInt (BigInteger x)
            let sb = toBin x1
            Expect.isTrue (isEqualNWS (systemToMyBigInt s) sb)     
        ]
        
[<Tests>]
let Tests3 =
    testList "Test for BigInt. Int64"
         [testProperty "Test for sum" <| fun (a: int64, b: int64) ->
             let res = addition (stringToNWS (string a)) (stringToNWS (string b)) 
             let exp = stringToNWS (string (a + b))
             Expect.isTrue (isEqualNWS res exp)
          testProperty "Test for mult" <| fun (a:  BigInteger, b: BigInteger) ->
             let res = multiplication (stringToNWS (string a)) (stringToNWS (string b))
             let exp = stringToNWS (string (a * b))
             Expect.isTrue (isEqualNWS res exp)
          testProperty "Test for sub" <| fun (a: int64, b: int64) ->
             let res = subtraction (stringToNWS (string a)) (stringToNWS (string b))
             let exp = stringToNWS (string (a - b))
             Expect.isTrue (isEqualNWS res exp)
          testProperty "Test for div" <| fun (a: int64, b: int64) ->            
             let test (a: int64) (b: int64) =
                 let res = division (stringToNWS (string a)) (stringToNWS (string b))
                 let exp = stringToNWS (string (a / b))                
                 Expect.isTrue (isEqualNWS res exp)            
             if b = 0L then test a (b + 1L) else test a b
          testProperty "Test for rem" <| fun (a: int64, b: int64) ->            
             let test (a: int64) (b: int64) =
                 let res = remainder (stringToNWS (string a)) (stringToNWS (string b))
                 let exp = stringToNWS (string (a % b))                
                 Expect.isTrue (isEqualNWS res exp)            
             if b = 0L then test a (b + 1L) else test a b
          testProperty "Test for unMin"  <| fun (a: int64) ->
              let res = unMinus (stringToNWS (string a))
              let exp = stringToNWS (string (- a))
              Expect.isTrue (isEqualNWS res exp)
          testProperty "Test for abs" <| fun (a: int64) ->
              let res = absNWS (stringToNWS (string a))
              let exp = stringToNWS (string (System.Math.Abs a))
              Expect.isTrue (isEqualNWS res exp)                          
         ]   
[<Tests>]
let Tests4=
    testList "Tests for helping functions. Int64"
        [testProperty "Test for greater" <| fun (a: int64, b: int64) ->
            let a1 = abs a
            let b1 = abs b
            if a1 > b1
            then Expect.isTrue (fstGreaterThanSec  (stringToMyList (string a1)) (stringToMyList (string b1)))
            else Expect.isFalse (fstGreaterThanSec (stringToMyList (string a1)) (stringToMyList (string b1)))
         testProperty "Test for isEqual" <| fun (a: int64, b: int64) ->
            if a = b
            then Expect.isTrue (isEqualNWS (stringToNWS (string a)) (stringToNWS (string b)))
            else Expect.isFalse (isEqualNWS (stringToNWS (string a)) (stringToNWS(string b)))        
    ]      
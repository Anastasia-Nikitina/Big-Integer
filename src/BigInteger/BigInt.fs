module BigInt
open MyList

type NumberWithSign =
    val sign: bool
    val number: MyList<int>
    new(a, s) = {number = a; sign = s}
  
let stringToNWS (string: string) =
    if string.[0] = '-'
    then NumberWithSign (stringToMyList (string.[1..string.Length-1]), false)
    else NumberWithSign (stringToMyList string, true)

let NWSToString (a: NumberWithSign) =
    let s = listToString (myListToSystem a.number)
    if a.sign then s  else "-" + s
      
let fstGreaterThanSec (x: MyList<int>) (y: MyList<int>) = // выводит true если первое число больше второго
    let rec go (x: MyList<int>) (y: MyList<int>) =
        match (x, y) with
        | (Single a, Single b) -> a > b          
        | (Single _, Cons _) -> false         
        | (Cons _, Single _) -> true
        | (Cons(hd1, tail1), Cons(hd2, tail2)) ->
            if length x <> length y then length x > length y                            
            elif hd1 <> hd2 then hd1 > hd2                                                        
            else go tail1 tail2
    let res = go x y
    res

let addZeros (x: MyList<int>) (y: MyList<int>) =
    let mutable a = x
    let mutable b = y
    let dif = abs (length x - length y)
    if dif <> 0 then
        let lst = List.init dif (fun _ -> 0)    
        let zero = systemToMyList lst // дополняем нулями, чтобы числа были одинаковой длины
        if length x <= length y
        then a <- concat zero a  
        else b <- concat zero b 
    (a, b)
    
let removeZeros (a: MyList<int>) =
    let rec go (a: MyList<int>) =
        match a with
        | Single 0 -> Single 0
        | Single x -> Single x
        | Cons (hd, tail) -> 
            if hd = 0 then go tail
            else Cons (hd, tail)         
    go a

let goSub (x: MyList<int>) (y: MyList<int>) =
    let (a, b) = addZeros x y
    let rec go (a: MyList<_>) (b: MyList<_>) transfer =
        match (a, b) with
        | (Cons(head_a, tail_a), Cons(head_b, tail_b)) ->
            if head_a - transfer < head_b
            then Cons(head_a + 10 - head_b - transfer, go tail_a tail_b 1)
            else Cons(head_a - head_b - transfer, go tail_a tail_b 0)
        | (Single a, Single b) ->
            Single (a - transfer - b)
        | (_, _) -> failwithf "Error"
    if fstGreaterThanSec a b
    then removeZeros (reverse (go (reverse a) (reverse b) 0))
    else removeZeros (reverse (go (reverse b) (reverse a) 0))

let transfer (x: MyList<_>) =
    let rec go (x: MyList<_>) acc =
        match x with
        | Single a -> 
            if a + acc >= 10
            then
                let tl = 
                    if (a + acc)/10 >= 10
                    then go (Single((a+acc)/10)) 0
                    else Single ((a + acc) / 10)
                Cons((a + acc)%10, tl)
            else Single (a + acc)
        | Cons(hd, tl) ->
            if hd + acc >= 10
            then Cons((hd + acc) % 10, go tl ((hd + acc) / 10))
            else Cons(hd + acc, go tl 0)
    go x 0 
        
let goSum (x: MyList<_>) (y: MyList<_>) = 
    let (a, b) = addZeros x y
    let rec go (a: MyList<_>) (b: MyList<_>) =                           
        match (a, b) with
        | (Cons(hd_a, tl_a), Cons(hd_b, tl_b)) ->             
            Cons(hd_a + hd_b, go tl_a tl_b)
        | (Single a, Single b) ->           
            Single (a + b)
        | (_, _) -> failwith "Error"    
    removeZeros (reverse (transfer (go (reverse a) (reverse b))))
   
let multToNum (a: MyList<_>) n =
    let rec go (a: MyList<_>) n =
        match a with
        | Single a -> 
            Single (a * n) 
        | Cons(hd, tl) ->
            Cons(hd * n, go tl n)
    reverse(transfer (go (reverse a) n))
           
let goMult (a: MyList<_>) (b: MyList<_>) =
    let rec go (a: MyList<_>) (b: MyList<_>) (sum: MyList<_>) acc = 
        match (a, b) with
        | (a, Single b) ->
            let deg = stringToMyList (string (float 10**float acc))
            let res = 
                if deg = Single 1 then multToNum a b
                else concat (multToNum a b) (slice deg 1 ((length deg) - 1))                
            goSum res sum           
        | (a, Cons(hd_b, tl_b)) ->                      
            let deg = stringToMyList (string (float 10**float acc))
            let res = 
                if deg = Single 1 then multToNum a hd_b
                else concat (multToNum a hd_b) (slice deg 1 ((length deg) - 1))
            go a tl_b (goSum sum res) (acc + 1)   
    if  fstGreaterThanSec a b 
    then reverse (transfer (reverse (go a (reverse b) (Single 0) 0)))
    else reverse (transfer (reverse (go b (reverse a) (Single 0) 0)))

let subtraction (a: NumberWithSign) (b: NumberWithSign) =
    match (a.sign, b.sign) with
    | (true, true) | (false, false) ->
        let eq = isEqual a.number b.number
        if (fstGreaterThanSec a.number b.number) || eq
        then NumberWithSign ((goSub a.number b.number), eq || a.sign)
        else NumberWithSign ((goSub a.number b.number), not a.sign)
    | (false, true) | (true, false) -> NumberWithSign ((goSum a.number b.number), a.sign)

let addition (a: NumberWithSign) (b : NumberWithSign) =
    match (a.sign, b.sign) with
    | (true, true) | (false, false) ->
        NumberWithSign((goSum a.number b.number), a.sign)
    | (false, true) | (true, false)  ->
        let eq = isEqual a.number b.number
        if fstGreaterThanSec a.number b.number
        then NumberWithSign ((goSub a.number b.number), eq || a.sign)
        else NumberWithSign ((goSub a.number b.number), eq || b.sign)
 
let multiplication (a: NumberWithSign) (b: NumberWithSign) =
    match (a.sign, b.sign) with
    | (true, true) | (false, false) -> NumberWithSign (goMult a.number b.number, true)
    | (true, false) | (false, true) -> NumberWithSign (goMult a.number b.number, (a.number = Single 0) || (b.number = Single 0))

let selection (x: MyList<int>) (y: MyList<int>) =
    let mutable k = 1
    while ( fstGreaterThanSec x (multToNum y k)) do
        k <- k + 1
    if isEqual x (multToNum y k) then k else k - 1 
    
let goDiv (x: MyList<_>) (y: MyList<_>) =
    let l = length x
    let rec go (a: MyList<_>) (b: MyList<_>) len div =  
        if b = Single 0 then failwith "Division by 0 is impossible"
        if len <= l
            then
                if fstGreaterThanSec b a
                then 
                    if len + 1 <= l                      
                    then                           
                        go (concat a (slice x len len)) b (len + 1) (concat div (Single 0)) 
                    else concat div (Single 0)
                else
                    let k = selection (removeZeros a) b
                    if len + 1 <= l
                    then
                        let a1 = removeZeros (concat (goSub a (multToNum b k)) (slice x len len))
                        go a1 b (len + 1) (concat div (Single k))
                    else concat div (Single k) 
        else div 
    let a = slice x 0 (length y - 1)
    let x = reverse (transfer (reverse (removeZeros (go a y (length y) (Single 0)))))
    x
    
let goRem (x: MyList<_>) (y: MyList<_>) =
    goSub x (goMult (goDiv x y) y)

let division (a: NumberWithSign) (b: NumberWithSign) =
    match (a.sign, b.sign) with
    | (true, true) | (false, false) -> NumberWithSign(goDiv a.number b.number, true)
    | (false, true) | (true, false) ->
        let div = goDiv a.number b.number
        NumberWithSign (div, (div = Single 0))
let remainder (a: NumberWithSign) (b: NumberWithSign) =   
    subtraction a (multiplication (division a b) b)
    
let isOdd (a: NumberWithSign) = ((head (reverse a.number)) % 2) = 0 
let power (n: NumberWithSign) (pow: NumberWithSign) =
    if not pow.sign then failwith "Power can't be negative"
    let rec go (nList: MyList<_>) (pow: MyList<_>) =
        match pow with
        | Single 0 -> Single 1
        | Single 1 -> nList
        | _ ->
            let div, rem = (goDiv pow (Single 2), goRem pow (Single 2))
            let p = go nList div
            let p1 = goMult p p
            if rem = Single 0 then p1 else (goMult p1 nList)
    match n.sign with
    | true -> NumberWithSign ((go n.number pow.number), true)
    | false -> if isOdd pow 
               then NumberWithSign ((go n.number pow.number), true)
               else NumberWithSign ((go n.number pow.number), false)
        
let absNWS (a: NumberWithSign)  =
    NumberWithSign(a.number, true)
    
let unMinus (a: NumberWithSign) =
    if a.number = Single 0
    then a
    else
        let rSign = not a.sign
        NumberWithSign(a.number, rSign)

let toBin (x: NumberWithSign) =
    let rec go l r =
        match l with
        | Single 0 -> r
        | _ ->
            let divd = division (NumberWithSign(l, true)) (NumberWithSign(Single 2, true))
            let rem = remainder (NumberWithSign(l, true)) (NumberWithSign(Single 2, true))
            go divd.number (Cons(head rem.number, r))
    let divd = division (NumberWithSign(x.number, true)) (NumberWithSign(Single 2, true))
    let rem = remainder (NumberWithSign(x.number, true)) (NumberWithSign(Single 2, true))
    NumberWithSign(go divd.number (Single(head rem.number)), x.sign)
module MyList
open System
type MyList<'t> =
    | Single of 't
    | Cons of 't * MyList<'t>

let rec fold f acc list =
    match list with
    | Single x -> f acc x
    | Cons (head, tail) -> fold f (f acc head) tail

let length list =
    fold (fun q x -> q + 1) 0 list

let rec map f list =
    match list with
    | Single a -> Single (f a)
    | Cons (head, tail) -> Cons(f head, map f tail)

let rec iter f list =
    match list with
    | Single a -> f a
    | Cons (head, tail) ->
        f head
        iter f tail

let rec concat list1 list2 =         
    match list1 with
    | Single x -> Cons(x, list2)
    | Cons (head, tail) -> Cons (head, concat tail list2)
    
let rec systemToMyList list =
    match list with
    | [] -> failwith "Empty list"
    | [a] -> Single a
    | head :: tail -> Cons (head, systemToMyList tail)
    
let private tryParseWith (tryParseFunc: string -> bool * _) =  
    tryParseFunc >> function                                   
    | true, v -> Some v                                        
    | false, _ -> None                                             
let stringToMyList (str: string) =                                                
    let listOfInt =                                                               
        [for ch in str do                                                         
            match tryParseWith Int32.TryParse (string ch) with                    
            | None -> failwith $"Could not parse symbol: %A{ch} in %s{str}"       
            | Some intValue -> intValue                                           
        ]                                                                         
                                                                                  
    systemToMyList listOfInt                                                      
let rec myListToSystem list =
    match list with
    | Single x -> [x]
    | Cons (head, tail) -> head :: myListToSystem tail

let head list =
    match list with
    | Single x -> x
    | Cons(head, tail) -> head

let tail list =
    match list with
    | Single x -> Single x
    | Cons(_, tail) -> tail

let sort list =
    let rec go1 list1 =
        match list1 with
        | Single x -> Single x
        | Cons (head1, Cons(head2, tail)) ->
            if head1 >= head2
            then Cons (head2, go1 (Cons(head1, tail)))
            else Cons (head1, go1 (Cons(head2, tail)))
        | Cons (head, Single tail) ->
            if head >= tail
            then Cons (tail, Single head)
            else list1
    let rec go2 (list2: MyList<'t>) k =
        if k <> length list2
        then go2 (go1 list2) (k + 1)
        else list2
    go2 list 0

let arrayToString (array: array<int>) =
    let stringArr: array<string> = Array.zeroCreate array.Length
    for i = 0 to array.Length - 1 do
        stringArr.[i] <- string array.[i]
    stringArr
    
let listToString list =
    let rec go list =
       match list with
       | head :: tail -> head.ToString() + go tail
       | [] -> ""
    go list
    
let reverse (x: MyList<int>) =
   systemToMyList (List.rev (myListToSystem x))

let slice (a: MyList<int>) x1 x2 =
    systemToMyList ((myListToSystem a).[x1..x2])
    
let isEqual (a: MyList<int>) (b: MyList<int>) =
    let rec go (a: MyList<int>) (b: MyList<int>) =
        if length a <> length b then false
        else
           match (a, b) with
           | (Single a, Single b) -> a = b 
           | (Cons(hd_a, tl_a), Cons(hd_b, tl_b)) -> (hd_a = hd_b) && (go tl_a tl_b)
           | (_, _) -> failwith "Error"
    go a b            
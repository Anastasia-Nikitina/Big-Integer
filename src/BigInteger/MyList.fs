module MyList

type MyList<'t> =
    | Single of 't
    | Cons of 't * MyList<'t>

let rec Fold f acc list =
    match list with
    | Single x -> f acc x
    | Cons (head, tail) -> Fold f (f acc head) tail

let Length list =
    Fold (fun q x -> q + 1) 0 list

let rec Map f list =
    match list with
    | Single a -> Single (f a)
    | Cons (head, tail) -> Cons(f head, Map f tail)

let rec Iter f list =
    match list with
    | Single a -> f a
    | Cons (head, tail) ->
        f head
        Iter f tail

let rec Concat list1 list2 =         
    match list1 with
    | Single x -> Cons(x, list2)
    | Cons (head, tail) -> Cons (head, Concat tail list2)
    
let rec SystemToMyList list =
    match list with
    | [] -> failwith "Empty list"
    | [a] -> Single a
    | head :: tail -> Cons (head, SystemToMyList tail)

let rec MyListToSystem list =
    match list with
    | Single x -> [x]
    | Cons (head, tail) -> head :: MyListToSystem tail

let Head list =
    match list with
    | Single x -> x
    | Cons(head, tail) -> head

let Tail list =
    match list with
    | Single x -> Single x
    | Cons(_, tail) -> tail

let Sort list =
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
        if k <> Length list2
        then go2 (go1 list2) (k + 1)
        else list2
    go2 list 0

let ArrayIntoString (array: array<int>) =
    let stringArr: array<string> = Array.zeroCreate array.Length
    for i = 0 to array.Length - 1 do
        stringArr.[i] <- string array.[i]
    stringArr
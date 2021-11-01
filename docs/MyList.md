# MyList

MyList is the library of non-empty lists which used in BigInt module

## Type

`MyList` is a discriminated union with `One of 't` or `Cons of 't * MyList<t>`

## Functions  

* `stringToMyList (str: string)` - converts string to MyList
* `fold (f: 'a -> 'b -> 'a) (acc: int) (l: MyList<_>)` - implementation of system fold for MyList
* `length (x: MyList)` - returns a length of a list
* `map (f: 'a -> 'b -> 'a) (l: Mylist<_>)` - implementation of system map for MyList
* `iter (f: 'a -> 'b -> 'a) (l: Mylist<_>)` - implementation of system map for MyList
* `concat (l1: Mylist<_>) (l2: Mylist<_>)` - adds l2 to the end of l2
* `systemToMyList (l: list)` - converts system list to MyList
* `myListToSystem (l: MyList<_>)` - converts MyList to system list
* `head (l: MyList<_>)` - returns the first element of the list
* `tail (l: MyList <_>)` - returns a list without first element
* `sort (l: MyList <_>)` - returns a sorted list
* `arrayToString (a: array<int>)` - converts array to string
* `listToString (l: list)` - converts list to string
* `reverse (l: MyList<_>)` - returns a list with elements in a reversed order
* `slice (a: MyList<int>) (x1: int) (x2: int)` - returns the part of the list  from the x1 to the x2 position
* `isEqual (l1: MyList<int>) (l2: MyList<int>)` - checks the two lists for equality

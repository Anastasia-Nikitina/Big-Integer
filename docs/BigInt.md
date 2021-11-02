# BigInt

BigInt is library of a long arithmetic used in the BigInteger

## Type

Main type of BigInt is NumberWithSign. It consists of `sign of Bolean` and `number of <MyList<int>>`. `MyList<int>` consists of integers from 0 to 9

## Functions

* `stringToNWS (str: string)` - converts string to NumberWithSign
* `NWSToString (a: NumberWithSign)` - converts NumberWithSign to string
* `fstGreaterThanSec (x: MyList<int>) (y: MyList<int>)` - returns true if x greater than y else return false
* `addZeros (x: MyList<int>) (y: MyList<int>)` - adds zeros to the beginning in x or in y
* `removeZeros (a: MyList<int>)` - removes zeros from the beginning
* `transfer (x: MyList<_>)` - transfers digits through position
* `goSub (x: MyList<int>) (y: MyList<int>)` - subtracts two numbers without sign
* `goSum (x: MyList<_>) (y: MyList<_>)` - sums up two numbers without sign
* `multToNum (a: MyList<_>) (n: int)` - multiplies number a (without sign) to int n
* `goMult (a: MyList<_>) (b: MyList<_>)` - multiplies two numbers without sign
* `subtraction (a: NumberWithSign) (b: NumberWithSign)` - returns result of subtraction of two numbers
* `addition (a: NumberWithSign) (b : NumberWithSign)` - returns result of addition of two numbers
* `multiplication (a: NumberWithSign) (b: NumberWithSign)` - returns result of multiplication of two numbers
* `selection (x: MyList<int>) (y: MyList<int>)` - selects the quotient of the division
* `goDiv (x: MyList<_>) (y: MyList<_>)` - diviides two numbers without sign
* `goRem (x: MyList<_>) (y: MyList<_>)` - finds the remainder of the division of two numbers without sign
* `division (a: NumberWithSign) (b: NumberWithSign)` - returns result of division of two numbers
* `remainder (a: NumberWithSign) (b: NumberWithSign)` - returns remainder of division of two numbers
* `isEven (a: NumberWithSign)` - determines whether the number is even
* `power (n: NumberWithSign) (pow: NumberWithSign)` - returns then power of number
* `absNWS (a: NumberWithSign)` - returns the absolute value of number
* `unMinus (a: NumberWithSign)` - returns the opposite number in sign
* `toBin (x: NumberWithSign)` - converts a number to binary
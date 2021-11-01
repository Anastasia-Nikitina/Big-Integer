# Language guide

BigIntCalc uses simple programming language to define arithmetic expressions


## Statements

Language support following statements:

*	`print <vname>`
*	`let <vname> = <expression>`

`print` prints a result of arithmetic expression corresponding to a given variable and `let` defines a variable

* `<vname>` begins with lowercase or uppercase letters of the Latin alphabet, followed by numbers or other letters
* `<expression>` can consists of numbers, variables and arithmetic operators such as `+, -, *, /, %, ^, ~, (, ), |`

## Expressions 

*	`Num of <BigInt>`
*	`NVar of <VName>`
*	`Sum of <Expression * Expression>`
*	`Sub of <Expression * Expression>`
*	`Mul of <Expression * Expression>`
*	`Div of <Expression * Expression>`
*	`Rem of <Expression * Expression>`
*	`Pow of <Expression * Expression>`
*	`Bin of <Expression>`
*	`Abs of <Expression>`

## Operators

BigIntCalc support following operators:

* `+` - calculates the sum of two expressions
* `-` - calculates the difference between two expressions; also calculate the number with opposite sign (unary minus)
* `*` - calculates the product of two numbers
* `/` - calculates the whole part of the division
* `%` - calculates the remainder of the division
* `^` - calculates the power of number
* `~` - converts a number to a binary number syste
* `( expr )` - responsible for prioritizing operation
* `| expr |` - calculates the absolute value of the expression

## Code example

	let x = |2^3 - 10| * 5
	let y = (99 + 1) / 10
    let z = x - y
	print z
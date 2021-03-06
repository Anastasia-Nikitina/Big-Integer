# Interpreter

The interpreter can be used by both developers and users

## To developers

To interpretate your code, at first you need to create an abstract sytax tree by using the following function
`Main.parse <string>`

Then you can run the `Interpreter.run <ast>` funtion that returns three dictionaries. The first contains values of all variables in `AST.Expression` format,the second contains variables in `string` format, the third has only one key - `"print"` with string of result of interpretation.
You can also get a dot file which contains a syntax tree by using `DrawTree.drawTree <ast> <output file path>`

### Another functions

* `processExpr (vDict: Dictionary<AST.VName,AST.Expression>) (expr: AST.Expression)` - return a result of a given expression in `BigInt` format
* `processStmt (vDict: Dictionary<AST.VName,AST.Expression>) (pDict:Dictionary<string,string>) (stmt: AST.Stmt)` - gets an expression from a statement and sets it's value to a dictionaries with variable as a key
* `calculate (ast: AST.Stmt list)` - assisting function to compute a result of code with a single statement

### Code example

    let x = "let x = 13 print x"
    let ast = parse x
    let _, _, pDict = Interpreter.run ast
	printfn "%s" pD.[Interpreter.outputBuffer]
Given code prints "13" into console

## To users

Run "BigIntCalc.exe" from console with following commands:

* `--inputfile <file path>` - enter a file with code
* `--inputstring <string>` - enter a string with code
* `--compute` - return the result of interpretation of the code
* `--todot <file path>` - return dot code of syntax tree to the given file
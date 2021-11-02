# Project structure

This page contains some information about the project's structure

## Github repository structure 
	BigInteger
	├── .config - dotnet tools
	├── .github - GitHub Actions setup 
	├── docs - site with documentation in .md format
	├── src - main code of the project
	│   └── BigInteger - Interpreter, MyList and BigInt libraries
	├── tests - tests
	│   ├── BigInteger.UnitTests - tests for BigInt functions
    │   └── Interpreter.UnitTest - tests for interpreter
	├── fsharplint.json - linter config
	├──  mkdocs.yml - MkDocs config
	└── BigInteger.sln - main solution file
# BigInteger

BigInteger is a package for calculation expressions with long numbers. It contains libraries for long arithmetics, non-empty lists and an interpreter for a simple programming language.

## Builds

||Github Actions|
|------|:------:|
|**Build Status**|[![GitHub Actions](https://github.com/Anastasia-Nikitina/Big-Integer/workflows/Build/badge.svg?branch=master)](https://github.com/Anastasia-Nikitina/Big-Integer/actions?query=branch%3Amain) |
|**Build History**|[![Build History](https://buildstats.info/github/chart/Anastasia-Nikitina/Big-Integer)](https://github.com/Anastasia-Nikitina/Big-Integer/actions?query=branch%3Amain) |


## Getting Started

You can install the package with dotnet by following this steps:

* Add a source in your NuGet.config file
#
	dotnet nuget add source "https://nuget.pkg.github.com/Anastasia-Nikitina/index.json"
* Authorize with your github token
#
	paket config add-token "https://nuget.pkg.github.com/Anastasia-Nikitina/index.json" <token>
* Install the package
#
	dotnet add PROJECT package BigInteger --version <version>

## Usage

BigInteger contains a console application with interpreter for arithmetic expressions and libraries for long ariphmetic and non-empty lists.

## Documentation

The [docs](https://Anastasia-Nikitina.github.io/Big-Integer/) contains an overview of the tool and how to use it

## Requirements

* .NET 5.0 or greater


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
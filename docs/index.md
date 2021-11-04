# BigInteger 

BigInteger is a calculator of expressions with long ariphmetics.

## Installation


Package can be installed with dotnet by following these steps:

* Add a source in your NuGet.config file
```
dotnet nuget add source "https://nuget.pkg.github.com/Anastasia-Nikitina/index.json"
```

* Authorize with your github token
```
paket config add-token "https://nuget.pkg.github.com/Anastasia-Nikitina/index.json" <token>
```

* Install the package
```
dotnet add PROJECT package BigInteger --version <version>
```
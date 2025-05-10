# Makefile for C# Matrix Product Solution

CSHARP=dotnet
SRC=.

all: MatrixProduct.dll

test: MatrixProductTest.dll
	$(CSHARP) run --project MatrixProductTest.csproj

MatrixProduct.dll: MatrixProduct.cs
	$(CSHARP) build -o ./bin -c Release /property:GenerateFullPaths=true /property:OutputType=Exe MatrixProduct.cs

MatrixProductTest.dll: MatrixProductTest.cs
	$(CSHARP) build -o ./bin -c Release /property:GenerateFullPaths=true /property:OutputType=Exe MatrixProductTest.cs

run: MatrixProduct.dll
	$(CSHARP) run --project MatrixProduct.csproj matrix.txt

clean:
	rm -rf bin obj *.dll *.exe

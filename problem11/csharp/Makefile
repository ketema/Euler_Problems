# Makefile for C# Matrix Product Solution

CSHARP=dotnet
TEST_PROJECT=MatrixProductTestProject
APP_PROJECT=MatrixProductApp

all: build

build:
	cd $(APP_PROJECT) && $(CSHARP) build

test:
	cd $(TEST_PROJECT) && $(CSHARP) run

run:
	cd $(APP_PROJECT) && $(CSHARP) run matrix.txt

clean:
	rm -rf $(TEST_PROJECT)/bin $(TEST_PROJECT)/obj
	rm -rf $(APP_PROJECT)/bin $(APP_PROJECT)/obj

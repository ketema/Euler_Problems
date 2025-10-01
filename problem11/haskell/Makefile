# Makefile for Haskell Matrix Product Solution

.PHONY: test run build clean setup

# Setup cabal config if needed
setup:
	@if [ ! -f ~/.config/cabal/config ]; then \
		echo "Setting up cabal config..."; \
		mkdir -p ~/.config/cabal; \
		if [ -f ~/.cabal/config ]; then \
			cp ~/.cabal/config ~/.config/cabal/config; \
		else \
			cabal user-config init; \
		fi; \
	fi

# Run tests
test: setup
	cabal test

# Run the solution
run: setup
	cabal run MatrixProduct matrix.txt

# Build the project
build: setup
	cabal build

# Clean build artifacts
clean:
	cabal clean
	rm -rf dist-newstyle

# Update dependencies
update: setup
	cabal update


# Package definition for Euler Problem #26

version       = "1.0.0"
author        = "Project Euler Solutions"
description   = "Reciprocal Cycles - Find d < 1000 with longest cycle in 1/d"
license       = "MIT"
srcDir        = "src"

# Dependencies
requires "nim >= 1.6.0"

# Tasks
task test, "Run tests":
  exec "nim c -r tests/test_reciprocal.nim"

task coverage, "Run tests with coverage":
  exec "testament --coverage tests/"

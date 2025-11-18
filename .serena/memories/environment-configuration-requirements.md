# Environment Configuration Requirements

## Current Language Usage (as of 2025-05-XX)

**Total Unique Languages**: 22 (out of 27 configured in Serena)

**Languages Currently in Use**:
1. c
2. cpp
3. csharp
4. elixir
5. fortran
6. fsharp
7. go
8. haskell
9. java
10. javascript
11. julia
12. kotlin
13. nim (not in Serena supported list)
14. perl
15. php
16. python
17. ruby
18. rust
19. scala
20. shell (bash)
21. swift
22. typescript

**Serena-Supported but Not Yet Used** (5 languages):
- bash (shell used instead)
- clojure
- dart
- erlang
- lua
- markdown
- r
- zig

**Non-Serena Languages in Use** (1 language):
- nim (requires manual configuration or alternative tooling)

## Build Isolation Coverage

**Currently Configured in .envrc Template**:
- Rust: RUST_TARGET_DIR, CARGO_TARGET_DIR
- Python: PYTHON_VENV_DIR, POETRY_VIRTUALENVS_PATH
- Haskell: HASKELL_STACK_WORK
- Node/TypeScript: NODE_MODULES_DIR
- F#: FSHARP_NUGET_DIR

**Languages Needing Build Isolation** (when used):
- Java: `.gradle/`, `.m2/` (Maven)
- Go: `GOPATH`, `GOCACHE`
- Ruby: `GEM_HOME`, `BUNDLE_PATH`
- Scala: `.sbt/`, `.ivy2/`
- C/C++: CMake build dirs
- Kotlin: Gradle build dirs
- Swift: `.build/` directory
- Nim: `nimcache/`

## Maintenance Protocol

**When Starting Problem in New Compiled Language**:
1. Research language build cache directories
2. Update `scripts/templates/envrc.template` with agent-specific variables
3. Sync existing worktrees: `./scripts/worktree_manager.sh sync-envrc`
4. Test isolation (verify builds don't conflict)
5. Document in this memory

**Reference**: See `.serena/memories/polyglot-build-isolation-pattern.md` for detailed patterns

## Discovery vs Fixed Stack

**Euler_Problems**: Discovery-based (languages added as needed)
- Advantage: Flexibility to use any language for any problem
- Challenge: Build isolation must be extended dynamically

**Other Projects**: Fixed stack (known language set)
- Advantage: All build isolation configured upfront
- Euler_Problems will trend toward fixed stack over time as language diversity stabilizes

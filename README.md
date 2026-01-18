# TinyJS

TinyJS is a minimal JavaScript/TypeScript runtime written in Zig, focused on maximum portability for small server-side scripts.

This project is vibe-coded and primarily tested on my OS: Twilight OS.

## Features (current)

- JavaScript + TypeScript (`.ts` uses a best-effort type-stripping prepass)
- Bytecode VM by default, with interpreter fallback (and `--no-vm`)
- Lexical scopes / closures
- Functions: `function f(){}`, function expressions, arrow functions (`=>`)
- Control flow: `if/else`, `while`, `for`, `for..of`, `break/continue`, blocks `{ }`
- Operators: arithmetic, comparisons, strict equality, logical `&&` / `||` / `??`, unary `!` / `-`, `++/--`, compound assignments (`+=` etc)
- Objects/arrays: literals, property access/assignment (`obj.a`, `obj["a"]`), indexing (`arr[0]`)
- Prototype chain basics: `Object.prototype.toString`, `Function.prototype.toString`, `Function.prototype.call`
- Builtins: `console.log/error` (with object/array inspection), `fs` (sync helpers), `Math` (minimal), `Number` (minimal)
- REPL: `TinyJS repl` (or run with no args)

## Quick start

Build:

```sh
zig build
```

Run a file:

```sh
./zig-out/bin/TinyJS <file.js|file.ts>
```

Run the REPL:

```sh
./zig-out/bin/TinyJS
# or:
./zig-out/bin/TinyJS repl
```

## Test scripts

See `test_scripts/README.md`.

# TinyJS test scripts

Run any script with:

```sh
zig build run -- test_scripts/<name>.js
zig build run -- test_scripts/<name>.ts
```

REPL:

```sh
./zig-out/bin/TinyJS repl
./zig-out/bin/TinyJS
```

Notes:
- TinyJS is a small subset; these scripts only use currently-supported syntax/features.
- `fs` is available via `import fs from "fs";`.

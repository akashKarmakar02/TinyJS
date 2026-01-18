const std = @import("std");
const TinyJS = @import("TinyJS");

pub fn main() !void {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_state.deinit();
    const gpa = gpa_state.allocator();

    var args_it = try std.process.argsWithAllocator(gpa);
    defer args_it.deinit();

    const exe_name = args_it.next() orelse "TinyJS";
    const first = args_it.next() orelse {
        try runRepl(gpa, exe_name);
        return;
    };

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    stdout_writer.interface.end = 0;

    var runtime = try TinyJS.Runtime.init(gpa, &stdout_writer.interface);
    defer runtime.deinit();
    try TinyJS.installBuiltins(&runtime);

    if (std.mem.eql(u8, first, "--no-vm")) {
        runtime.use_vm = false;
        const path = args_it.next() orelse {
            try printHelp(exe_name);
            return;
        };
        try runEntrypoint(&runtime, path);
        return;
    }

    if (std.mem.eql(u8, first, "--vm")) {
        runtime.use_vm = true;
        const path = args_it.next() orelse {
            try printHelp(exe_name);
            return;
        };
        try runEntrypoint(&runtime, path);
        return;
    }

    if (std.mem.eql(u8, first, "run")) {
        const maybe_flag_or_path = args_it.next() orelse {
            try printHelp(exe_name);
            return;
        };
        if (std.mem.eql(u8, maybe_flag_or_path, "--no-vm")) {
            runtime.use_vm = false;
            const path = args_it.next() orelse {
                try printHelp(exe_name);
                return;
            };
            try runEntrypoint(&runtime, path);
            return;
        }
        const path = if (std.mem.eql(u8, maybe_flag_or_path, "--vm")) blk: {
            runtime.use_vm = true;
            break :blk args_it.next() orelse {
                try printHelp(exe_name);
                return;
            };
        } else maybe_flag_or_path;
        try runEntrypoint(&runtime, path);
        return;
    }

    if (std.mem.eql(u8, first, "repl")) {
        try runReplWithRuntime(gpa, exe_name, &runtime);
        return;
    }

    if (std.mem.eql(u8, first, "-h") or std.mem.eql(u8, first, "--help")) {
        try printHelp(exe_name);
        return;
    }

    // Default: `TinyJS <file>`
    try runEntrypoint(&runtime, first);
}

fn runRepl(gpa: std.mem.Allocator, exe_name: []const u8) !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    stdout_writer.interface.end = 0;

    var runtime = try TinyJS.Runtime.init(gpa, &stdout_writer.interface);
    defer runtime.deinit();
    try TinyJS.installBuiltins(&runtime);

    try runReplWithRuntime(gpa, exe_name, &runtime);
}

fn runReplWithRuntime(gpa: std.mem.Allocator, exe_name: []const u8, runtime: *TinyJS.Runtime) !void {
    // Keep REPL on the interpreter for now (simpler, also prints expression results).
    runtime.use_vm = false;

    var stdin_buffer: [4096]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    const stdout = runtime.out;

    try stdout.print("{s} REPL (.help for help)\n", .{exe_name});
    try stdout.flush();

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(gpa);

    while (true) {
        const prompt: []const u8 = if (buf.items.len == 0) "> " else "... ";
        try stdout.writeAll(prompt);
        try stdout.flush();

        const line_opt = stdin_reader.interface.takeDelimiter('\n') catch |err| switch (err) {
            error.StreamTooLong => {
                try stdout.writeAll("error: input line too long\n");
                try stdout.flush();
                buf.clearRetainingCapacity();
                continue;
            },
            else => return err,
        };
        if (line_opt == null) break;
        const line = line_opt.?;

        const trimmed = std.mem.trimRight(u8, line, "\r");
        if (buf.items.len == 0) {
            if (std.mem.eql(u8, trimmed, ".exit")) break;
            if (std.mem.eql(u8, trimmed, ".help")) {
                try stdout.writeAll(
                    \\Commands:
                    \\  .help         Show this help
                    \\  .exit         Exit the REPL
                    \\  .load <file>  Run a JS/TS file in the current environment
                    \\
                );
                try stdout.flush();
                continue;
            }
            if (std.mem.startsWith(u8, trimmed, ".load ")) {
                const path = std.mem.trim(u8, trimmed[".load ".len..], " \t");
                if (path.len == 0) continue;
                runtime.runFile(path) catch |err| {
                    try renderRuntimeError(runtime, err);
                };
                continue;
            }
        }

        try buf.appendSlice(gpa, trimmed);
        try buf.append(gpa, '\n');

        runtime.runReplSource(buf.items, "<repl>") catch |err| {
            if (err == error.ParseError) {
                if (runtime.last_parse_diagnostic) |d| {
                    if (d.got.tag == .eof) {
                        // Likely incomplete input; keep buffering.
                        continue;
                    }
                    try d.render(stdout);
                    try stdout.flush();
                    buf.clearRetainingCapacity();
                    continue;
                }
            }
            try renderRuntimeError(runtime, err);
            buf.clearRetainingCapacity();
            continue;
        };

        buf.clearRetainingCapacity();
    }
}

fn runEntrypoint(runtime: *TinyJS.Runtime, path: []const u8) !void {
    runtime.runFile(path) catch |err| {
        try renderRuntimeError(runtime, err);
        return err;
    };
}

fn renderRuntimeError(runtime: *TinyJS.Runtime, err: anyerror) !void {
    if (err == error.ParseError) {
        if (runtime.last_parse_diagnostic) |d| {
            var stderr_buffer: [4096]u8 = undefined;
            var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
            stderr_writer.interface.end = 0;
            const stderr = &stderr_writer.interface;
            try d.render(stderr);
            try stderr.flush();
            std.process.exit(1);
        }
    }
    if (err == error.RuntimeError) {
        if (runtime.last_runtime_diagnostic) |d| {
            var stderr_buffer: [4096]u8 = undefined;
            var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
            stderr_writer.interface.end = 0;
            const stderr = &stderr_writer.interface;
            try d.render(stderr);
            try stderr.flush();
            std.process.exit(1);
        }
    }
}

fn printHelp(exe_name: []const u8) !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    stdout_writer.interface.end = 0;
    const stdout = &stdout_writer.interface;
    try stdout.print(
        \\{s} - tiny JavaScript/TypeScript runtime (minimal subset)
        \\
        \\Usage:
        \\  {s} <file.js|file.ts>
        \\  {s} run <file.js|file.ts>
        \\  {s} repl
        \\  {s} --no-vm <file.js|file.ts>
        \\  {s} run --no-vm <file.js|file.ts>
        \\
        \\Notes:
        \\  - Runs on the bytecode VM by default (fallback to interpreter on VM compile failures).
        \\  - TS support is a light, best-effort type-stripping prepass.
        \\
    , .{ exe_name, exe_name, exe_name, exe_name, exe_name, exe_name });
    try stdout.flush();
}

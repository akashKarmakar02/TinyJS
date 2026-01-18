const std = @import("std");
const rtmod = @import("../runtime.zig");

const Runtime = rtmod.Runtime;
const Value = rtmod.Value;
const Object = rtmod.Object;
const Array = rtmod.Array;

pub fn install(rt: *Runtime, console_obj: *rtmod.Object) !void {
    try rt.putProp(console_obj, "log", .{ .native_fn = .{ .func = nativeConsoleLog } });
    try rt.putProp(console_obj, "error", .{ .native_fn = .{ .func = nativeConsoleError } });
}

fn nativeConsoleLog(rt: *Runtime, _: Value, args: []const Value) !Value {
    for (args, 0..) |arg, i| {
        if (i != 0) try rt.out.writeByte(' ');
        try writeConsoleArgTo(rt, rt.out, arg);
    }
    try rt.out.writeByte('\n');
    try rt.out.flush();
    return .undefined;
}

fn nativeConsoleError(rt: *Runtime, _: Value, args: []const Value) !Value {
    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    stderr_writer.interface.end = 0;
    const w = &stderr_writer.interface;
    for (args, 0..) |arg, i| {
        if (i != 0) try w.writeByte(' ');
        try writeConsoleArgTo(rt, w, arg);
    }
    try w.writeByte('\n');
    try w.flush();
    return .undefined;
}

fn writeConsoleArgTo(rt: *Runtime, w: anytype, v: Value) !void {
    // Match Node-ish behavior: top-level strings print raw, but arrays/objects are inspected.
    return switch (v) {
        .string => |s| w.writeAll(s.bytes),
        else => {
            var ctx: InspectCtx = .{ .max_depth = 4 };
            defer ctx.deinit(rt.gpa);
            try writeInspectTo(rt, w, v, &ctx);
        },
    };
}

const InspectCtx = struct {
    max_depth: u32,
    visited: std.AutoHashMapUnmanaged(usize, void) = .{},

    fn deinit(self: *InspectCtx, a: std.mem.Allocator) void {
        self.visited.deinit(a);
        self.* = undefined;
    }
};

fn writeInspectTo(rt: *Runtime, w: anytype, v: Value, ctx: *InspectCtx) anyerror!void {
    switch (v) {
        .undefined => try w.writeAll("undefined"),
        .null => try w.writeAll("null"),
        .boolean => |b| try w.writeAll(if (b) "true" else "false"),
        .number => |n| try w.print("{d}", .{n}),
        .string => |s| try writeQuotedString(w, s.bytes),
        .native_fn, .native_function, .function => try w.writeAll("[Function]"),
        .array => |arr| try writeInspectArray(rt, w, arr, ctx),
        .object => |obj| try writeInspectObject(rt, w, obj, ctx),
    }
}

fn writeInspectArray(rt: *Runtime, w: anytype, arr: *Array, ctx: *InspectCtx) !void {
    const ptr_key: usize = @intFromPtr(arr);
    if (ctx.visited.contains(ptr_key)) {
        try w.writeAll("[Circular]");
        return;
    }
    try ctx.visited.put(rt.gpa, ptr_key, {});
    defer _ = ctx.visited.remove(ptr_key);

    if (ctx.max_depth == 0) {
        try w.writeAll("[Array]");
        return;
    }
    ctx.max_depth -= 1;
    defer ctx.max_depth += 1;

    try w.writeByte('[');
    for (arr.items.items, 0..) |item, i| {
        if (i != 0) try w.writeAll(", ");
        try writeInspectTo(rt, w, item, ctx);
    }
    try w.writeByte(']');
}

fn writeInspectObject(rt: *Runtime, w: anytype, obj: *Object, ctx: *InspectCtx) !void {
    const ptr_key: usize = @intFromPtr(obj);
    if (ctx.visited.contains(ptr_key)) {
        try w.writeAll("[Circular]");
        return;
    }
    try ctx.visited.put(rt.gpa, ptr_key, {});
    defer _ = ctx.visited.remove(ptr_key);

    if (ctx.max_depth == 0) {
        try w.writeAll("[Object]");
        return;
    }
    ctx.max_depth -= 1;
    defer ctx.max_depth += 1;

    var keys: std.ArrayListUnmanaged([]const u8) = .{};
    defer keys.deinit(rt.gpa);
    var it = obj.props.iterator();
    while (it.next()) |entry| {
        try keys.append(rt.gpa, entry.key_ptr.*);
    }
    std.sort.pdq([]const u8, keys.items, {}, struct {
        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.lessThan(u8, a, b);
        }
    }.lessThan);

    try w.writeAll("{ ");
    for (keys.items, 0..) |k, i| {
        if (i != 0) try w.writeAll(", ");
        try writeObjectKey(w, k);
        try w.writeAll(": ");
        const val = obj.props.get(k) orelse .undefined;
        try writeInspectTo(rt, w, val, ctx);
    }
    try w.writeAll(" }");
}

fn writeObjectKey(w: anytype, key: []const u8) !void {
    if (isIdent(key)) return w.writeAll(key);
    try writeQuotedString(w, key);
}

fn isIdent(s: []const u8) bool {
    if (s.len == 0) return false;
    if (!isIdentStart(s[0])) return false;
    for (s[1..]) |c| if (!isIdentContinue(c)) return false;
    return true;
}

fn isIdentStart(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_' or c == '$';
}

fn isIdentContinue(c: u8) bool {
    return isIdentStart(c) or (c >= '0' and c <= '9');
}

fn writeQuotedString(w: anytype, s: []const u8) !void {
    try w.writeByte('\'');
    for (s) |c| {
        switch (c) {
            '\\' => try w.writeAll("\\\\"),
            '\'' => try w.writeAll("\\'"),
            '\n' => try w.writeAll("\\n"),
            '\r' => try w.writeAll("\\r"),
            '\t' => try w.writeAll("\\t"),
            else => try w.writeByte(c),
        }
    }
    try w.writeByte('\'');
}

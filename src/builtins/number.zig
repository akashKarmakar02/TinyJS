const std = @import("std");
const rtmod = @import("../runtime.zig");

const Runtime = rtmod.Runtime;
const Value = rtmod.Value;
const NativeFunctionObject = rtmod.NativeFunctionObject;

pub fn install(rt: *Runtime, number_fn: *NativeFunctionObject) !void {
    // Constants (minimal)
    try rt.putPropNativeFunction(number_fn, "NaN", .{ .number = std.math.nan(f64) });
    try rt.putPropNativeFunction(number_fn, "POSITIVE_INFINITY", .{ .number = std.math.inf(f64) });
    try rt.putPropNativeFunction(number_fn, "NEGATIVE_INFINITY", .{ .number = -std.math.inf(f64) });

    // Predicates / parsing
    try rt.putPropNativeFunction(number_fn, "isNaN", .{ .native_fn = .{ .func = nativeIsNaN } });
    try rt.putPropNativeFunction(number_fn, "isFinite", .{ .native_fn = .{ .func = nativeIsFinite } });
    try rt.putPropNativeFunction(number_fn, "parseInt", .{ .native_fn = .{ .func = nativeParseInt } });
    try rt.putPropNativeFunction(number_fn, "parseFloat", .{ .native_fn = .{ .func = nativeParseFloat } });
}

pub fn nativeNumber(_: *Runtime, _: Value, args: []const Value) !Value {
    const v = if (args.len >= 1) args[0] else .undefined;
    return toNumber(v);
}

fn nativeIsNaN(_: *Runtime, _: Value, args: []const Value) !Value {
    if (args.len < 1) return .{ .boolean = false };
    return switch (args[0]) {
        .number => |n| .{ .boolean = std.math.isNan(n) },
        else => .{ .boolean = false },
    };
}

fn nativeIsFinite(_: *Runtime, _: Value, args: []const Value) !Value {
    if (args.len < 1) return .{ .boolean = false };
    return switch (args[0]) {
        .number => |n| .{ .boolean = std.math.isFinite(n) },
        else => .{ .boolean = false },
    };
}

fn nativeParseFloat(_: *Runtime, _: Value, args: []const Value) !Value {
    const v = if (args.len >= 1) args[0] else .undefined;
    return parseFloat(v);
}

fn nativeParseInt(_: *Runtime, _: Value, args: []const Value) !Value {
    const input = if (args.len >= 1) args[0] else .undefined;
    const radix_val = if (args.len >= 2) args[1] else .undefined;
    return parseInt(input, radix_val);
}

fn toNumber(v: Value) Value {
    return switch (v) {
        .number => v,
        .boolean => |b| .{ .number = if (b) 1 else 0 },
        .null => .{ .number = 0 },
        .undefined => .{ .number = std.math.nan(f64) },
        .string => |s| blk: {
            const trimmed = std.mem.trim(u8, s.bytes, " \t\r\n");
            if (trimmed.len == 0) break :blk .{ .number = 0 };
            const parsed = std.fmt.parseFloat(f64, trimmed) catch break :blk .{ .number = std.math.nan(f64) };
            break :blk .{ .number = parsed };
        },
        else => .{ .number = std.math.nan(f64) },
    };
}

fn parseFloat(v: Value) Value {
    return switch (v) {
        .number => v,
        .string => |s| parseFloatString(s.bytes),
        .boolean => |b| parseFloatString(if (b) "true" else "false"),
        .null => parseFloatString("null"),
        .undefined => .{ .number = std.math.nan(f64) },
        else => .{ .number = std.math.nan(f64) },
    };
}

fn parseFloatString(s: []const u8) Value {
    const trimmed = std.mem.trim(u8, s, " \t\r\n");
    if (trimmed.len == 0) return .{ .number = std.math.nan(f64) };

    // Parse a numeric prefix (minimal, but handles common cases).
    var i: usize = 0;
    if (trimmed[i] == '+' or trimmed[i] == '-') i += 1;
    var saw_digit = false;
    while (i < trimmed.len and std.ascii.isDigit(trimmed[i])) : (i += 1) saw_digit = true;
    if (i < trimmed.len and trimmed[i] == '.') {
        i += 1;
        while (i < trimmed.len and std.ascii.isDigit(trimmed[i])) : (i += 1) saw_digit = true;
    }
    if (!saw_digit) return .{ .number = std.math.nan(f64) };
    const prefix = trimmed[0..i];
    const parsed = std.fmt.parseFloat(f64, prefix) catch std.math.nan(f64);
    return .{ .number = parsed };
}

fn parseInt(input: Value, radix_val: Value) Value {
    const s = switch (input) {
        .string => |str| str.bytes,
        .number => |n| blk: {
            if (std.math.isNan(n) or !std.math.isFinite(n)) break :blk "";
            // Truncate toward zero as JS does before string conversion in many cases.
            // We keep it simple and format as an integer.
            var buf: [64]u8 = undefined;
            const i: i64 = @intFromFloat(std.math.trunc(n));
            const out = std.fmt.bufPrint(&buf, "{d}", .{i}) catch "";
            break :blk out;
        },
        .boolean => |b| if (b) "true" else "false",
        .null => "null",
        .undefined => "",
        else => "",
    };

    var radix: i64 = switch (radix_val) {
        .undefined, .null => 0,
        .number => |n| @intFromFloat(std.math.trunc(n)),
        else => 0,
    };

    const trimmed = std.mem.trim(u8, s, " \t\r\n");
    if (trimmed.len == 0) return .{ .number = std.math.nan(f64) };

    var start: usize = 0;
    var sign: i64 = 1;
    if (trimmed[start] == '+') {
        start += 1;
    } else if (trimmed[start] == '-') {
        sign = -1;
        start += 1;
    }
    if (start >= trimmed.len) return .{ .number = std.math.nan(f64) };

    if (radix == 0) {
        if (trimmed.len - start >= 2 and trimmed[start] == '0' and (trimmed[start + 1] == 'x' or trimmed[start + 1] == 'X')) {
            radix = 16;
            start += 2;
        } else {
            radix = 10;
        }
    }
    if (radix < 2 or radix > 36) return .{ .number = std.math.nan(f64) };

    var acc: i64 = 0;
    var saw_any = false;
    var i: usize = start;
    while (i < trimmed.len) : (i += 1) {
        const d = digitValue(trimmed[i]) orelse break;
        if (d >= radix) break;
        saw_any = true;
        acc = acc * radix + d;
    }
    if (!saw_any) return .{ .number = std.math.nan(f64) };
    return .{ .number = @as(f64, @floatFromInt(sign * acc)) };
}

fn digitValue(c: u8) ?i64 {
    if (c >= '0' and c <= '9') return @intCast(c - '0');
    if (c >= 'a' and c <= 'z') return @intCast(10 + (c - 'a'));
    if (c >= 'A' and c <= 'Z') return @intCast(10 + (c - 'A'));
    return null;
}


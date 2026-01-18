const std = @import("std");
const rtmod = @import("../runtime.zig");

const Runtime = rtmod.Runtime;
const Value = rtmod.Value;
const Array = rtmod.Array;
const Object = rtmod.Object;

pub fn installPrototype(rt: *Runtime, array_proto: *Object) !void {
    try rt.putProp(array_proto, "push", .{ .native_fn = .{ .func = nativePush } });
    try rt.putProp(array_proto, "pop", .{ .native_fn = .{ .func = nativePop } });
    try rt.putProp(array_proto, "forEach", .{ .native_fn = .{ .func = nativeForEach } });
    try rt.putProp(array_proto, "map", .{ .native_fn = .{ .func = nativeMap } });
    try rt.putProp(array_proto, "indexOf", .{ .native_fn = .{ .func = nativeIndexOf } });
    try rt.putProp(array_proto, "includes", .{ .native_fn = .{ .func = nativeIncludes } });
}

fn asArray(this_value: Value) !*Array {
    return switch (this_value) {
        .array => |a| a,
        else => error.RuntimeError,
    };
}

fn nativePush(rt: *Runtime, this_value: Value, args: []const Value) !Value {
    const arr = try asArray(this_value);
    for (args) |v| try arr.items.append(rt.gpa, v);
    return .{ .number = @floatFromInt(arr.items.items.len) };
}

fn nativePop(rt: *Runtime, this_value: Value, _: []const Value) !Value {
    _ = rt;
    const arr = try asArray(this_value);
    if (arr.items.items.len == 0) return .undefined;
    const last = arr.items.items[arr.items.items.len - 1];
    arr.items.items.len -= 1;
    return last;
}

fn nativeForEach(rt: *Runtime, this_value: Value, args: []const Value) !Value {
    const arr = try asArray(this_value);
    if (args.len < 1) return error.InvalidArguments;
    const callback = args[0];
    const cb_this = if (args.len >= 2) args[1] else .undefined;

    for (arr.items.items, 0..) |item, i| {
        const argv: [3]Value = .{ item, .{ .number = @floatFromInt(i) }, .{ .array = arr } };
        _ = try rt.callValueDirect(callback, cb_this, argv[0..]);
    }
    return .undefined;
}

fn nativeMap(rt: *Runtime, this_value: Value, args: []const Value) !Value {
    const arr = try asArray(this_value);
    if (args.len < 1) return error.InvalidArguments;
    const callback = args[0];
    const cb_this = if (args.len >= 2) args[1] else .undefined;

    const out_arr = try rt.newArray();
    for (arr.items.items, 0..) |item, i| {
        const argv: [3]Value = .{ item, .{ .number = @floatFromInt(i) }, .{ .array = arr } };
        const mapped = try rt.callValueDirect(callback, cb_this, argv[0..]);
        try out_arr.items.append(rt.gpa, mapped);
    }
    return .{ .array = out_arr };
}

fn nativeIndexOf(_: *Runtime, this_value: Value, args: []const Value) !Value {
    const arr = try asArray(this_value);
    const search: Value = if (args.len >= 1) args[0] else .undefined;
    const from: i64 = try toFromIndex(args, 1);
    const start = clampStart(from, @intCast(arr.items.items.len));

    var i: usize = start;
    while (i < arr.items.items.len) : (i += 1) {
        if (isStrictEqual(arr.items.items[i], search)) {
            return .{ .number = @as(f64, @floatFromInt(i)) };
        }
    }
    return .{ .number = -1 };
}

fn nativeIncludes(_: *Runtime, this_value: Value, args: []const Value) !Value {
    const arr = try asArray(this_value);
    const search: Value = if (args.len >= 1) args[0] else .undefined;
    const from: i64 = try toFromIndex(args, 1);
    const start = clampStart(from, @intCast(arr.items.items.len));

    var i: usize = start;
    while (i < arr.items.items.len) : (i += 1) {
        if (sameValueZero(arr.items.items[i], search)) return .{ .boolean = true };
    }
    return .{ .boolean = false };
}

fn toFromIndex(args: []const Value, index: usize) !i64 {
    if (args.len <= index) return 0;
    return switch (args[index]) {
        .undefined, .null => 0,
        .boolean => |b| if (b) 1 else 0,
        .number => |n| @intFromFloat(std.math.floor(n)),
        .string => |s| blk: {
            const trimmed = std.mem.trim(u8, s.bytes, " \t\r\n");
            const n = std.fmt.parseFloat(f64, trimmed) catch 0;
            break :blk @intFromFloat(std.math.floor(n));
        },
        else => error.InvalidArguments,
    };
}

fn clampStart(from: i64, len: i64) usize {
    var start = from;
    if (start < 0) start = len + start;
    if (start < 0) start = 0;
    if (start > len) start = len;
    return @intCast(start);
}

fn isStrictEqual(a: Value, b: Value) bool {
    return switch (a) {
        .undefined => b == .undefined,
        .null => b == .null,
        .boolean => |av| switch (b) { .boolean => |bv| av == bv, else => false },
        .number => |av| switch (b) { .number => |bv| av == bv, else => false },
        .string => |av| switch (b) { .string => |bv| std.mem.eql(u8, av.bytes, bv.bytes), else => false },
        .object => |av| switch (b) { .object => |bv| av == bv, else => false },
        .array => |av| switch (b) { .array => |bv| av == bv, else => false },
        .native_fn => |av| switch (b) { .native_fn => |bv| av.func == bv.func, else => false },
        .native_function => |av| switch (b) { .native_function => |bv| av == bv, else => false },
        .function => |av| switch (b) { .function => |bv| av == bv, else => false },
    };
}

fn sameValueZero(a: Value, b: Value) bool {
    if (isStrictEqual(a, b)) return true;
    return switch (a) {
        .number => |av| switch (b) {
            .number => |bv| std.math.isNan(av) and std.math.isNan(bv),
            else => false,
        },
        else => false,
    };
}

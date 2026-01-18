const std = @import("std");
const rtmod = @import("../runtime.zig");

const Runtime = rtmod.Runtime;
const Value = rtmod.Value;

pub fn install(rt: *Runtime, fs_obj: *rtmod.Object) !void {
    try rt.putProp(fs_obj, "readFileSync", .{ .native_fn = .{ .func = nativeFsReadFileSync } });
    try rt.putProp(fs_obj, "writeFileSync", .{ .native_fn = .{ .func = nativeFsWriteFileSync } });
    try rt.putProp(fs_obj, "existsSync", .{ .native_fn = .{ .func = nativeFsExistsSync } });
    try rt.putProp(fs_obj, "readdirSync", .{ .native_fn = .{ .func = nativeFsReaddirSync } });
    try rt.putProp(fs_obj, "mkdirSync", .{ .native_fn = .{ .func = nativeFsMkdirSync } });
    try rt.putProp(fs_obj, "cwd", .{ .native_fn = .{ .func = nativeFsCwd } });
}

fn expectString(args: []const Value, index: usize) ![]const u8 {
    if (args.len <= index) return error.InvalidArguments;
    return switch (args[index]) {
        .string => |s| s.bytes,
        else => error.InvalidArguments,
    };
}

fn valueToBytes(rt: *Runtime, v: Value) ![]const u8 {
    return switch (v) {
        .string => |s| s.bytes,
        .number => |n| blk: {
            const tmp = try std.fmt.allocPrint(rt.gpa, "{d}", .{n});
            defer rt.gpa.free(tmp);
            const s = try rt.newString(tmp);
            break :blk s.bytes;
        },
        .boolean => |b| if (b) "true" else "false",
        .null => "null",
        .undefined => "undefined",
        else => error.InvalidArguments,
    };
}

fn nativeFsReadFileSync(rt: *Runtime, _: Value, args: []const Value) !Value {
    const path = try expectString(args, 0);
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const max_bytes: usize = 64 * 1024 * 1024;
    const tmp = try file.readToEndAlloc(rt.gpa, max_bytes);
    defer rt.gpa.free(tmp);
    return .{ .string = try rt.newString(tmp) };
}

fn nativeFsWriteFileSync(rt: *Runtime, _: Value, args: []const Value) !Value {
    const path = try expectString(args, 0);
    const data_val = if (args.len >= 2) args[1] else .undefined;
    const data = try valueToBytes(rt, data_val);

    var file = try std.fs.cwd().createFile(path, .{ .truncate = true });
    defer file.close();
    try file.writeAll(data);
    return .undefined;
}

fn nativeFsExistsSync(_: *Runtime, _: Value, args: []const Value) !Value {
    const path = try expectString(args, 0);
    std.fs.cwd().access(path, .{}) catch return .{ .boolean = false };
    return .{ .boolean = true };
}

fn nativeFsReaddirSync(rt: *Runtime, _: Value, args: []const Value) !Value {
    const dir_path = try expectString(args, 0);

    var dir = try std.fs.cwd().openDir(dir_path, .{ .iterate = true });
    defer dir.close();

    const arr = try rt.newArray();
    var it = dir.iterate();
    while (try it.next()) |entry| {
        try arr.items.append(rt.gpa, .{ .string = try rt.newString(entry.name) });
    }
    return .{ .array = arr };
}

fn nativeFsMkdirSync(_: *Runtime, _: Value, args: []const Value) !Value {
    const dir_path = try expectString(args, 0);
    try std.fs.cwd().makePath(dir_path);
    return .undefined;
}

fn nativeFsCwd(rt: *Runtime, _: Value, _: []const Value) !Value {
    const tmp = try std.process.getCwdAlloc(rt.gpa);
    defer rt.gpa.free(tmp);
    return .{ .string = try rt.newString(tmp) };
}

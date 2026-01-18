const std = @import("std");
const ast = @import("ast.zig");
const parser_mod = @import("parser.zig");

pub const Runtime = struct {
    gpa: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    out: *std.io.Writer,

    root_env: *Environment = undefined,
    current_env: *Environment = undefined,
    envs: std.ArrayListUnmanaged(*Environment) = .{},
    modules: std.StringHashMapUnmanaged(Value) = .{}, // resolved path -> module namespace object
    gc_head: ?*GcHeader = null,
    gc_bytes: usize = 0,
    gc_threshold: usize = 1024 * 1024,
    gc_temp_roots: std.ArrayListUnmanaged(Value) = .{},
    gc_call_roots: std.ArrayListUnmanaged(CallRoot) = .{},
    vm_stack: std.ArrayListUnmanaged(Value) = .{},
    vm_chunks: std.ArrayListUnmanaged(*Chunk) = .{},
    use_vm: bool = true,

    builtin_console: Value = .undefined,
    builtin_fs: Value = .undefined,
    builtin_object: Value = .undefined,
    object_prototype: ?*Object = null,
    builtin_function: Value = .undefined,
    function_prototype: ?*Object = null,
    builtin_array: Value = .undefined,
    array_prototype: ?*Object = null,
    builtin_math: Value = .undefined,
    builtin_number: Value = .undefined, // Number function object
    global_this: Value = .undefined,
    current_this: Value = .undefined,
    loop_depth: u32 = 0,
    last_parse_diagnostic: ?parser_mod.Parser.Diagnostic = null,
    last_runtime_diagnostic: ?RuntimeDiagnostic = null,
    current_source_name: []const u8 = "<unknown>",
    current_source: []const u8 = "",

    pub fn init(gpa: std.mem.Allocator, out: *std.io.Writer) !Runtime {
        var self: Runtime = .{
            .gpa = gpa,
            .arena = std.heap.ArenaAllocator.init(gpa),
            .out = out,
        };
        errdefer self.arena.deinit();
        try self.initEnvironments();
        return self;
    }

    pub fn deinit(self: *Runtime) void {
        for (self.envs.items) |env| env.bindings.deinit(self.gpa);
        self.envs.deinit(self.gpa);

        self.modules.deinit(self.gpa);
        self.gc_temp_roots.deinit(self.gpa);
        self.gc_call_roots.deinit(self.gpa);
        self.vm_stack.deinit(self.gpa);
        self.vm_chunks.deinit(self.gpa);
        self.gcFreeAll();
        self.arena.deinit();
        self.* = undefined;
    }

    pub fn runFile(self: *Runtime, path: []const u8) !void {
        const resolved = try self.resolveEntryPath(path);
        const src_raw = try self.readFileAlloc(resolved);
        const src = if (std.mem.endsWith(u8, resolved, ".ts")) try stripTypescript(self, src_raw) else src_raw;
        try self.runSource(src, resolved);
    }

    pub fn runSource(self: *Runtime, source: []const u8, source_name: []const u8) !void {
        self.last_parse_diagnostic = null;
        self.last_runtime_diagnostic = null;
        const prev_name = self.current_source_name;
        const prev_src = self.current_source;
        self.current_source_name = source_name;
        self.current_source = source;
        defer {
            self.current_source_name = prev_name;
            self.current_source = prev_src;
        }
        var parser = parser_mod.Parser.init(self.arena.allocator(), source, source_name);
        const program = parser.parseProgram() catch |err| {
            if (err == error.ParseError) self.last_parse_diagnostic = parser.last_diagnostic;
            return err;
        };
        if (self.use_vm) {
            self.compileAndExec(program, source_name) catch |err| {
                // VM is still evolving; fall back to the AST interpreter if compilation/execution fails.
                if (err == error.RuntimeError) return err;
                return self.evalProgram(program, source_name);
            };
        } else {
            try self.evalProgram(program, source_name);
        }
    }

    // REPL helper: executes `source` and prints the value of a lone expression statement
    // using `console.log(...)` formatting.
    pub fn runReplSource(self: *Runtime, source: []const u8, source_name: []const u8) !void {
        self.last_parse_diagnostic = null;
        self.last_runtime_diagnostic = null;
        const prev_name = self.current_source_name;
        const prev_src = self.current_source;
        self.current_source_name = source_name;
        self.current_source = source;
        defer {
            self.current_source_name = prev_name;
            self.current_source = prev_src;
        }

        var parser = parser_mod.Parser.init(self.arena.allocator(), source, source_name);
        const program = parser.parseProgram() catch |err| {
            if (err == error.ParseError) self.last_parse_diagnostic = parser.last_diagnostic;
            return err;
        };

        if (program.statements.len == 1) {
            switch (program.statements[0]) {
                .expr_stmt => |s| {
                    // REPL prints expression results.
                    const v = try self.evalExpr(s.expr);
                    try self.replPrintValue(v);
                    return;
                },
                else => {},
            }
        }

        // For now, keep REPL on the interpreter path for correctness and simplicity.
        try self.evalProgram(program, source_name);
    }

    fn replPrintValue(self: *Runtime, v: Value) !void {
        const console_obj = switch (self.builtin_console) {
            .object => |o| o,
            else => {
                try self.writeValue(v);
                try self.out.writeByte('\n');
                try self.out.flush();
                return;
            },
        };
        const logv = self.getPropChain(console_obj, "log") orelse .undefined;
        if (logv != .function and logv != .native_fn and logv != .native_function) {
            try self.writeValue(v);
            try self.out.writeByte('\n');
            try self.out.flush();
            return;
        }
        const argv: [1]Value = .{v};
        _ = try self.callValueDirect(logv, self.builtin_console, argv[0..]);
    }

    fn resolveEntryPath(self: *Runtime, path: []const u8) ![]const u8 {
        if (std.fs.path.isAbsolute(path)) return self.dupe(path);
        // Keep entry paths stable for the module cache.
        const tmp = try std.fs.path.resolve(self.gpa, &.{ ".", path });
        defer self.gpa.free(tmp);
        return self.dupe(tmp);
    }

    fn initEnvironments(self: *Runtime) !void {
        const env = try self.arena.allocator().create(Environment);
        env.* = .{ .parent = null };
        self.root_env = env;
        self.current_env = env;
        try self.envs.append(self.gpa, env);
    }

    pub fn define(self: *Runtime, name: []const u8, value: Value, mutable: bool) !void {
        try self.current_env.bindings.put(self.gpa, try self.dupe(name), .{ .value = value, .mutable = mutable });
    }

    fn get(self: *Runtime, name: []const u8) ?Value {
        var env: ?*Environment = self.current_env;
        while (env) |e| : (env = e.parent) {
            if (e.bindings.get(name)) |b| return b.value;
        }
        return null;
    }

    fn assign(self: *Runtime, name: []const u8, value: Value, span: ast.Span) !void {
        var env: ?*Environment = self.current_env;
        while (env) |e| : (env = e.parent) {
            if (e.bindings.getPtr(name)) |b| {
                if (!b.mutable) {
                    self.noteRuntimeErrorFmt(span, "TypeError: Assignment to constant variable '{s}'.", .{name});
                    return error.RuntimeError;
                }
                b.value = value;
                return;
            }
        }
        // JS-like (sloppy) fallback: create on root scope if missing.
        try self.root_env.bindings.put(self.gpa, try self.dupe(name), .{ .value = value, .mutable = true });
    }

    pub fn pushScope(self: *Runtime) !void {
        const env = try self.arena.allocator().create(Environment);
        env.* = .{ .parent = self.current_env };
        self.current_env = env;
        try self.envs.append(self.gpa, env);
    }

    pub fn popScope(self: *Runtime) void {
        if (self.current_env.parent) |p| self.current_env = p;
    }

    pub fn newObject(self: *Runtime) !*Object {
        const proto = self.object_prototype;
        return self.newObjectRaw(proto);
    }

    pub fn newObjectRaw(self: *Runtime, proto: ?*Object) !*Object {
        const obj = try self.gpa.create(Object);
        obj.* = .{ .prototype = proto };
        obj.header = .{ .kind = .object, .marked = false, .next = self.gc_head };
        self.gc_head = &obj.header;
        self.gc_bytes += @sizeOf(Object);
        return obj;
    }

    pub fn newArray(self: *Runtime) !*Array {
        const proto = self.array_prototype orelse self.object_prototype;
        return self.newArrayRaw(proto);
    }

    pub fn newArrayRaw(self: *Runtime, proto: ?*Object) !*Array {
        const arr = try self.gpa.create(Array);
        arr.* = .{ .prototype = proto };
        arr.header = .{ .kind = .array, .marked = false, .next = self.gc_head };
        self.gc_head = &arr.header;
        self.gc_bytes += @sizeOf(Array);
        return arr;
    }

    fn newFunction(
        self: *Runtime,
        name: []const u8,
        params: []const []const u8,
        body: FunctionBody,
        closure_env: *Environment,
        is_arrow: bool,
        arrow_this: Value,
        source_name: []const u8,
        source: []const u8,
        source_span: ?ast.Span,
    ) !*Function {
        const f = try self.gpa.create(Function);
        f.* = .{
            .name = name,
            .params = params,
            .body = body,
            .closure_env = closure_env,
            .is_arrow = is_arrow,
            .arrow_this = arrow_this,
            .source_name = source_name,
            .source = source,
            .source_span = source_span,
        };
        f.header = .{ .kind = .function, .marked = false, .next = self.gc_head };
        self.gc_head = &f.header;
        self.gc_bytes += @sizeOf(Function);
        return f;
    }

    pub fn newString(self: *Runtime, bytes: []const u8) !*GcString {
        const owned = try self.gpa.dupe(u8, bytes);
        errdefer self.gpa.free(owned);
        return self.newStringOwned(owned);
    }

    pub fn newStringOwned(self: *Runtime, owned: []u8) !*GcString {
        const s = try self.gpa.create(GcString);
        errdefer self.gpa.destroy(s);
        s.* = .{ .bytes = owned };
        s.header = .{ .kind = .string, .marked = false, .next = self.gc_head };
        self.gc_head = &s.header;
        self.gc_bytes += @sizeOf(GcString) + owned.len;
        return s;
    }

    pub fn newNativeFunctionObject(self: *Runtime, func: NativeFn, name: []const u8) !*NativeFunctionObject {
        const fno = try self.gpa.create(NativeFunctionObject);
        fno.* = .{ .prototype = self.function_prototype, .func = .{ .func = func }, .name = name };
        fno.header = .{ .kind = .native_function_object, .marked = false, .next = self.gc_head };
        self.gc_head = &fno.header;
        self.gc_bytes += @sizeOf(NativeFunctionObject);
        return fno;
    }

    pub fn putPropNativeFunction(self: *Runtime, fnobj: *NativeFunctionObject, key: []const u8, value: Value) !void {
        if (fnobj.props.getPtr(key)) |p| {
            p.* = value;
            return;
        }
        const owned = try self.gpa.dupe(u8, key);
        errdefer self.gpa.free(owned);
        try fnobj.props.put(self.gpa, owned, value);
    }

    fn maybeCollect(self: *Runtime) void {
        if (self.gc_bytes < self.gc_threshold) return;
        self.collectGarbage();
        self.gc_threshold = @max(@as(usize, 1024 * 1024), self.gc_bytes * 2);
    }

    fn collectGarbage(self: *Runtime) void {
        self.markRoots();
        self.sweep();
    }

    fn markRoots(self: *Runtime) void {
        self.markValue(self.global_this);
        self.markValue(self.builtin_console);
        self.markValue(self.builtin_fs);
        self.markValue(self.builtin_object);
        if (self.object_prototype) |p| self.markHeader(&p.header);
        self.markValue(self.builtin_function);
        if (self.function_prototype) |p| self.markHeader(&p.header);
        self.markValue(self.current_this);

        self.markEnvironment(self.current_env);
        for (self.gc_call_roots.items) |r| {
            self.markEnvironment(r.env);
            self.markValue(r.this_value);
        }

        var it = self.modules.iterator();
        while (it.next()) |entry| self.markValue(entry.value_ptr.*);

        for (self.gc_temp_roots.items) |v| self.markValue(v);
        for (self.vm_stack.items) |v| self.markValue(v);
        for (self.vm_chunks.items) |c| self.markChunkConstants(c);
    }

    fn markChunkConstants(self: *Runtime, chunk: *Chunk) void {
        for (chunk.consts.items) |c| switch (c) {
            .value => |v| self.markValue(v),
            .proto => |p| self.markChunkConstants(p.chunk),
            .name => {},
        };
    }

    fn pushTempRoot(self: *Runtime, v: Value) !void {
        try self.gc_temp_roots.append(self.gpa, v);
    }

    fn popTempRoots(self: *Runtime, base_len: usize) void {
        self.gc_temp_roots.shrinkRetainingCapacity(base_len);
    }

    fn markEnvironment(self: *Runtime, start: *Environment) void {
        var env: ?*Environment = start;
        while (env) |e| : (env = e.parent) {
            var it = e.bindings.iterator();
            while (it.next()) |entry| self.markValue(entry.value_ptr.value);
        }
    }

    fn markValue(self: *Runtime, v: Value) void {
        switch (v) {
            .string => |s| self.markHeader(&s.header),
            .object => |o| self.markHeader(&o.header),
            .array => |a| self.markHeader(&a.header),
            .function => |f| self.markHeader(&f.header),
            .native_function => |fno| self.markHeader(&fno.header),
            else => {},
        }
    }

    fn markHeader(self: *Runtime, header: *GcHeader) void {
        if (header.marked) return;
        header.marked = true;

        switch (header.kind) {
            .string => {},
            .object => {
                const obj: *Object = @fieldParentPtr("header", header);
                if (obj.prototype) |p| self.markHeader(&p.header);
                var it = obj.props.iterator();
                while (it.next()) |entry| self.markValue(entry.value_ptr.*);
            },
            .array => {
                const arr: *Array = @fieldParentPtr("header", header);
                if (arr.prototype) |p| self.markHeader(&p.header);
                var it = arr.props.iterator();
                while (it.next()) |entry| self.markValue(entry.value_ptr.*);
                for (arr.items.items) |item| self.markValue(item);
            },
            .function => {
                const f: *Function = @fieldParentPtr("header", header);
                self.markEnvironment(f.closure_env);
                if (f.is_arrow) self.markValue(f.arrow_this);
            },
            .native_function_object => {
                const fno: *NativeFunctionObject = @fieldParentPtr("header", header);
                if (fno.prototype) |p| self.markHeader(&p.header);
                var it = fno.props.iterator();
                while (it.next()) |entry| self.markValue(entry.value_ptr.*);
            },
        }
    }

    fn sweep(self: *Runtime) void {
        var prev: ?*GcHeader = null;
        var cur_opt = self.gc_head;
        while (cur_opt) |cur| {
            const next = cur.next;
            if (!cur.marked) {
                self.sweepOne(cur);
                if (prev) |p| {
                    p.next = next;
                } else {
                    self.gc_head = next;
                }
            } else {
                cur.marked = false;
                prev = cur;
            }
            cur_opt = next;
        }
    }

    fn sweepOne(self: *Runtime, header: *GcHeader) void {
        switch (header.kind) {
            .string => {
                const s: *GcString = @fieldParentPtr("header", header);
                self.gc_bytes -|= @sizeOf(GcString) + s.bytes.len;
                self.gpa.free(s.bytes);
                self.gpa.destroy(s);
            },
            .object => {
                const obj: *Object = @fieldParentPtr("header", header);
                var it = obj.props.iterator();
                while (it.next()) |entry| self.gpa.free(@constCast(entry.key_ptr.*));
                obj.props.deinit(self.gpa);
                self.gc_bytes -|= @sizeOf(Object);
                self.gpa.destroy(obj);
            },
            .array => {
                const arr: *Array = @fieldParentPtr("header", header);
                var it = arr.props.iterator();
                while (it.next()) |entry| self.gpa.free(@constCast(entry.key_ptr.*));
                arr.props.deinit(self.gpa);
                arr.items.deinit(self.gpa);
                self.gc_bytes -|= @sizeOf(Array);
                self.gpa.destroy(arr);
            },
            .function => {
                const f: *Function = @fieldParentPtr("header", header);
                self.gc_bytes -|= @sizeOf(Function);
                self.gpa.destroy(f);
            },
            .native_function_object => {
                const fno: *NativeFunctionObject = @fieldParentPtr("header", header);
                var it = fno.props.iterator();
                while (it.next()) |entry| self.gpa.free(@constCast(entry.key_ptr.*));
                fno.props.deinit(self.gpa);
                self.gc_bytes -|= @sizeOf(NativeFunctionObject);
                self.gpa.destroy(fno);
            },
        }
    }

    fn gcFreeAll(self: *Runtime) void {
        var cur_opt = self.gc_head;
        while (cur_opt) |cur| {
            const next = cur.next;
            self.sweepOne(cur);
            cur_opt = next;
        }
        self.gc_head = null;
        self.gc_bytes = 0;
    }

    pub fn putProp(self: *Runtime, obj: *Object, key: []const u8, value: Value) !void {
        if (obj.props.getPtr(key)) |existing| {
            existing.* = value;
            return;
        }
        const owned = try self.gpa.dupe(u8, key);
        errdefer self.gpa.free(owned);
        try obj.props.put(self.gpa, owned, value);
    }

    pub fn getProp(_: *Runtime, obj: *Object, key: []const u8) ?Value {
        return obj.props.get(key);
    }

    pub fn getPropChain(self: *Runtime, obj: *Object, key: []const u8) ?Value {
        var cur: ?*Object = obj;
        while (cur) |o| : (cur = o.prototype) {
            if (self.getProp(o, key)) |v| return v;
        }
        return null;
    }

    fn dupe(self: *Runtime, s: []const u8) ![]const u8 {
        return self.arena.allocator().dupe(u8, s);
    }

    fn readFileAlloc(self: *Runtime, path: []const u8) ![]const u8 {
        var file = try std.fs.cwd().openFile(path, .{});
        defer file.close();
        const max_bytes: usize = 16 * 1024 * 1024;
        const tmp = try file.readToEndAlloc(self.gpa, max_bytes);
        defer self.gpa.free(tmp);
        return self.dupe(tmp);
    }

    fn importModule(self: *Runtime, importer: []const u8, spec: []const u8) !Value {
        if (std.mem.eql(u8, spec, "fs")) return self.builtin_fs;
        if (std.mem.eql(u8, spec, "console")) return self.builtin_console;

        const resolved = try self.resolveModulePath(importer, spec);
        if (self.modules.get(resolved)) |ns| return ns;

        const ns_obj = try self.newObject();
        const ns: Value = .{ .object = ns_obj };
        try self.modules.put(self.gpa, resolved, ns);

        const src_raw = try self.readFileAlloc(resolved);
        const src = if (std.mem.endsWith(u8, resolved, ".ts")) try stripTypescript(self, src_raw) else src_raw;

        self.last_parse_diagnostic = null;
        self.last_runtime_diagnostic = null;
        const prev_name = self.current_source_name;
        const prev_src = self.current_source;
        self.current_source_name = resolved;
        self.current_source = src;
        defer {
            self.current_source_name = prev_name;
            self.current_source = prev_src;
        }
        var parser = parser_mod.Parser.init(self.arena.allocator(), src, resolved);
        const program = parser.parseProgram() catch |err| {
            if (err == error.ParseError) self.last_parse_diagnostic = parser.last_diagnostic;
            return err;
        };
        try self.evalProgram(program, resolved);

        return ns;
    }

    fn resolveModulePath(self: *Runtime, importer: []const u8, spec: []const u8) ![]const u8 {
        const importer_dir = std.fs.path.dirname(importer) orelse ".";
        const candidate = if (std.fs.path.isAbsolute(spec)) blk: {
            break :blk try self.dupe(spec);
        } else blk: {
            const tmp = try std.fs.path.resolve(self.gpa, &.{ importer_dir, spec });
            defer self.gpa.free(tmp);
            break :blk try self.dupe(tmp);
        };

        if (hasSupportedExt(candidate)) return candidate;

        // Extensionless import: try `*.js` then `*.ts`.
        const js_tmp = try std.mem.concat(self.gpa, u8, &.{ candidate, ".js" });
        defer self.gpa.free(js_tmp);
        const js = try self.dupe(js_tmp);
        if (fileExists(js)) return js;
        const ts_tmp = try std.mem.concat(self.gpa, u8, &.{ candidate, ".ts" });
        defer self.gpa.free(ts_tmp);
        const ts = try self.dupe(ts_tmp);
        if (fileExists(ts)) return ts;

        return error.ModuleNotFound;
    }

    fn hasSupportedExt(path: []const u8) bool {
        return std.mem.endsWith(u8, path, ".js") or std.mem.endsWith(u8, path, ".ts");
    }

    fn fileExists(path: []const u8) bool {
        std.fs.cwd().access(path, .{}) catch return false;
        return true;
    }

    pub fn writeValue(self: *Runtime, v: Value) !void {
        return writeValueTo(self.out, v);
    }

    pub fn writeValueTo(w: anytype, v: Value) !void {
        switch (v) {
            .undefined => try w.writeAll("undefined"),
            .null => try w.writeAll("null"),
            .boolean => |b| try w.writeAll(if (b) "true" else "false"),
            .number => |n| try w.print("{d}", .{n}),
            .string => |s| try w.writeAll(s.bytes),
            .native_fn => try w.writeAll("[Function]"),
            .function => try w.writeAll("[Function]"),
            .native_function => try w.writeAll("[Function]"),
            .object => try w.writeAll("[Object]"),
            .array => |arr| {
                try w.writeByte('[');
                for (arr.items.items, 0..) |item, i| {
                    if (i != 0) try w.writeAll(", ");
                    try writeValueTo(w, item);
                }
                try w.writeByte(']');
            },
        }
    }

    fn evalProgram(self: *Runtime, program: ast.Program, source_name: []const u8) anyerror!void {
        for (program.statements) |stmt| {
            const ctrl = try self.evalStmt(stmt, source_name);
            if (ctrl == .ret) return error.ReturnOutsideFunction;
            if (ctrl == .brk) return error.BreakOutsideLoop;
            if (ctrl == .cont) return error.ContinueOutsideLoop;
            self.maybeCollect();
        }
    }

    pub fn callValueDirect(self: *Runtime, callee: Value, this_value: Value, args: []const Value) anyerror!Value {
        const effective_this: Value = if (this_value == .undefined) self.global_this else this_value;
        const roots_base = self.gc_temp_roots.items.len;
        defer self.popTempRoots(roots_base);
        try self.pushTempRoot(callee);
        try self.pushTempRoot(effective_this);
        for (args) |a| try self.pushTempRoot(a);

        return switch (callee) {
            .native_fn => |nf| nf.func(self, effective_this, args),
            .native_function => |fno| fno.func.func(self, effective_this, args),
            .function => |f| self.callFunction(f, effective_this, args),
            else => error.RuntimeError,
        };
    }

    fn compileAndExec(self: *Runtime, program: ast.Program, source_name: []const u8) anyerror!void {
        const chunk = try self.compileProgram(program);
        self.vm_stack.shrinkRetainingCapacity(0);
        _ = try self.execChunk(chunk, source_name, false);
    }

    fn compileProgram(self: *Runtime, program: ast.Program) !*Chunk {
        const a = self.arena.allocator();
        const chunk = try a.create(Chunk);
        chunk.* = .{};
        try self.vm_chunks.append(self.gpa, chunk);

        var c = Compiler.init(self, a, chunk);
        defer c.deinit();
        try c.compileProgram(program);
        return chunk;
    }

    fn execChunk(self: *Runtime, chunk: *Chunk, source_name: []const u8, is_function: bool) anyerror!Value {
        const base_len = self.vm_stack.items.len;

        var scope_depth: u32 = 0;
        var loop_frames: std.ArrayListUnmanaged(LoopFrame) = .{};
        defer loop_frames.deinit(self.gpa);

        var ip: usize = 0;
        var span_cursor: usize = 0;
        var current_span: ast.Span = .{ .start = 0, .end = 0, .line = 1, .col = 1 };

        while (ip < chunk.code.items.len) {
            if (span_cursor < chunk.span_offsets.items.len and chunk.span_offsets.items[span_cursor] == ip) {
                current_span = chunk.spans.items[span_cursor];
                span_cursor += 1;
            }

            const op: Op = @enumFromInt(chunk.code.items[ip]);
            ip += 1;
            switch (op) {
                .const_ => {
                    const idx = readU32(chunk.code.items, &ip);
                    switch (chunk.consts.items[idx]) {
                        .value => |v| try self.vm_stack.append(self.gpa, v),
                        else => return error.RuntimeError,
                    }
                },
                .pop => {
                    _ = self.vm_stack.pop();
                },
                .dup => {
                    try self.vm_stack.append(self.gpa, self.vm_stack.items[self.vm_stack.items.len - 1]);
                },
                .swap => {
                    const a = self.vm_stack.pop().?;
                    const b = self.vm_stack.pop().?;
                    try self.vm_stack.append(self.gpa, a);
                    try self.vm_stack.append(self.gpa, b);
                },
                .this_ => try self.vm_stack.append(self.gpa, self.current_this),
                .push_scope => {
                    try self.pushScope();
                    scope_depth += 1;
                },
                .pop_scope => {
                    if (scope_depth != 0) scope_depth -= 1;
                    self.popScope();
                },
                .get_var => {
                    const name_idx = readU32(chunk.code.items, &ip);
                    const name = chunk.constName(name_idx);
                    const v = self.get(name) orelse {
                        self.noteRuntimeErrorFmt(current_span, "ReferenceError: {s} is not defined.", .{name});
                        return error.RuntimeError;
                    };
                    try self.vm_stack.append(self.gpa, v);
                },
                .define_var => {
                    const name_idx = readU32(chunk.code.items, &ip);
                    const mut_flag = chunk.code.items[ip];
                    ip += 1;
                    const name = chunk.constName(name_idx);
                    const value = self.vm_stack.pop().?;
                    try self.define(name, value, mut_flag != 0);
                    try self.vm_stack.append(self.gpa, value);
                },
                .set_var => {
                    const name_idx = readU32(chunk.code.items, &ip);
                    const name = chunk.constName(name_idx);
                    const value = self.vm_stack.items[self.vm_stack.items.len - 1];
                    try self.assign(name, value, current_span);
                },
                .new_object => {
                    const o = try self.newObject();
                    try self.vm_stack.append(self.gpa, .{ .object = o });
                },
                .get_prop_name => {
                    const name_idx = readU32(chunk.code.items, &ip);
                    const name = chunk.constName(name_idx);
                    const objv = self.vm_stack.pop().?;
                    const out = switch (objv) {
                        .string => |s| blk: {
                            if (std.mem.eql(u8, name, "length")) break :blk Value{ .number = @as(f64, @floatFromInt(s.bytes.len)) };
                            break :blk Value.undefined;
                        },
                        .object => |o| self.getPropChain(o, name) orelse .undefined,
                        .array => |a| blk: {
                            if (a.props.get(name)) |v| break :blk v;
                            if (a.prototype) |p| break :blk self.getPropChain(p, name) orelse .undefined;
                            break :blk .undefined;
                        },
                        .function, .native_fn => blk: {
                            if (self.function_prototype) |p| break :blk self.getPropChain(p, name) orelse .undefined;
                            break :blk .undefined;
                        },
                        .native_function => |fno| blk: {
                            if (fno.props.get(name)) |v| break :blk v;
                            if (fno.prototype) |p| break :blk self.getPropChain(p, name) orelse .undefined;
                            break :blk .undefined;
                        },
                        else => .undefined,
                    };
                    try self.vm_stack.append(self.gpa, out);
                },
                .set_prop_name => {
                    const name_idx = readU32(chunk.code.items, &ip);
                    const name = chunk.constName(name_idx);
                    const value = self.vm_stack.pop().?;
                    const objv = self.vm_stack.pop().?;
                    switch (objv) {
                        .object => |o| try self.putProp(o, name, value),
                        .array => |a| {
                            if (a.props.getPtr(name)) |p| {
                                p.* = value;
                            } else {
                                const owned = try self.gpa.dupe(u8, name);
                                errdefer self.gpa.free(owned);
                                try a.props.put(self.gpa, owned, value);
                            }
                        },
                        .native_function => |fno| {
                            if (fno.props.getPtr(name)) |p| {
                                p.* = value;
                            } else {
                                const owned = try self.gpa.dupe(u8, name);
                                errdefer self.gpa.free(owned);
                                try fno.props.put(self.gpa, owned, value);
                            }
                        },
                        else => return error.RuntimeError,
                    }
                    try self.vm_stack.append(self.gpa, value);
                },
                .new_array => {
                    const a = try self.newArray();
                    try self.vm_stack.append(self.gpa, .{ .array = a });
                },
                .array_push => {
                    const value = self.vm_stack.pop().?;
                    const arrv = self.vm_stack.pop().?;
                    const arr = switch (arrv) {
                        .array => |a| a,
                        else => return error.RuntimeError,
                    };
                    try arr.items.append(self.gpa, value);
                    try self.vm_stack.append(self.gpa, .{ .array = arr });
                },
                .array_len => {
                    const arrv = self.vm_stack.pop().?;
                    const arr = switch (arrv) {
                        .array => |a| a,
                        else => return error.RuntimeError,
                    };
                    try self.vm_stack.append(self.gpa, .{ .number = @floatFromInt(arr.items.items.len) });
                },
                .get_index => {
                    const key = self.vm_stack.pop().?;
                    const obj = self.vm_stack.pop().?;
                    const out = try self.getIndex(obj, key);
                    try self.vm_stack.append(self.gpa, out);
                },
                .set_index => {
                    const value = self.vm_stack.pop().?;
                    const key = self.vm_stack.pop().?;
                    const obj = self.vm_stack.pop().?;
                    try self.setIndex(obj, key, value);
                    try self.vm_stack.append(self.gpa, value);
                },
                .neg => {
                    const v = self.vm_stack.pop().?;
                    const out = switch (v) {
                        .number => |n| Value{ .number = -n },
                        else => return error.RuntimeError,
                    };
                    try self.vm_stack.append(self.gpa, out);
                },
                .not => {
                    const v = self.vm_stack.pop().?;
                    try self.vm_stack.append(self.gpa, .{ .boolean = !isTruthy(v) });
                },
                .add => try execBinary(self, &current_span, .add),
                .sub => try execBinary(self, &current_span, .sub),
                .mul => try execBinary(self, &current_span, .mul),
                .div => try execBinary(self, &current_span, .div),
                .mod => try execBinary(self, &current_span, .mod),
                .eq => try execBinary(self, &current_span, .eq),
                .neq => try execBinary(self, &current_span, .neq),
                .strict_eq => try execBinary(self, &current_span, .strict_eq),
                .strict_neq => try execBinary(self, &current_span, .strict_neq),
                .lt => try execBinary(self, &current_span, .lt),
                .lte => try execBinary(self, &current_span, .lte),
                .gt => try execBinary(self, &current_span, .gt),
                .gte => try execBinary(self, &current_span, .gte),
                .loop_enter => {
                    const break_ip = readU32(chunk.code.items, &ip);
                    const continue_ip = readU32(chunk.code.items, &ip);
                    const target_depth = readU32(chunk.code.items, &ip);
                    try loop_frames.append(self.gpa, .{
                        .break_ip = @intCast(break_ip),
                        .continue_ip = @intCast(continue_ip),
                        .scope_depth = target_depth,
                    });
                },
                .loop_exit => {
                    _ = loop_frames.pop();
                },
                .break_ => {
                    if (loop_frames.items.len == 0) return error.BreakOutsideLoop;
                    const top = loop_frames.items[loop_frames.items.len - 1];
                    while (scope_depth > top.scope_depth) {
                        scope_depth -= 1;
                        self.popScope();
                    }
                    ip = top.break_ip;
                },
                .continue_ => {
                    if (loop_frames.items.len == 0) return error.ContinueOutsideLoop;
                    const top = loop_frames.items[loop_frames.items.len - 1];
                    while (scope_depth > top.scope_depth) {
                        scope_depth -= 1;
                        self.popScope();
                    }
                    ip = top.continue_ip;
                },
                .jump => {
                    const off = readI32(chunk.code.items, &ip);
                    ip = @intCast(@as(isize, @intCast(ip)) + off);
                },
                .jump_if_false => {
                    const off = readI32(chunk.code.items, &ip);
                    const v = self.vm_stack.items[self.vm_stack.items.len - 1];
                    if (!isTruthy(v)) ip = @intCast(@as(isize, @intCast(ip)) + off);
                },
                .jump_if_true => {
                    const off = readI32(chunk.code.items, &ip);
                    const v = self.vm_stack.items[self.vm_stack.items.len - 1];
                    if (isTruthy(v)) ip = @intCast(@as(isize, @intCast(ip)) + off);
                },
                .jump_if_nullish => {
                    const off = readI32(chunk.code.items, &ip);
                    const v = self.vm_stack.items[self.vm_stack.items.len - 1];
                    if (v == .null or v == .undefined) ip = @intCast(@as(isize, @intCast(ip)) + off);
                },
                .make_function => {
                    const proto_idx = readU32(chunk.code.items, &ip);
                    const proto = chunk.constProto(proto_idx);
                    const f = try self.newFunction(
                        proto.name,
                        proto.params,
                        .{ .bc = proto.chunk },
                        self.current_env,
                        proto.is_arrow,
                        if (proto.is_arrow) self.current_this else .undefined,
                        proto.source_name,
                        proto.source,
                        proto.span,
                    );
                    try self.vm_stack.append(self.gpa, .{ .function = f });
                },
                .call => {
                    const argc_u32 = readU32(chunk.code.items, &ip);
                    const argc: usize = @intCast(argc_u32);
                    var args: std.ArrayList(Value) = .empty;
                    defer args.deinit(self.gpa);
                    try args.ensureTotalCapacity(self.gpa, argc);
                    var i: usize = 0;
                    while (i < argc) : (i += 1) {
                        args.appendAssumeCapacity(self.vm_stack.pop().?);
                    }
                    std.mem.reverse(Value, args.items);
                    const this_value = self.vm_stack.pop().?;
                    const callee = self.vm_stack.pop().?;
                    if (callee != .function and callee != .native_fn and callee != .native_function) {
                        self.noteRuntimeErrorFmt(current_span, "TypeError: value is not a function.", .{});
                        return error.RuntimeError;
                    }
                    const res = try self.callValueDirect(callee, this_value, args.items);
                    try self.vm_stack.append(self.gpa, res);
                },
                .import_ => {
                    const spec_idx = readU32(chunk.code.items, &ip);
                    const bind_idx = readU32(chunk.code.items, &ip);
                    const spec = chunk.constName(spec_idx);
                    const ns = try self.importModule(source_name, spec);
                    if (bind_idx != std.math.maxInt(u32)) {
                        const bind = chunk.constName(bind_idx);
                        try self.define(bind, ns, true);
                    }
                    try self.vm_stack.append(self.gpa, ns);
                },
                .return_ => {
                    const v = self.vm_stack.pop().?;
                    if (!is_function) return error.ReturnOutsideFunction;
                    self.vm_stack.shrinkRetainingCapacity(base_len);
                    return v;
                },
                .safepoint => self.maybeCollect(),
            }
        }
        self.vm_stack.shrinkRetainingCapacity(base_len);
        return .undefined;
    }

    const Control = union(enum) { normal, ret: Value, brk, cont };

    fn evalStmt(self: *Runtime, stmt: ast.Stmt, source_name: []const u8) anyerror!Control {
        switch (stmt) {
            .import_stmt => |s| {
                const ns = try self.importModule(source_name, s.spec);
                if (s.bind_name) |name| try self.define(name, ns, true);
                return .normal;
            },
            .var_decl => |s| {
                const init_val = if (s.init) |e| try self.evalExpr(e) else .undefined;
                try self.define(s.name, init_val, s.kind != .@"const");
                return .normal;
            },
            .function_decl => |s| {
                const f = try self.newFunction(
                    s.name,
                    s.params,
                    .{ .ast = s.body },
                    self.current_env,
                    false,
                    .undefined,
                    self.current_source_name,
                    self.current_source,
                    s.span,
                );
                try self.define(s.name, .{ .function = f }, true);
                return .normal;
            },
            .return_stmt => |s| {
                const v = if (s.value) |e| try self.evalExpr(e) else .undefined;
                return .{ .ret = v };
            },
            .if_stmt => |s| {
                const cond = try self.evalExpr(s.condition);
                if (isTruthy(cond)) {
                    return try self.evalStmt(s.then_branch.*, source_name);
                }
                if (s.else_branch) |e| {
                    return try self.evalStmt(e.*, source_name);
                }
                return .normal;
            },
            .for_stmt => |s| {
                try self.pushScope();
                defer self.popScope();

                if (s.init) |clause| try self.evalForClause(clause);

                self.loop_depth += 1;
                defer self.loop_depth -= 1;

                while (true) {
                    if (s.condition) |cond_expr| {
                        if (!isTruthy(try self.evalExpr(cond_expr))) break;
                    }

                    const ctrl = try self.evalStmt(s.body.*, source_name);
                    switch (ctrl) {
                        .normal => {},
                        .ret => return ctrl,
                        .brk => break,
                        .cont => {
                            if (s.post) |p| try self.evalForClause(p);
                            self.maybeCollect();
                            continue;
                        },
                    }

                    if (s.post) |p| try self.evalForClause(p);
                    self.maybeCollect();
                }

                return .normal;
            },
            .for_of_stmt => |s| {
                const iterable_val = try self.evalExpr(s.iterable);
                const arr = switch (iterable_val) {
                    .array => |a| a,
                    else => return error.RuntimeError,
                };

                self.loop_depth += 1;
                defer self.loop_depth -= 1;

                for (arr.items.items) |item| {
                    try self.pushScope();
                    defer self.popScope();

                    try self.define(s.name, item, s.kind != .@"const");

                    const ctrl = try self.evalStmt(s.body.*, source_name);
                    switch (ctrl) {
                        .normal => {},
                        .cont => {
                            self.maybeCollect();
                            continue;
                        },
                        .brk => break,
                        .ret => return ctrl,
                    }
                    self.maybeCollect();
                }
                return .normal;
            },
            .while_stmt => |s| {
                self.loop_depth += 1;
                defer self.loop_depth -= 1;
                while (isTruthy(try self.evalExpr(s.condition))) {
                    const ctrl = try self.evalStmt(s.body.*, source_name);
                    switch (ctrl) {
                        .normal => {},
                        .cont => {
                            self.maybeCollect();
                            continue;
                        },
                        .brk => break,
                        .ret => return ctrl,
                    }
                    self.maybeCollect();
                }
                return .normal;
            },
            .break_stmt => {
                if (self.loop_depth == 0) return error.BreakOutsideLoop;
                return .brk;
            },
            .continue_stmt => {
                if (self.loop_depth == 0) return error.ContinueOutsideLoop;
                return .cont;
            },
            .block => |b| {
                try self.pushScope();
                defer self.popScope();
                for (b.statements) |inner| {
                    const ctrl = try self.evalStmt(inner, source_name);
                    if (ctrl != .normal) return ctrl;
                }
                return .normal;
            },
            .assign => |s| {
                const rhs = try self.evalExpr(s.value);
                const out = switch (s.op) {
                    .assign => rhs,
                    .add_assign => blk: {
                        const cur = self.get(s.name) orelse {
                            self.noteRuntimeErrorFmt(s.name_span, "ReferenceError: {s} is not defined.", .{s.name});
                            return error.RuntimeError;
                        };
                        break :blk try evalBinary(self, .add, cur, rhs);
                    },
                    .sub_assign => try self.evalCompoundNumeric(s.name, s.name_span, rhs, .sub),
                    .mul_assign => try self.evalCompoundNumeric(s.name, s.name_span, rhs, .mul),
                    .div_assign => try self.evalCompoundNumeric(s.name, s.name_span, rhs, .div),
                    .mod_assign => try self.evalCompoundNumeric(s.name, s.name_span, rhs, .mod),
                };
                try self.assign(s.name, out, s.name_span);
                return .normal;
            },
            .expr_stmt => |s| {
                _ = try self.evalExpr(s.expr);
                return .normal;
            },
        }
    }

    fn evalForClause(self: *Runtime, clause: ast.ForClause) anyerror!void {
        switch (clause) {
            .var_decl => |v| {
                const init_val = if (v.init) |e| try self.evalExpr(e) else .undefined;
                try self.define(v.name, init_val, v.kind != .@"const");
            },
            .assign => |a| {
                const rhs = try self.evalExpr(a.value);
                const out = switch (a.op) {
                    .assign => rhs,
                    .add_assign => blk: {
                        const cur = self.get(a.name) orelse {
                            self.noteRuntimeErrorFmt(a.name_span, "ReferenceError: {s} is not defined.", .{a.name});
                            return error.RuntimeError;
                        };
                        break :blk try evalBinary(self, .add, cur, rhs);
                    },
                    .sub_assign => try self.evalCompoundNumeric(a.name, a.name_span, rhs, .sub),
                    .mul_assign => try self.evalCompoundNumeric(a.name, a.name_span, rhs, .mul),
                    .div_assign => try self.evalCompoundNumeric(a.name, a.name_span, rhs, .div),
                    .mod_assign => try self.evalCompoundNumeric(a.name, a.name_span, rhs, .mod),
                };
                try self.assign(a.name, out, a.name_span);
            },
            .expr => |e| {
                _ = try self.evalExpr(e);
            },
        }
    }

    pub fn noteRuntimeErrorFmt(self: *Runtime, span: ast.Span, comptime fmt: []const u8, args: anytype) void {
        if (self.last_runtime_diagnostic != null) return;
        const tmp = std.fmt.allocPrint(self.gpa, fmt, args) catch {
            self.last_runtime_diagnostic = .{
                .source_name = self.current_source_name,
                .source = self.current_source,
                .span = span,
                .message = "RuntimeError",
            };
            return;
        };
        defer self.gpa.free(tmp);
        const msg = self.dupe(tmp) catch "RuntimeError";
        self.last_runtime_diagnostic = .{
            .source_name = self.current_source_name,
            .source = self.current_source,
            .span = span,
            .message = msg,
        };
    }

    fn evalCompoundNumeric(self: *Runtime, name: []const u8, span: ast.Span, rhs: Value, op: ast.BinaryOp) anyerror!Value {
        const cur = self.get(name) orelse {
            self.noteRuntimeErrorFmt(span, "ReferenceError: {s} is not defined.", .{name});
            return error.RuntimeError;
        };
        return evalBinary(self, op, cur, rhs) catch |err| {
            // Ensure we keep any previously set diagnostic; otherwise set a generic one.
            if (err == error.RuntimeError and self.last_runtime_diagnostic == null) {
                self.noteRuntimeErrorFmt(span, "TypeError: invalid operands for {s}=", .{@tagName(op)});
            }
            return err;
        };
    }

    fn evalExpr(self: *Runtime, expr: *const ast.Expr) anyerror!Value {
        return switch (expr.*) {
            .number => |n| .{ .number = n },
            .string => |s| .{ .string = try self.newString(s) },
            .ident => |id| self.get(id.name) orelse {
                self.noteRuntimeErrorFmt(id.span, "ReferenceError: {s} is not defined.", .{id.name});
                return error.RuntimeError;
            },
            .this => self.current_this,
            .boolean => |b| .{ .boolean = b },
            .null => .null,
            .undefined => .undefined,
            .object_literal => |o| {
                const obj = try self.newObject();
                for (o.props) |p| {
                    try self.putProp(obj, p.key, try self.evalExpr(p.value));
                }
                return .{ .object = obj };
            },
            .array_literal => |a| {
                const arr = try self.newArray();
                for (a.items) |e| try arr.items.append(self.gpa, try self.evalExpr(e));
                return .{ .array = arr };
            },
            .update => |u| {
                const cur = self.get(u.target.name) orelse {
                    self.noteRuntimeErrorFmt(u.target.span, "ReferenceError: {s} is not defined.", .{u.target.name});
                    return error.RuntimeError;
                };
                const n = switch (cur) {
                    .number => |x| x,
                    else => return error.RuntimeError,
                };
                const next: f64 = switch (u.op) {
                    .inc => n + 1,
                    .dec => n - 1,
                };
                const next_val: Value = .{ .number = next };
                try self.assign(u.target.name, next_val, u.target.span);
                return if (u.prefix) next_val else .{ .number = n };
            },
            .unary => |u| {
                const v = try self.evalExpr(u.expr);
                return switch (u.op) {
                    .neg => switch (v) {
                        .number => |n| .{ .number = -n },
                        else => return error.RuntimeError,
                    },
                    .not => .{ .boolean = !isTruthy(v) },
                };
            },
            .binary => |b| {
                const left = try self.evalExpr(b.left);
                const right = try self.evalExpr(b.right);
                return evalBinary(self, b.op, left, right);
            },
            .logical => |l| {
                const left = try self.evalExpr(l.left);
                switch (l.op) {
                    .@"and" => {
                        if (!isTruthy(left)) return left;
                        return self.evalExpr(l.right);
                    },
                    .@"or" => {
                        if (isTruthy(left)) return left;
                        return self.evalExpr(l.right);
                    },
                    .nullish => {
                        if (left == .null or left == .undefined) return self.evalExpr(l.right);
                        return left;
                    },
                }
            },
            .index => |idx| {
                const obj = try self.evalExpr(idx.object);
                const key = try self.evalExpr(idx.index);
                return self.getIndex(obj, key);
            },
            .assign_expr => |a| {
                const rhs = try self.evalExpr(a.value);
                const out = switch (a.op) {
                    .assign => rhs,
                    .add_assign => blk: {
                        const cur = try self.getLValue(a.target);
                        break :blk try evalBinary(self, .add, cur, rhs);
                    },
                    .sub_assign => blk: {
                        const cur = try self.getLValue(a.target);
                        break :blk try evalBinary(self, .sub, cur, rhs);
                    },
                    .mul_assign => blk: {
                        const cur = try self.getLValue(a.target);
                        break :blk try evalBinary(self, .mul, cur, rhs);
                    },
                    .div_assign => blk: {
                        const cur = try self.getLValue(a.target);
                        break :blk try evalBinary(self, .div, cur, rhs);
                    },
                    .mod_assign => blk: {
                        const cur = try self.getLValue(a.target);
                        break :blk try evalBinary(self, .mod, cur, rhs);
                    },
                };
                try self.setLValue(a.target, out);
                return out;
            },
            .function_expr => |f| {
                const fname = f.name orelse "";
                const fun = try self.newFunction(
                    fname,
                    f.params,
                    .{ .ast = f.body },
                    self.current_env,
                    f.is_arrow,
                    if (f.is_arrow) self.current_this else .undefined,
                    self.current_source_name,
                    self.current_source,
                    f.span,
                );
                return .{ .function = fun };
            },
            .member => |m| {
                const obj = try self.evalExpr(m.object);
                return switch (obj) {
                    .string => |s| {
                        if (std.mem.eql(u8, m.property, "length")) return .{ .number = @as(f64, @floatFromInt(s.bytes.len)) };
                        return .undefined;
                    },
                    .object => |o| self.getPropChain(o, m.property) orelse .undefined,
                    .array => |a| {
                        if (a.props.get(m.property)) |v| return v;
                        if (a.prototype) |p| return self.getPropChain(p, m.property) orelse .undefined;
                        return .undefined;
                    },
                    .function, .native_fn => {
                        if (self.function_prototype) |p| return self.getPropChain(p, m.property) orelse .undefined;
                        return .undefined;
                    },
                    .native_function => |fno| {
                        if (fno.props.get(m.property)) |v| return v;
                        if (fno.prototype) |p| return self.getPropChain(p, m.property) orelse .undefined;
                        return .undefined;
                    },
                    else => return .undefined,
                };
            },
            .call => |c| {
                // Supports `obj.method(...)` by detecting a member call.
                var this_value: Value = .undefined;
                switch (c.callee.*) {
                    .member => |mem| this_value = try self.evalExpr(mem.object),
                    .index => |idx| this_value = try self.evalExpr(idx.object),
                    else => {},
                }
                const callee = try self.evalExpr(c.callee);
                if (callee != .function and callee != .native_fn and callee != .native_function) {
                    self.noteRuntimeErrorFmt(bestCallSpan(c.callee), "TypeError: value is not a function.", .{});
                    return error.RuntimeError;
                }
                return try self.callValue(callee, this_value, c.args);
            },
        };
    }

    fn callValue(self: *Runtime, callee: Value, this_value: Value, arg_exprs: []const *ast.Expr) anyerror!Value {
        var args: std.ArrayList(Value) = .empty;
        defer args.deinit(self.gpa);
        for (arg_exprs) |e| try args.append(self.gpa, try self.evalExpr(e));

        const effective_this: Value = if (this_value == .undefined) self.global_this else this_value;
        const roots_base = self.gc_temp_roots.items.len;
        defer self.popTempRoots(roots_base);
        // Keep call operands alive across GC safe-points during the call body.
        try self.pushTempRoot(callee);
        try self.pushTempRoot(effective_this);
        for (args.items) |a| try self.pushTempRoot(a);
        return switch (callee) {
            .native_fn => |nf| nf.func(self, effective_this, args.items),
            .native_function => |fno| fno.func.func(self, effective_this, args.items),
            .function => |f| self.callFunction(f, effective_this, args.items),
            else => error.RuntimeError,
        };
    }

    fn callFunction(self: *Runtime, f: *Function, this_value: Value, args: []const Value) anyerror!Value {
        const effective_this: Value = if (f.is_arrow) f.arrow_this else this_value;
        const saved_env = self.current_env;
        const saved_this = self.current_this;
        defer {
            self.current_env = saved_env;
            self.current_this = saved_this;
        }
        const roots_base = self.gc_call_roots.items.len;
        try self.gc_call_roots.append(self.gpa, .{ .env = saved_env, .this_value = saved_this });
        defer self.gc_call_roots.shrinkRetainingCapacity(roots_base);

        const call_env = try self.arena.allocator().create(Environment);
        call_env.* = .{ .parent = f.closure_env };
        self.current_env = call_env;
        self.current_this = effective_this;
        try self.envs.append(self.gpa, call_env);

        // Named function expression: bind its name inside its own scope for recursion.
        if (f.name.len != 0) {
            try self.define(f.name, .{ .function = f }, false);
        }

        const argc = args.len;
        const paramc = f.params.len;
        const n = if (argc < paramc) argc else paramc;
        for (f.params[0..n], 0..) |p, i| try self.define(p, args[i], true);
        if (argc < paramc) for (f.params[argc..]) |p| try self.define(p, .undefined, true);

        const ctrl = switch (f.body) {
            .ast => |b| try self.evalStmt(.{ .block = b }, "<function>"),
            .bc => |chunk| blk: {
                const v = try self.execChunk(chunk, "<function>", true);
                break :blk Control{ .ret = v };
            },
        };
        return switch (ctrl) {
            .normal => .undefined,
            .ret => |v| v,
            .brk => return error.BreakOutsideLoop,
            .cont => return error.ContinueOutsideLoop,
        };
    }

    fn getLValue(self: *Runtime, target: *const ast.Expr) anyerror!Value {
        return switch (target.*) {
            .ident => |id| self.get(id.name) orelse {
                self.noteRuntimeErrorFmt(id.span, "ReferenceError: {s} is not defined.", .{id.name});
                return error.RuntimeError;
            },
            .member => |m| {
                const obj = try self.evalExpr(m.object);
                return switch (obj) {
                    .object => |o| self.getPropChain(o, m.property) orelse .undefined,
                    .array => |a| {
                        if (a.props.get(m.property)) |v| return v;
                        if (a.prototype) |p| return self.getPropChain(p, m.property) orelse .undefined;
                        return .undefined;
                    },
                    else => return error.RuntimeError,
                };
            },
            .index => |idx| {
                const obj = try self.evalExpr(idx.object);
                const key = try self.evalExpr(idx.index);
                return self.getIndex(obj, key);
            },
            else => return error.RuntimeError,
        };
    }

    fn setLValue(self: *Runtime, target: *const ast.Expr, value: Value) anyerror!void {
        switch (target.*) {
            .ident => |id| return self.assign(id.name, value, id.span),
            .member => |m| {
                const obj = try self.evalExpr(m.object);
                return switch (obj) {
                    .object => |o| self.putProp(o, m.property, value),
                    .array => |a| {
                        if (a.props.getPtr(m.property)) |p| {
                            p.* = value;
                            return;
                        }
                        const owned = try self.gpa.dupe(u8, m.property);
                        errdefer self.gpa.free(owned);
                        try a.props.put(self.gpa, owned, value);
                        return;
                    },
                    else => error.RuntimeError,
                };
            },
            .index => |idx| {
                const obj = try self.evalExpr(idx.object);
                const key = try self.evalExpr(idx.index);
                return self.setIndex(obj, key, value);
            },
            else => return error.RuntimeError,
        }
    }

    fn getIndex(self: *Runtime, obj: Value, key: Value) anyerror!Value {
        return switch (obj) {
            .array => |arr| {
                switch (key) {
                    .number => {
                        const i = try indexToUsize(key);
                        if (i >= arr.items.items.len) return .undefined;
                        return arr.items.items[i];
                    },
                    else => {
                        const prop = try toStringBytes(self, key);
                        if (arr.props.get(prop)) |v| return v;
                        if (arr.prototype) |p| return self.getPropChain(p, prop) orelse .undefined;
                        return .undefined;
                    },
                }
            },
            .object => |o| {
                const prop = try toStringBytes(self, key);
                return self.getPropChain(o, prop) orelse .undefined;
            },
                .function, .native_fn => {
                    const prop = try toStringBytes(self, key);
                    if (self.function_prototype) |p| return self.getPropChain(p, prop) orelse .undefined;
                    return .undefined;
                },
                .native_function => |fno| {
                    const prop = try toStringBytes(self, key);
                    if (fno.props.get(prop)) |v| return v;
                    if (fno.prototype) |p| return self.getPropChain(p, prop) orelse .undefined;
                    return .undefined;
                },
            else => return error.RuntimeError,
        };
    }

    fn setIndex(self: *Runtime, obj: Value, key: Value, value: Value) anyerror!void {
        switch (obj) {
            .array => |arr| {
                switch (key) {
                    .number => {
                        const i = try indexToUsize(key);
                        while (arr.items.items.len < i) try arr.items.append(self.gpa, .undefined);
                        if (arr.items.items.len == i) {
                            try arr.items.append(self.gpa, value);
                        } else {
                            arr.items.items[i] = value;
                        }
                        return;
                    },
                    else => {
                        const prop = try toStringBytes(self, key);
                        if (arr.props.getPtr(prop)) |p| {
                            p.* = value;
                            return;
                        }
                        const owned = try self.gpa.dupe(u8, prop);
                        errdefer self.gpa.free(owned);
                        try arr.props.put(self.gpa, owned, value);
                        return;
                    },
                }
            },
            .object => |o| {
                const prop = try toStringBytes(self, key);
                try self.putProp(o, prop, value);
                return;
            },
            .native_function => |fno| {
                const prop = try toStringBytes(self, key);
                if (fno.props.getPtr(prop)) |p| {
                    p.* = value;
                    return;
                }
                const owned = try self.gpa.dupe(u8, prop);
                errdefer self.gpa.free(owned);
                try fno.props.put(self.gpa, owned, value);
                return;
            },
            else => return error.RuntimeError,
        }
    }
};

fn indexToUsize(v: Value) anyerror!usize {
    return switch (v) {
        .number => |n| {
            if (n < 0) return error.RuntimeError;
            const i: usize = @intFromFloat(n);
            if (@as(f64, @floatFromInt(i)) != n) return error.RuntimeError;
            return i;
        },
        else => error.RuntimeError,
    };
}

fn evalBinary(rt: *Runtime, op: ast.BinaryOp, left: Value, right: Value) !Value {
    if (op == .add) {
        if (left == .number and right == .number) return .{ .number = left.number + right.number };
        const ls = try toStringBytes(rt, left);
        const rs = try toStringBytes(rt, right);
        const buf = try rt.gpa.alloc(u8, ls.len + rs.len);
        errdefer rt.gpa.free(buf);
        @memcpy(buf[0..ls.len], ls);
        @memcpy(buf[ls.len..], rs);
        return .{ .string = try rt.newStringOwned(buf) };
    }

    switch (op) {
        .eq => return .{ .boolean = isEqual(left, right) },
        .neq => return .{ .boolean = !isEqual(left, right) },
        .strict_eq => return .{ .boolean = isStrictEqual(left, right) },
        .strict_neq => return .{ .boolean = !isStrictEqual(left, right) },
        .lt, .lte, .gt, .gte => return .{ .boolean = try compare(op, left, right) },
        else => {},
    }

    const l = switch (left) {
        .number => |n| n,
        else => return error.RuntimeError,
    };
    const r = switch (right) {
        .number => |n| n,
        else => return error.RuntimeError,
    };
    return .{ .number = switch (op) {
        .sub => l - r,
        .mul => l * r,
        .div => l / r,
        .mod => @rem(l, r),
        .add => unreachable,
        .eq, .neq, .strict_eq, .strict_neq, .lt, .lte, .gt, .gte => unreachable,
    } };
}

const NativeFn = *const fn (rt: *Runtime, this_value: Value, args: []const Value) anyerror!Value;

const NativeFunction = struct {
    func: NativeFn,
};

const Binding = struct {
    value: Value,
    mutable: bool,
};

const Environment = struct {
    parent: ?*Environment,
    bindings: std.StringHashMapUnmanaged(Binding) = .{},
};

const CallRoot = struct {
    env: *Environment,
    this_value: Value,
};

const FunctionBody = union(enum) {
    ast: ast.Block,
    bc: *Chunk,
};

const Function = struct {
    header: GcHeader = undefined,
    name: []const u8,
    params: []const []const u8,
    body: FunctionBody,
    closure_env: *Environment,
    is_arrow: bool = false,
    arrow_this: Value = .undefined,
    source_name: []const u8,
    source: []const u8,
    source_span: ?ast.Span = null,
};

pub const RuntimeDiagnostic = struct {
    source_name: []const u8,
    source: []const u8,
    span: ast.Span,
    message: []const u8,

    pub fn render(self: *const RuntimeDiagnostic, w: anytype) !void {
        try w.print("{s}:{d}:{d}: error: {s}\n", .{ self.source_name, self.span.line, self.span.col, self.message });
        const bounds = findLineBounds(self.source, self.span.start);
        const line_text = self.source[bounds.start..bounds.end];
        try w.print("  {s}\n", .{line_text});
        try w.writeAll("  ");
        var i: u32 = 1;
        while (i < self.span.col) : (i += 1) try w.writeByte(' ');
        try w.writeAll("^\n");
    }
};

fn findLineBounds(source: []const u8, index: usize) struct { start: usize, end: usize } {
    var start = if (index < source.len) index else source.len;
    while (start > 0 and source[start - 1] != '\n') start -= 1;
    var end = if (index < source.len) index else source.len;
    while (end < source.len and source[end] != '\n') end += 1;
    return .{ .start = start, .end = end };
}

const Op = enum(u8) {
    const_,
    pop,
    dup,
    swap,
    this_,
    push_scope,
    pop_scope,
    get_var,
    define_var,
    set_var,
    new_object,
    get_prop_name,
    set_prop_name,
    new_array,
    array_push,
    array_len,
    get_index,
    set_index,
    neg,
    not,
    add,
    sub,
    mul,
    div,
    mod,
    eq,
    neq,
    strict_eq,
    strict_neq,
    lt,
    lte,
    gt,
    gte,
    loop_enter,
    loop_exit,
    break_,
    continue_,
    jump,
    jump_if_false,
    jump_if_true,
    jump_if_nullish,
    make_function,
    call,
    import_,
    return_,
    safepoint,
};

const Const = union(enum) {
    value: Value,
    name: []const u8,
    proto: *FunctionProto,
};

const FunctionProto = struct {
    name: []const u8,
    params: []const []const u8,
    chunk: *Chunk,
    is_arrow: bool,
    source_name: []const u8,
    source: []const u8,
    span: ast.Span,
};

const Chunk = struct {
    code: std.ArrayListUnmanaged(u8) = .{},
    consts: std.ArrayListUnmanaged(Const) = .{},
    span_offsets: std.ArrayListUnmanaged(u32) = .{},
    spans: std.ArrayListUnmanaged(ast.Span) = .{},

    fn recordSpan(self: *Chunk, a: std.mem.Allocator, span: ast.Span) !void {
        try self.span_offsets.append(a, @intCast(self.code.items.len));
        try self.spans.append(a, span);
    }

    fn emitOp(self: *Chunk, a: std.mem.Allocator, span: ast.Span, op: Op) !void {
        try self.recordSpan(a, span);
        try self.code.append(a, @intFromEnum(op));
    }

    fn emitU8(self: *Chunk, a: std.mem.Allocator, b: u8) !void {
        try self.code.append(a, b);
    }

    fn emitU32(self: *Chunk, a: std.mem.Allocator, v: u32) !void {
        var buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &buf, v, .little);
        try self.code.appendSlice(a, &buf);
    }

    fn emitI32(self: *Chunk, a: std.mem.Allocator, v: i32) !void {
        var buf: [4]u8 = undefined;
        std.mem.writeInt(i32, &buf, v, .little);
        try self.code.appendSlice(a, &buf);
    }

    fn constName(self: *const Chunk, idx: u32) []const u8 {
        return switch (self.consts.items[idx]) {
            .name => |s| s,
            else => "",
        };
    }

    fn constProto(self: *const Chunk, idx: u32) *FunctionProto {
        return switch (self.consts.items[idx]) {
            .proto => |p| p,
            else => unreachable,
        };
    }
};

const LoopFrame = struct {
    break_ip: usize,
    continue_ip: usize,
    scope_depth: u32,
};

fn readU32(code: []const u8, ip: *usize) u32 {
    const start = ip.*;
    ip.* += 4;
    const ptr: *const [4]u8 = @ptrCast(code[start .. start + 4].ptr);
    return std.mem.readInt(u32, ptr, .little);
}

fn readI32(code: []const u8, ip: *usize) i32 {
    const start = ip.*;
    ip.* += 4;
    const ptr: *const [4]u8 = @ptrCast(code[start .. start + 4].ptr);
    return std.mem.readInt(i32, ptr, .little);
}

fn execBinary(rt: *Runtime, span: *const ast.Span, op: ast.BinaryOp) !void {
    const right = rt.vm_stack.pop().?;
    const left = rt.vm_stack.pop().?;
    const out = evalBinary(rt, op, left, right) catch |err| {
        if (err == error.RuntimeError and rt.last_runtime_diagnostic == null) {
            rt.noteRuntimeErrorFmt(span.*, "TypeError: invalid operands for {s}.", .{@tagName(op)});
        }
        return err;
    };
    try rt.vm_stack.append(rt.gpa, out);
}

const Compiler = struct {
    rt: *Runtime,
    a: std.mem.Allocator,
    chunk: *Chunk,
    tmp_id: u32 = 0,
    scope_depth: u32 = 0,

    fn init(rt: *Runtime, a: std.mem.Allocator, chunk: *Chunk) Compiler {
        return .{ .rt = rt, .a = a, .chunk = chunk };
    }

    fn deinit(self: *Compiler) void {
        _ = self;
    }

    fn compileProgram(self: *Compiler, program: ast.Program) anyerror!void {
        for (program.statements) |stmt| {
            try self.compileStmt(stmt);
            try self.chunk.emitOp(self.a, stmtSpan(stmt), .safepoint);
        }
    }

    fn compileStmt(self: *Compiler, stmt: ast.Stmt) anyerror!void {
        switch (stmt) {
            .import_stmt => |s| {
                const spec_idx = try self.constName(s.spec);
                const bind_idx = if (s.bind_name) |n| try self.constName(n) else std.math.maxInt(u32);
                try self.chunk.emitOp(self.a, defaultSpan(), .import_);
                try self.chunk.emitU32(self.a, spec_idx);
                try self.chunk.emitU32(self.a, bind_idx);
                try self.chunk.emitOp(self.a, defaultSpan(), .pop);
            },
            .var_decl => |s| {
                if (s.init) |e| {
                    try self.compileExpr(e);
                } else {
                    try self.emitConstValue(defaultSpan(), .undefined);
                }
                const name_idx = try self.constName(s.name);
                try self.chunk.emitOp(self.a, s.name_span, .define_var);
                try self.chunk.emitU32(self.a, name_idx);
                try self.chunk.emitU8(self.a, if (s.kind != .@"const") 1 else 0);
                try self.chunk.emitOp(self.a, s.name_span, .pop);
            },
            .function_decl => |s| {
                const proto_idx = try self.constFunctionProto(s.name, s.params, s.body, s.span, false);
                try self.chunk.emitOp(self.a, defaultSpan(), .make_function);
                try self.chunk.emitU32(self.a, proto_idx);
                const name_idx = try self.constName(s.name);
                try self.chunk.emitOp(self.a, defaultSpan(), .define_var);
                try self.chunk.emitU32(self.a, name_idx);
                try self.chunk.emitU8(self.a, 1);
                try self.chunk.emitOp(self.a, defaultSpan(), .pop);
            },
            .return_stmt => |s| {
                if (s.value) |e| {
                    try self.compileExpr(e);
                } else {
                    try self.emitConstValue(defaultSpan(), .undefined);
                }
                try self.chunk.emitOp(self.a, defaultSpan(), .return_);
            },
            .block => |b| {
                try self.chunk.emitOp(self.a, defaultSpan(), .push_scope);
                self.scope_depth += 1;
                for (b.statements) |inner| {
                    try self.compileStmt(inner);
                    try self.chunk.emitOp(self.a, stmtSpan(inner), .safepoint);
                }
                try self.chunk.emitOp(self.a, defaultSpan(), .pop_scope);
                if (self.scope_depth != 0) self.scope_depth -= 1;
            },
            .if_stmt => |s| try self.compileIf(s),
            .while_stmt => |s| try self.compileWhile(s),
            .for_stmt => |s| try self.compileFor(s),
            .for_of_stmt => |s| try self.compileForOf(s),
            .break_stmt => {
                try self.chunk.emitOp(self.a, defaultSpan(), .break_);
            },
            .continue_stmt => {
                try self.chunk.emitOp(self.a, defaultSpan(), .continue_);
            },
            .assign => |s| {
                try self.compileAssignIdent(s.name, s.op, s.value, s.name_span);
                try self.chunk.emitOp(self.a, s.name_span, .pop);
            },
            .expr_stmt => |s| {
                try self.compileExpr(s.expr);
                try self.chunk.emitOp(self.a, exprSpan(s.expr), .pop);
            },
        }
    }

    fn compileIf(self: *Compiler, s: ast.IfStmt) anyerror!void {
        try self.compileExpr(s.condition);
        const jf = try self.emitJump(exprSpan(s.condition), .jump_if_false);
        try self.chunk.emitOp(self.a, exprSpan(s.condition), .pop);
        try self.compileStmt(s.then_branch.*);
        const jend = try self.emitJump(defaultSpan(), .jump);
        const else_ip: u32 = @intCast(self.chunk.code.items.len);
        self.patchJump(jf, else_ip);
        try self.chunk.emitOp(self.a, exprSpan(s.condition), .pop);
        if (s.else_branch) |e| try self.compileStmt(e.*);
        const end_ip: u32 = @intCast(self.chunk.code.items.len);
        self.patchJump(jend, end_ip);
    }

    fn compileWhile(self: *Compiler, s: ast.WhileStmt) anyerror!void {
        const enter = try self.emitLoopEnter(defaultSpan(), self.scope_depth);
        const loop_start: u32 = @intCast(self.chunk.code.items.len);

        try self.compileExpr(s.condition);
        const jf = try self.emitJump(exprSpan(s.condition), .jump_if_false);
        try self.chunk.emitOp(self.a, exprSpan(s.condition), .pop);

        try self.compileStmt(s.body.*);
        try self.chunk.emitOp(self.a, defaultSpan(), .safepoint);
        try self.emitJumpBack(loop_start);

        const cond_false_ip: u32 = @intCast(self.chunk.code.items.len);
        self.patchJump(jf, cond_false_ip);
        try self.chunk.emitOp(self.a, exprSpan(s.condition), .pop);

        const exit_ip: u32 = @intCast(self.chunk.code.items.len);
        try self.chunk.emitOp(self.a, defaultSpan(), .loop_exit);

        self.patchU32(enter.break_pos, exit_ip);
        self.patchU32(enter.continue_pos, loop_start);
    }

    fn compileFor(self: *Compiler, s: ast.ForStmt) anyerror!void {
        try self.chunk.emitOp(self.a, defaultSpan(), .push_scope);
        self.scope_depth += 1;
        defer {
            _ = self.chunk.emitOp(self.a, defaultSpan(), .pop_scope) catch {};
            if (self.scope_depth != 0) self.scope_depth -= 1;
        }

        if (s.init) |clause| try self.compileForClause(clause);

        const enter = try self.emitLoopEnter(defaultSpan(), self.scope_depth);
        const loop_start: u32 = @intCast(self.chunk.code.items.len);

        if (s.condition) |cond| {
            try self.compileExpr(cond);
        } else {
            try self.emitConstValue(defaultSpan(), .{ .boolean = true });
        }

        const jf = try self.emitJump(defaultSpan(), .jump_if_false);
        try self.chunk.emitOp(self.a, defaultSpan(), .pop);

        try self.compileStmt(s.body.*);

        const post_ip: u32 = @intCast(self.chunk.code.items.len);
        if (s.post) |p| try self.compileForClause(p);
        try self.chunk.emitOp(self.a, defaultSpan(), .safepoint);
        try self.emitJumpBack(loop_start);

        const cond_false_ip: u32 = @intCast(self.chunk.code.items.len);
        self.patchJump(jf, cond_false_ip);
        try self.chunk.emitOp(self.a, defaultSpan(), .pop);

        const exit_ip: u32 = @intCast(self.chunk.code.items.len);
        try self.chunk.emitOp(self.a, defaultSpan(), .loop_exit);

        self.patchU32(enter.break_pos, exit_ip);
        self.patchU32(enter.continue_pos, post_ip);
    }

    fn compileForOf(self: *Compiler, s: ast.ForOfStmt) anyerror!void {
        try self.chunk.emitOp(self.a, defaultSpan(), .push_scope);
        self.scope_depth += 1;
        defer {
            _ = self.chunk.emitOp(self.a, defaultSpan(), .pop_scope) catch {};
            if (self.scope_depth != 0) self.scope_depth -= 1;
        }

        const tmp_arr_name = try std.fmt.allocPrint(self.rt.gpa, "\x01forof_arr_{d}", .{self.tmp_id});
        defer self.rt.gpa.free(tmp_arr_name);
        const tmp_i_name = try std.fmt.allocPrint(self.rt.gpa, "\x01forof_i_{d}", .{self.tmp_id});
        defer self.rt.gpa.free(tmp_i_name);
        self.tmp_id += 1;

        // __arr = iterable
        try self.compileExpr(s.iterable);
        const arr_idx = try self.constName(tmp_arr_name);
        try self.chunk.emitOp(self.a, defaultSpan(), .define_var);
        try self.chunk.emitU32(self.a, arr_idx);
        try self.chunk.emitU8(self.a, 1);
        try self.chunk.emitOp(self.a, defaultSpan(), .pop);

        // __i = 0
        try self.emitConstValue(defaultSpan(), .{ .number = 0 });
        const i_idx = try self.constName(tmp_i_name);
        try self.chunk.emitOp(self.a, defaultSpan(), .define_var);
        try self.chunk.emitU32(self.a, i_idx);
        try self.chunk.emitU8(self.a, 1);
        try self.chunk.emitOp(self.a, defaultSpan(), .pop);

        const enter = try self.emitLoopEnter(defaultSpan(), self.scope_depth);
        const loop_start: u32 = @intCast(self.chunk.code.items.len);

        // condition: __i < __arr.length
        try self.chunk.emitOp(self.a, defaultSpan(), .get_var);
        try self.chunk.emitU32(self.a, i_idx);
        try self.chunk.emitOp(self.a, defaultSpan(), .get_var);
        try self.chunk.emitU32(self.a, arr_idx);
        try self.chunk.emitOp(self.a, defaultSpan(), .array_len);
        try self.chunk.emitOp(self.a, defaultSpan(), .lt);

        const jf = try self.emitJump(defaultSpan(), .jump_if_false);
        try self.chunk.emitOp(self.a, defaultSpan(), .pop);

        // per-iteration scope
        try self.chunk.emitOp(self.a, defaultSpan(), .push_scope);
        self.scope_depth += 1;

        // const/let n = __arr[__i]
        try self.chunk.emitOp(self.a, defaultSpan(), .get_var);
        try self.chunk.emitU32(self.a, arr_idx);
        try self.chunk.emitOp(self.a, defaultSpan(), .get_var);
        try self.chunk.emitU32(self.a, i_idx);
        try self.chunk.emitOp(self.a, defaultSpan(), .get_index);
        const name_idx = try self.constName(s.name);
        try self.chunk.emitOp(self.a, s.name_span, .define_var);
        try self.chunk.emitU32(self.a, name_idx);
        try self.chunk.emitU8(self.a, if (s.kind != .@"const") 1 else 0);
        try self.chunk.emitOp(self.a, s.name_span, .pop);

        try self.compileStmt(s.body.*);

        try self.chunk.emitOp(self.a, defaultSpan(), .pop_scope);
        if (self.scope_depth != 0) self.scope_depth -= 1;

        const continue_ip: u32 = @intCast(self.chunk.code.items.len);

        // __i++
        try self.chunk.emitOp(self.a, defaultSpan(), .get_var);
        try self.chunk.emitU32(self.a, i_idx);
        try self.emitConstValue(defaultSpan(), .{ .number = 1 });
        try self.chunk.emitOp(self.a, defaultSpan(), .add);
        try self.chunk.emitOp(self.a, defaultSpan(), .set_var);
        try self.chunk.emitU32(self.a, i_idx);
        try self.chunk.emitOp(self.a, defaultSpan(), .pop);

        try self.chunk.emitOp(self.a, defaultSpan(), .safepoint);

        try self.emitJumpBack(loop_start);

        const cond_false_ip: u32 = @intCast(self.chunk.code.items.len);
        self.patchJump(jf, cond_false_ip);
        try self.chunk.emitOp(self.a, defaultSpan(), .pop);

        const exit_ip: u32 = @intCast(self.chunk.code.items.len);
        try self.chunk.emitOp(self.a, defaultSpan(), .loop_exit);

        self.patchU32(enter.break_pos, exit_ip);
        self.patchU32(enter.continue_pos, continue_ip);
    }

    fn compileForClause(self: *Compiler, clause: ast.ForClause) anyerror!void {
        switch (clause) {
            .var_decl => |v| {
                const init_val = if (v.init) |e| e else null;
                if (init_val) |e| {
                    try self.compileExpr(e);
                } else {
                    try self.emitConstValue(defaultSpan(), .undefined);
                }
                const name_idx = try self.constName(v.name);
                try self.chunk.emitOp(self.a, v.name_span, .define_var);
                try self.chunk.emitU32(self.a, name_idx);
                try self.chunk.emitU8(self.a, if (v.kind != .@"const") 1 else 0);
                try self.chunk.emitOp(self.a, v.name_span, .pop);
            },
            .assign => |a| {
                try self.compileAssignIdent(a.name, a.op, a.value, a.name_span);
                try self.chunk.emitOp(self.a, a.name_span, .pop);
            },
            .expr => |e| {
                try self.compileExpr(e);
                try self.chunk.emitOp(self.a, exprSpan(e), .pop);
            },
        }
    }

    fn compileExpr(self: *Compiler, expr: *ast.Expr) anyerror!void {
        switch (expr.*) {
            .number => |n| try self.emitConstValue(exprSpan(expr), .{ .number = n }),
            .string => |s| try self.emitConstValue(exprSpan(expr), .{ .string = try self.rt.newString(s) }),
            .boolean => |b| try self.emitConstValue(exprSpan(expr), .{ .boolean = b }),
            .null => try self.emitConstValue(exprSpan(expr), .null),
            .undefined => try self.emitConstValue(exprSpan(expr), .undefined),
            .this => try self.chunk.emitOp(self.a, exprSpan(expr), .this_),
            .ident => |id| {
                const idx = try self.constName(id.name);
                try self.chunk.emitOp(self.a, id.span, .get_var);
                try self.chunk.emitU32(self.a, idx);
            },
            .object_literal => |o| {
                try self.chunk.emitOp(self.a, exprSpan(expr), .new_object);
                for (o.props) |p| {
                    try self.chunk.emitOp(self.a, exprSpan(expr), .dup);
                    try self.compileExpr(p.value);
                    const key_idx = try self.constName(p.key);
                    try self.chunk.emitOp(self.a, exprSpan(expr), .set_prop_name);
                    try self.chunk.emitU32(self.a, key_idx);
                    try self.chunk.emitOp(self.a, exprSpan(expr), .pop);
                }
            },
            .array_literal => |a| {
                try self.chunk.emitOp(self.a, exprSpan(expr), .new_array);
                for (a.items) |it| {
                    try self.compileExpr(it);
                    try self.chunk.emitOp(self.a, exprSpan(expr), .array_push);
                }
            },
            .member => |m| {
                try self.compileExpr(m.object);
                const key_idx = try self.constName(m.property);
                try self.chunk.emitOp(self.a, exprSpan(expr), .get_prop_name);
                try self.chunk.emitU32(self.a, key_idx);
            },
            .index => |idx| {
                try self.compileExpr(idx.object);
                try self.compileExpr(idx.index);
                try self.chunk.emitOp(self.a, exprSpan(expr), .get_index);
            },
            .unary => |u| {
                try self.compileExpr(u.expr);
                try self.chunk.emitOp(self.a, exprSpan(expr), if (u.op == .neg) .neg else .not);
            },
            .binary => |b| {
                try self.compileExpr(b.left);
                try self.compileExpr(b.right);
                try self.chunk.emitOp(self.a, exprSpan(expr), binaryToOp(b.op));
            },
            .logical => |l| try self.compileLogical(exprSpan(expr), l),
            .update => |u| try self.compileUpdate(exprSpan(expr), u),
            .assign_expr => |a| try self.compileAssignExpr(exprSpan(expr), a),
            .function_expr => |f| {
                const name = f.name orelse "";
                const proto_idx = try self.constFunctionProto(name, f.params, f.body, f.span, f.is_arrow);
                try self.chunk.emitOp(self.a, defaultSpan(), .make_function);
                try self.chunk.emitU32(self.a, proto_idx);
            },
            .call => |c| try self.compileCall(exprSpan(expr), c),
        }
    }

    fn compileCall(self: *Compiler, span: ast.Span, c: ast.Call) anyerror!void {
        switch (c.callee.*) {
            .member => |m| {
                try self.compileExpr(m.object);
                try self.chunk.emitOp(self.a, span, .dup);
                const key_idx = try self.constName(m.property);
                try self.chunk.emitOp(self.a, span, .get_prop_name);
                try self.chunk.emitU32(self.a, key_idx);
                try self.chunk.emitOp(self.a, span, .swap);
            },
            .index => |ix| {
                try self.compileExpr(ix.object);
                try self.chunk.emitOp(self.a, span, .dup);
                try self.compileExpr(ix.index);
                try self.chunk.emitOp(self.a, span, .get_index);
                try self.chunk.emitOp(self.a, span, .swap);
            },
            else => {
                try self.compileExpr(c.callee);
                try self.emitConstValue(span, .undefined);
            },
        }
        for (c.args) |a| try self.compileExpr(a);
        try self.chunk.emitOp(self.a, span, .call);
        try self.chunk.emitU32(self.a, @intCast(c.args.len));
    }

    fn compileUpdate(self: *Compiler, span: ast.Span, u: ast.Update) anyerror!void {
        const name_idx = try self.constName(u.target.name);
        try self.chunk.emitOp(self.a, span, .get_var);
        try self.chunk.emitU32(self.a, name_idx);
        if (!u.prefix) {
            // Postfix: preserve the old value as the expression result.
            try self.chunk.emitOp(self.a, span, .dup);
        }
        try self.emitConstValue(span, .{ .number = 1 });
        try self.chunk.emitOp(self.a, span, if (u.op == .inc) .add else .sub);
        try self.chunk.emitOp(self.a, span, .set_var);
        try self.chunk.emitU32(self.a, name_idx);
        if (!u.prefix) {
            // Stack is now [old, new]; discard new and keep old.
            try self.chunk.emitOp(self.a, span, .pop);
        }
    }

    fn compileAssignIdent(self: *Compiler, name: []const u8, op: ast.AssignOp, value: *ast.Expr, span: ast.Span) anyerror!void {
        const name_idx = try self.constName(name);
        if (op == .assign) {
            try self.compileExpr(value);
            try self.chunk.emitOp(self.a, span, .set_var);
            try self.chunk.emitU32(self.a, name_idx);
            return;
        }

        try self.chunk.emitOp(self.a, span, .get_var);
        try self.chunk.emitU32(self.a, name_idx);
        try self.compileExpr(value);
        try self.chunk.emitOp(self.a, span, binaryToOp(assignToBinary(op)));
        try self.chunk.emitOp(self.a, span, .set_var);
        try self.chunk.emitU32(self.a, name_idx);
    }

    fn compileAssignExpr(self: *Compiler, span: ast.Span, a: ast.AssignExpr) anyerror!void {
        switch (a.target.*) {
            .ident => |id| return self.compileAssignIdent(id.name, a.op, a.value, id.span),
            .member => |m| return self.compileAssignMember(span, m, a.op, a.value),
            .index => |ix| return self.compileAssignIndex(span, ix, a.op, a.value),
            else => return error.RuntimeError,
        }
    }

    fn compileAssignMember(self: *Compiler, span: ast.Span, m: ast.Member, op: ast.AssignOp, value: *ast.Expr) anyerror!void {
        const key_idx = try self.constName(m.property);
        try self.compileExpr(m.object);
        if (op == .assign) {
            try self.compileExpr(value);
            try self.chunk.emitOp(self.a, span, .set_prop_name);
            try self.chunk.emitU32(self.a, key_idx);
            return;
        }

        try self.chunk.emitOp(self.a, span, .dup);
        try self.chunk.emitOp(self.a, span, .get_prop_name);
        try self.chunk.emitU32(self.a, key_idx);
        try self.compileExpr(value);
        try self.chunk.emitOp(self.a, span, binaryToOp(assignToBinary(op)));
        try self.chunk.emitOp(self.a, span, .set_prop_name);
        try self.chunk.emitU32(self.a, key_idx);
    }

    fn compileAssignIndex(self: *Compiler, span: ast.Span, ix: ast.Index, op: ast.AssignOp, value: *ast.Expr) anyerror!void {
        try self.compileExpr(ix.object);
        try self.compileExpr(ix.index);
        if (op == .assign) {
            try self.compileExpr(value);
            try self.chunk.emitOp(self.a, span, .set_index);
            return;
        }

        try self.chunk.emitOp(self.a, span, .dup); // duplicate key
        try self.chunk.emitOp(self.a, span, .get_index);
        try self.compileExpr(value);
        try self.chunk.emitOp(self.a, span, binaryToOp(assignToBinary(op)));
        try self.chunk.emitOp(self.a, span, .set_index);
    }

    fn compileLogical(self: *Compiler, span: ast.Span, l: ast.Logical) anyerror!void {
        switch (l.op) {
            .@"and" => {
                try self.compileExpr(l.left);
                try self.chunk.emitOp(self.a, span, .dup);
                const jf = try self.emitJump(span, .jump_if_false);
                try self.chunk.emitOp(self.a, span, .pop);
                try self.compileExpr(l.right);
                const end_ip: u32 = @intCast(self.chunk.code.items.len);
                self.patchJump(jf, end_ip);
            },
            .@"or" => {
                try self.compileExpr(l.left);
                try self.chunk.emitOp(self.a, span, .dup);
                const jt = try self.emitJump(span, .jump_if_true);
                try self.chunk.emitOp(self.a, span, .pop);
                try self.compileExpr(l.right);
                const end_ip: u32 = @intCast(self.chunk.code.items.len);
                self.patchJump(jt, end_ip);
            },
            .nullish => {
                try self.compileExpr(l.left);
                try self.chunk.emitOp(self.a, span, .dup);
                const jnull = try self.emitJump(span, .jump_if_nullish);
                try self.chunk.emitOp(self.a, span, .pop);
                const jend = try self.emitJump(span, .jump);
                const null_ip: u32 = @intCast(self.chunk.code.items.len);
                self.patchJump(jnull, null_ip);
                try self.chunk.emitOp(self.a, span, .pop);
                try self.compileExpr(l.right);
                const end_ip: u32 = @intCast(self.chunk.code.items.len);
                self.patchJump(jend, end_ip);
            },
        }
    }

    fn emitConstValue(self: *Compiler, span: ast.Span, v: Value) !void {
        const idx = try self.constValue(v);
        try self.chunk.emitOp(self.a, span, .const_);
        try self.chunk.emitU32(self.a, idx);
    }

    fn constValue(self: *Compiler, v: Value) !u32 {
        try self.chunk.consts.append(self.a, .{ .value = v });
        return @intCast(self.chunk.consts.items.len - 1);
    }

    fn constName(self: *Compiler, s: []const u8) !u32 {
        const owned = try self.a.dupe(u8, s);
        try self.chunk.consts.append(self.a, .{ .name = owned });
        return @intCast(self.chunk.consts.items.len - 1);
    }

    fn constFunctionProto(self: *Compiler, name: []const u8, params: []const []const u8, body: ast.Block, span: ast.Span, is_arrow: bool) !u32 {
        const a = self.a;
        const proto = try a.create(FunctionProto);
        const pname = try a.dupe(u8, name);
        const pparams = try a.alloc([]const u8, params.len);
        for (params, 0..) |p, i| pparams[i] = try a.dupe(u8, p);

        const chunk = try a.create(Chunk);
        chunk.* = .{};
        try self.rt.vm_chunks.append(self.rt.gpa, chunk);
        var child = Compiler.init(self.rt, a, chunk);
        defer child.deinit();
        try child.compileStmt(.{ .block = body });
        try chunk.emitOp(a, defaultSpan(), .safepoint);
        // implicit return undefined
        try child.emitConstValue(defaultSpan(), .undefined);
        try chunk.emitOp(a, defaultSpan(), .return_);

        proto.* = .{
            .name = pname,
            .params = pparams,
            .chunk = chunk,
            .is_arrow = is_arrow,
            .source_name = self.rt.current_source_name,
            .source = self.rt.current_source,
            .span = span,
        };
        try self.chunk.consts.append(self.a, .{ .proto = proto });
        return @intCast(self.chunk.consts.items.len - 1);
    }

    fn emitJump(self: *Compiler, span: ast.Span, op: Op) !u32 {
        try self.chunk.emitOp(self.a, span, op);
        const pos: u32 = @intCast(self.chunk.code.items.len);
        try self.chunk.emitI32(self.a, 0);
        return pos;
    }

    fn patchJump(self: *Compiler, operand_pos: u32, target_ip: u32) void {
        const base: i32 = @intCast(operand_pos + 4);
        const off: i32 = @intCast(@as(i32, @intCast(target_ip)) - base);
        var buf: [4]u8 = undefined;
        std.mem.writeInt(i32, &buf, off, .little);
        const start: usize = @intCast(operand_pos);
        @memcpy(self.chunk.code.items[start .. start + 4], buf[0..]);
    }

    const LoopEnterPatch = struct { break_pos: u32, continue_pos: u32 };

    fn emitLoopEnter(self: *Compiler, span: ast.Span, depth: u32) !LoopEnterPatch {
        try self.chunk.emitOp(self.a, span, .loop_enter);
        const break_pos: u32 = @intCast(self.chunk.code.items.len);
        try self.chunk.emitU32(self.a, 0);
        const continue_pos: u32 = @intCast(self.chunk.code.items.len);
        try self.chunk.emitU32(self.a, 0);
        try self.chunk.emitU32(self.a, depth);
        return .{ .break_pos = break_pos, .continue_pos = continue_pos };
    }

    fn patchU32(self: *Compiler, pos: u32, value: u32) void {
        var buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &buf, value, .little);
        const start: usize = @intCast(pos);
        @memcpy(self.chunk.code.items[start .. start + 4], buf[0..]);
    }

    fn emitJumpBack(self: *Compiler, target_ip: u32) !void {
        try self.chunk.emitOp(self.a, defaultSpan(), .jump);
        const base: i32 = @intCast(self.chunk.code.items.len + 4);
        const off: i32 = @intCast(@as(i32, @intCast(target_ip)) - base);
        try self.chunk.emitI32(self.a, off);
    }

    fn stmtSpan(stmt: ast.Stmt) ast.Span {
        return switch (stmt) {
            .var_decl => |s| s.name_span,
            .assign => |s| s.name_span,
            .expr_stmt => |s| exprSpan(s.expr),
            .for_of_stmt => |s| s.name_span,
            else => defaultSpan(),
        };
    }
};

fn binaryToOp(op: ast.BinaryOp) Op {
    return switch (op) {
        .add => .add,
        .sub => .sub,
        .mul => .mul,
        .div => .div,
        .mod => .mod,
        .eq => .eq,
        .neq => .neq,
        .strict_eq => .strict_eq,
        .strict_neq => .strict_neq,
        .lt => .lt,
        .lte => .lte,
        .gt => .gt,
        .gte => .gte,
    };
}

fn assignToBinary(op: ast.AssignOp) ast.BinaryOp {
    return switch (op) {
        .add_assign => .add,
        .sub_assign => .sub,
        .mul_assign => .mul,
        .div_assign => .div,
        .mod_assign => .mod,
        else => unreachable,
    };
}

fn defaultSpan() ast.Span {
    return .{ .start = 0, .end = 0, .line = 1, .col = 1 };
}

fn exprSpan(expr: *const ast.Expr) ast.Span {
    return switch (expr.*) {
        .ident => |id| id.span,
        .update => |u| u.target.span,
        else => defaultSpan(),
    };
}

fn bestCallSpan(callee: *const ast.Expr) ast.Span {
    return switch (callee.*) {
        .ident => |id| id.span,
        .member => |m| exprSpan(m.object),
        .index => |ix| exprSpan(ix.object),
        else => defaultSpan(),
    };
}

pub const Object = struct {
    header: GcHeader = undefined,
    prototype: ?*Object = null,
    props: std.StringHashMapUnmanaged(Value) = .{},
};

pub const Array = struct {
    header: GcHeader = undefined,
    prototype: ?*Object = null,
    props: std.StringHashMapUnmanaged(Value) = .{},
    items: std.ArrayListUnmanaged(Value) = .{},
};

pub const Value = union(enum) {
    undefined,
    null,
    boolean: bool,
    number: f64,
    string: *GcString,
    object: *Object,
    array: *Array,
    native_fn: NativeFunction,
    native_function: *NativeFunctionObject,
    function: *Function,
};

const GcKind = enum { object, array, function, native_function_object, string };

const GcHeader = struct {
    kind: GcKind,
    marked: bool,
    next: ?*GcHeader,
};

const GcString = struct {
    header: GcHeader = undefined,
    bytes: []u8,
};

pub const NativeFunctionObject = struct {
    header: GcHeader = undefined,
    prototype: ?*Object = null,
    props: std.StringHashMapUnmanaged(Value) = .{},
    func: NativeFunction,
    name: []const u8 = "",
};

fn toStringBytes(rt: *Runtime, v: Value) ![]const u8 {
    return switch (v) {
        .string => |s| s.bytes,
        else => (try toStringValue(rt, v)).string.bytes,
    };
}

fn toStringValue(rt: *Runtime, v: Value) !Value {
    return switch (v) {
        .string => v,
        .number => |n| blk: {
            const tmp = try std.fmt.allocPrint(rt.gpa, "{d}", .{n});
            defer rt.gpa.free(tmp);
            break :blk .{ .string = try rt.newString(tmp) };
        },
        .boolean => |b| .{ .string = try rt.newString(if (b) "true" else "false") },
        .null => .{ .string = try rt.newString("null") },
        .undefined => .{ .string = try rt.newString("undefined") },
        .object => .{ .string = try rt.newString("[Object]") },
        .array => .{ .string = try rt.newString("[Array]") },
        .native_fn => .{ .string = try rt.newString("[Function]") },
        .native_function => .{ .string = try rt.newString("[Function]") },
        .function => .{ .string = try rt.newString("[Function]") },
    };
}

fn isTruthy(v: Value) bool {
    return switch (v) {
        .undefined, .null => false,
        .boolean => |b| b,
        .number => |n| n != 0 and !std.math.isNan(n),
        .string => |s| s.bytes.len != 0,
        .object, .array, .native_fn, .native_function, .function => true,
    };
}

fn isEqual(a: Value, b: Value) bool {
    // Minimal JS-like equality for this tiny runtime (not fully spec compliant).
    if (a == .null and b == .undefined) return true;
    if (a == .undefined and b == .null) return true;

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

fn compare(op: ast.BinaryOp, left: Value, right: Value) !bool {
    switch (left) {
        .number => |l| switch (right) {
            .number => |r| return switch (op) {
                .lt => l < r,
                .lte => l <= r,
                .gt => l > r,
                .gte => l >= r,
                else => unreachable,
            },
            else => return error.RuntimeError,
        },
        .string => |l| switch (right) {
            .string => |r| {
                const ord = std.mem.order(u8, l.bytes, r.bytes);
                return switch (op) {
                    .lt => ord == .lt,
                    .lte => ord == .lt or ord == .eq,
                    .gt => ord == .gt,
                    .gte => ord == .gt or ord == .eq,
                    else => unreachable,
                };
            },
            else => return error.RuntimeError,
        },
        else => return error.RuntimeError,
    }
}

pub fn nativeObjectToString(rt: *Runtime, this_value: Value, _: []const Value) !Value {
    const tag: []const u8 = switch (this_value) {
        .undefined => "[object Undefined]",
        .null => "[object Null]",
        .boolean => "[object Boolean]",
        .number => "[object Number]",
        .string => "[object String]",
        .object => "[object Object]",
        .array => "[object Array]",
        .native_fn, .native_function, .function => "[object Function]",
    };
    return .{ .string = try rt.newString(tag) };
}

pub fn nativeFunctionToString(rt: *Runtime, this_value: Value, _: []const Value) !Value {
    switch (this_value) {
        .function => |f| {
            if (f.source_span) |sp| {
                const slice = if (sp.end <= f.source.len) f.source[sp.start..sp.end] else f.source;
                return .{ .string = try rt.newString(slice) };
            }
            return .{ .string = try rt.newString("[object Function]") };
        },
        .native_fn, .native_function => return .{ .string = try rt.newString("function () { [native code] }") },
        else => return error.RuntimeError,
    }
}

// Note: console/fs builtins live in `src/builtins/*`.

test "lexical environments shadow and restore" {
    const a = std.testing.allocator;
    var buf: [256]u8 = undefined;
    var out_writer = std.io.Writer.fixed(buf[0..]);
    out_writer.end = 0;

    var rt = try Runtime.init(a, &out_writer);
    defer rt.deinit();

    try rt.define("x", .{ .number = 1 }, true);
    try std.testing.expect((rt.get("x") orelse .undefined).number == 1);

    try rt.pushScope();
    try rt.define("x", .{ .number = 2 }, true);
    try std.testing.expect((rt.get("x") orelse .undefined).number == 2);

    rt.popScope();
    try std.testing.expect((rt.get("x") orelse .undefined).number == 1);
}

fn stripTypescript(rt: *Runtime, src: []const u8) ![]const u8 {
    // Best-effort type stripping to keep the parser tiny. This is intentionally incomplete.
    var tmp: std.ArrayList(u8) = .empty;
    errdefer tmp.deinit(rt.gpa);

    var i: usize = 0;
    var in_line_comment = false;
    var in_block_comment = false;
    var in_string: u8 = 0;
    var curly_depth: u32 = 0;

    while (i < src.len) : (i += 1) {
        const c = src[i];

        if (in_string != 0) {
            try tmp.append(rt.gpa, c);
            if (c == '\\') {
                if (i + 1 < src.len) {
                    i += 1;
                    try tmp.append(rt.gpa, src[i]);
                }
                continue;
            }
            if (c == in_string) in_string = 0;
            continue;
        }

        if (in_line_comment) {
            try tmp.append(rt.gpa, c);
            if (c == '\n') in_line_comment = false;
            continue;
        }

        if (in_block_comment) {
            try tmp.append(rt.gpa, c);
            if (c == '*' and i + 1 < src.len and src[i + 1] == '/') {
                i += 1;
                try tmp.append(rt.gpa, '/');
                in_block_comment = false;
            }
            continue;
        }

        if (c == '"' or c == '\'') {
            in_string = c;
            try tmp.append(rt.gpa, c);
            continue;
        }

        if (c == '/' and i + 1 < src.len and src[i + 1] == '/') {
            in_line_comment = true;
            try tmp.append(rt.gpa, '/');
            i += 1;
            try tmp.append(rt.gpa, '/');
            continue;
        }
        if (c == '/' and i + 1 < src.len and src[i + 1] == '*') {
            in_block_comment = true;
            try tmp.append(rt.gpa, '/');
            i += 1;
            try tmp.append(rt.gpa, '*');
            continue;
        }

        // Strip `as Type` assertions: `expr as Foo`
        if (isWordAt(src, i, "as") and isWordBoundary(src, i -| 1) and isWordBoundary(src, i + 2)) {
            // Drop `as`
            i += 2;
            // Skip whitespace
            while (i < src.len and (src[i] == ' ' or src[i] == '\t' or src[i] == '\n' or src[i] == '\r')) : (i += 1) {}
            // Skip a simple type expression until delimiter.
            while (i < src.len and !isTypeDelim(src[i])) : (i += 1) {}
            i -= 1;
            continue;
        }

        // Strip `: Type` annotations (common in variable decls and params).
        // Heuristic: avoid stripping inside `{ ... }` so object literals like `{ a: 1 }` survive.
        if (c == ':' and curly_depth == 0) {
            var j = i + 1;
            while (j < src.len and (src[j] == ' ' or src[j] == '\t')) : (j += 1) {}
            // If it's `::` (not TS), keep it.
            if (j < src.len and src[j] == ':') {
                try tmp.append(rt.gpa, ':');
                continue;
            }
            // Skip type tokens until a reasonable delimiter.
            while (j < src.len and !isTypeDelim(src[j])) : (j += 1) {}
            i = j - 1;
            continue;
        }

        if (c == '{') curly_depth += 1 else if (c == '}' and curly_depth > 0) curly_depth -= 1;
        try tmp.append(rt.gpa, c);
    }

    const owned = try tmp.toOwnedSlice(rt.gpa);
    defer rt.gpa.free(owned);
    return rt.dupe(owned);
}

fn isTypeDelim(c: u8) bool {
    return c == '=' or c == ',' or c == ')' or c == ';' or c == '\n' or c == '\r';
}

fn isWordAt(hay: []const u8, i: usize, needle: []const u8) bool {
    if (i + needle.len > hay.len) return false;
    return std.mem.eql(u8, hay[i .. i + needle.len], needle);
}

fn isWordBoundary(hay: []const u8, i: usize) bool {
    if (i >= hay.len) return true;
    const c = hay[i];
    return !((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_' or c == '$');
}

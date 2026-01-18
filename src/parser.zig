const std = @import("std");
const ast = @import("ast.zig");
const lex = @import("lexer.zig");

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: lex.Lexer,
    source_name: []const u8,
    cur: lex.Token,
    peek: lex.Token,
    last_diagnostic: ?Diagnostic = null,

    pub fn init(allocator: std.mem.Allocator, source: []const u8, source_name: []const u8) Parser {
        var lexer = lex.Lexer.init(source);
        const t0 = lexer.next();
        const t1 = lexer.next();
        return .{
            .allocator = allocator,
            .lexer = lexer,
            .source_name = source_name,
            .cur = t0,
            .peek = t1,
        };
    }

    pub fn parseProgram(self: *Parser) anyerror!ast.Program {
        var stmts: std.ArrayList(ast.Stmt) = .empty;
        defer stmts.deinit(self.allocator);

        while (self.cur.tag != .eof) {
            try stmts.append(self.allocator, try self.parseStmt());
        }

        return .{ .statements = try stmts.toOwnedSlice(self.allocator) };
    }

    pub const Diagnostic = struct {
        source_name: []const u8,
        source: []const u8,
        span: lex.Span,
        message: []const u8,
        expected: ?lex.TokenTag = null,
        got: lex.Token,

        pub fn render(self: *const Diagnostic, w: anytype) !void {
            try w.print("{s}:{d}:{d}: error: {s}\n", .{ self.source_name, self.span.line, self.span.col, self.message });
            if (self.expected) |exp| {
                try w.print("  expected: {s}\n", .{@tagName(exp)});
            }
            try w.print("       got: {s} ({s})\n", .{ @tagName(self.got.tag), self.got.lexeme });

            // Show the line and caret.
            const line_bounds = findLineBounds(self.source, self.span.start);
            const line_text = self.source[line_bounds.start..line_bounds.end];
            try w.print("  {s}\n", .{line_text});
            try w.writeAll("  ");
            // span.col is 1-based
            var i: u32 = 1;
            while (i < self.span.col) : (i += 1) try w.writeByte(' ');
            try w.writeAll("^\n");
        }
    };

    fn parseStmt(self: *Parser) anyerror!ast.Stmt {
        switch (self.cur.tag) {
            .kw_import => return .{ .import_stmt = try self.parseImport() },
            .kw_function => return .{ .function_decl = try self.parseFunctionDecl() },
            .kw_return => return .{ .return_stmt = try self.parseReturnStmt() },
            .kw_if => return .{ .if_stmt = try self.parseIfStmt() },
            .kw_while => return .{ .while_stmt = try self.parseWhileStmt() },
            .kw_for => return try self.parseForStmt(),
            .kw_break => return try self.parseBreakStmt(),
            .kw_continue => return try self.parseContinueStmt(),
            .kw_let, .kw_const, .kw_var => return .{ .var_decl = try self.parseVarDecl() },
            .lbrace => return .{ .block = try self.parseBlock() },
            else => {},
        }

        if (self.cur.tag == .identifier and (self.peek.tag == .equal or self.peek.tag == .plus_equal or self.peek.tag == .minus_equal or self.peek.tag == .star_equal or self.peek.tag == .slash_equal or self.peek.tag == .percent_equal)) {
            const name = self.cur.lexeme;
            const name_span = self.cur.span;
            self.advance(); // name
            const op: ast.AssignOp = switch (self.cur.tag) {
                .equal => .assign,
                .plus_equal => .add_assign,
                .minus_equal => .sub_assign,
                .star_equal => .mul_assign,
                .slash_equal => .div_assign,
                .percent_equal => .mod_assign,
                else => unreachable,
            };
            self.advance();
            const value = try self.parseExpr();
            _ = self.maybe(.semicolon);
            return .{ .assign = .{ .name = name, .name_span = name_span, .op = op, .value = value } };
        }

        const expr = try self.parseExpr();
        _ = self.maybe(.semicolon);
        return .{ .expr_stmt = .{ .expr = expr } };
    }

    fn parseImport(self: *Parser) anyerror!ast.ImportStmt {
        try self.expect(.kw_import);

        var bind_name: ?[]const u8 = null;
        if (self.cur.tag == .identifier and self.peek.tag == .kw_from) {
            bind_name = self.cur.lexeme;
            self.advance();
            try self.expect(.kw_from);
        }

        const raw = self.cur.lexeme;
        try self.expect(.string);
        const spec = try unquoteString(self.allocator, raw);
        _ = self.maybe(.semicolon);
        return .{ .bind_name = bind_name, .spec = spec };
    }

    fn parseVarDecl(self: *Parser) anyerror!ast.VarDecl {
        const kind: ast.VarKind = switch (self.cur.tag) {
            .kw_let => .@"let",
            .kw_const => .@"const",
            .kw_var => .@"var",
            else => return error.ParseError,
        };
        self.advance();

        const name = self.cur.lexeme;
        const name_span = self.cur.span;
        try self.expect(.identifier);

        var init_expr: ?*ast.Expr = null;
        if (self.maybe(.equal)) init_expr = try self.parseExpr();
        _ = self.maybe(.semicolon);
        return .{ .kind = kind, .name = name, .name_span = name_span, .init = init_expr };
    }

    fn parseFunctionDecl(self: *Parser) anyerror!ast.FunctionDecl {
        const start_tok = self.cur;
        try self.expect(.kw_function);
        const name = self.cur.lexeme;
        try self.expect(.identifier);
        try self.expect(.lparen);

        var params: std.ArrayList([]const u8) = .empty;
        defer params.deinit(self.allocator);

        if (!self.maybe(.rparen)) {
            while (true) {
                const p = self.cur.lexeme;
                try self.expect(.identifier);
                try params.append(self.allocator, p);
                if (self.maybe(.comma)) continue;
                try self.expect(.rparen);
                break;
            }
        }

        const body = try self.parseBlock();
        return .{
            .name = name,
            .params = try params.toOwnedSlice(self.allocator),
            .body = body,
            .span = .{
                .start = start_tok.span.start,
                .end = body.span.end,
                .line = start_tok.span.line,
                .col = start_tok.span.col,
            },
        };
    }

    fn parseReturnStmt(self: *Parser) anyerror!ast.ReturnStmt {
        try self.expect(.kw_return);
        if (self.maybe(.semicolon)) return .{ .value = null };
        if (self.cur.tag == .rbrace) return .{ .value = null };
        const e = try self.parseExpr();
        _ = self.maybe(.semicolon);
        return .{ .value = e };
    }

    fn parseBlock(self: *Parser) anyerror!ast.Block {
        const lbrace_tok = self.cur;
        try self.expect(.lbrace);
        var stmts: std.ArrayList(ast.Stmt) = .empty;
        defer stmts.deinit(self.allocator);

        while (self.cur.tag != .rbrace and self.cur.tag != .eof) {
            try stmts.append(self.allocator, try self.parseStmt());
        }
        const rbrace_tok = self.cur;
        try self.expect(.rbrace);
        return .{
            .span = .{
                .start = lbrace_tok.span.start,
                .end = rbrace_tok.span.end,
                .line = lbrace_tok.span.line,
                .col = lbrace_tok.span.col,
            },
            .statements = try stmts.toOwnedSlice(self.allocator),
        };
    }

    fn parseIfStmt(self: *Parser) anyerror!ast.IfStmt {
        try self.expect(.kw_if);
        try self.expect(.lparen);
        const condition = try self.parseExpr();
        try self.expect(.rparen);

        const then_stmt = try self.allocator.create(ast.Stmt);
        then_stmt.* = try self.parseStmt();

        var else_stmt: ?*ast.Stmt = null;
        if (self.maybe(.kw_else)) {
            const s = try self.allocator.create(ast.Stmt);
            s.* = try self.parseStmt();
            else_stmt = s;
        }

        return .{ .condition = condition, .then_branch = then_stmt, .else_branch = else_stmt };
    }

    fn parseWhileStmt(self: *Parser) anyerror!ast.WhileStmt {
        try self.expect(.kw_while);
        try self.expect(.lparen);
        const condition = try self.parseExpr();
        try self.expect(.rparen);

        const body = try self.allocator.create(ast.Stmt);
        body.* = try self.parseStmt();
        return .{ .condition = condition, .body = body };
    }

    fn parseForStmt(self: *Parser) anyerror!ast.Stmt {
        try self.expect(.kw_for);
        try self.expect(.lparen);

        // Detect `for (const x of iterable) stmt`
        if (self.cur.tag == .kw_let or self.cur.tag == .kw_const or self.cur.tag == .kw_var) {
            const kind: ast.VarKind = switch (self.cur.tag) {
                .kw_let => .@"let",
                .kw_const => .@"const",
                .kw_var => .@"var",
                else => unreachable,
            };
            self.advance();

            const name = self.cur.lexeme;
            const name_span = self.cur.span;
            try self.expect(.identifier);

            if (self.maybe(.kw_of)) {
                const iterable = try self.parseExpr();
                try self.expect(.rparen);
                const body = try self.allocator.create(ast.Stmt);
                body.* = try self.parseStmt();
                return .{ .for_of_stmt = .{ .kind = kind, .name = name, .name_span = name_span, .iterable = iterable, .body = body } };
            }

            // Not a for-of; fall back to classic `for` by treating this as init var-decl.
            var init_expr: ?*ast.Expr = null;
            if (self.maybe(.equal)) init_expr = try self.parseExpr();
            const init_clause: ?ast.ForClause = .{ .var_decl = .{ .kind = kind, .name = name, .name_span = name_span, .init = init_expr } };
            try self.expect(.semicolon);

            const condition: ?*ast.Expr = if (self.cur.tag == .semicolon) null else try self.parseExpr();
            try self.expect(.semicolon);

            const post: ?ast.ForClause = if (self.cur.tag == .rparen) null else try self.parseForClause();
            try self.expect(.rparen);

            const body = try self.allocator.create(ast.Stmt);
            body.* = try self.parseStmt();

            return .{ .for_stmt = .{ .init = init_clause, .condition = condition, .post = post, .body = body } };
        }

        const init_clause: ?ast.ForClause = if (self.cur.tag == .semicolon) null else try self.parseForClause();
        try self.expect(.semicolon);

        const condition: ?*ast.Expr = if (self.cur.tag == .semicolon) null else try self.parseExpr();
        try self.expect(.semicolon);

        const post: ?ast.ForClause = if (self.cur.tag == .rparen) null else try self.parseForClause();
        try self.expect(.rparen);

        const body = try self.allocator.create(ast.Stmt);
        body.* = try self.parseStmt();

        return .{ .for_stmt = .{ .init = init_clause, .condition = condition, .post = post, .body = body } };
    }

    fn parseForClause(self: *Parser) anyerror!ast.ForClause {
        // Allowed in init/post: var decl (no trailing ';'), assignment, or expression.
        switch (self.cur.tag) {
            .kw_let, .kw_const, .kw_var => return .{ .var_decl = try self.parseVarDeclNoSemi() },
            else => {},
        }

        if (self.cur.tag == .identifier and (self.peek.tag == .equal or self.peek.tag == .plus_equal or self.peek.tag == .minus_equal or self.peek.tag == .star_equal or self.peek.tag == .slash_equal or self.peek.tag == .percent_equal)) {
            const name = self.cur.lexeme;
            const name_span = self.cur.span;
            self.advance(); // name
            const op: ast.AssignOp = switch (self.cur.tag) {
                .equal => .assign,
                .plus_equal => .add_assign,
                .minus_equal => .sub_assign,
                .star_equal => .mul_assign,
                .slash_equal => .div_assign,
                .percent_equal => .mod_assign,
                else => unreachable,
            };
            self.advance();
            const value = try self.parseExpr();
            return .{ .assign = .{ .name = name, .name_span = name_span, .op = op, .value = value } };
        }

        return .{ .expr = try self.parseExpr() };
    }

    fn parseVarDeclNoSemi(self: *Parser) anyerror!ast.VarDecl {
        const kind: ast.VarKind = switch (self.cur.tag) {
            .kw_let => .@"let",
            .kw_const => .@"const",
            .kw_var => .@"var",
            else => return error.ParseError,
        };
        self.advance();

        const name = self.cur.lexeme;
        const name_span = self.cur.span;
        try self.expect(.identifier);

        var init_expr: ?*ast.Expr = null;
        if (self.maybe(.equal)) init_expr = try self.parseExpr();
        return .{ .kind = kind, .name = name, .name_span = name_span, .init = init_expr };
    }

    fn parseBreakStmt(self: *Parser) anyerror!ast.Stmt {
        try self.expect(.kw_break);
        _ = self.maybe(.semicolon);
        return .break_stmt;
    }

    fn parseContinueStmt(self: *Parser) anyerror!ast.Stmt {
        try self.expect(.kw_continue);
        _ = self.maybe(.semicolon);
        return .continue_stmt;
    }

    fn parseExpr(self: *Parser) anyerror!*ast.Expr {
        return self.parseAssignment();
    }

    fn parseAssignment(self: *Parser) anyerror!*ast.Expr {
        if (self.cur.tag == .identifier and self.peek.tag == .fat_arrow) {
            return self.parseArrowFunctionFromIdent();
        }
        if (self.cur.tag == .lparen and self.looksLikeArrowParamList()) {
            return self.parseArrowFunctionFromParenList();
        }

        const left = try self.parseOrNullish();
        const op: ?ast.AssignOp = switch (self.cur.tag) {
            .equal => .assign,
            .plus_equal => .add_assign,
            .minus_equal => .sub_assign,
            .star_equal => .mul_assign,
            .slash_equal => .div_assign,
            .percent_equal => .mod_assign,
            else => null,
        };
        if (op) |assign_op| {
            // Right-associative.
            self.advance();
            const rhs = try self.parseAssignment();
            if (!isAssignable(left)) {
                self.noteError(self.cur, "invalid assignment target", null);
                return error.ParseError;
            }
            return self.newExpr(.{ .assign_expr = .{ .op = assign_op, .target = left, .value = rhs } });
        }
        return left;
    }

    fn looksLikeArrowParamList(self: *Parser) bool {
        var tmp = self.*;
        if (tmp.cur.tag != .lparen) return false;
        tmp.advance(); // '('

        if (tmp.cur.tag != .rparen) {
            while (true) {
                if (tmp.cur.tag != .identifier) return false;
                tmp.advance();
                if (tmp.cur.tag == .comma) {
                    tmp.advance();
                    continue;
                }
                break;
            }
            if (tmp.cur.tag != .rparen) return false;
        }
        tmp.advance(); // ')'
        return tmp.cur.tag == .fat_arrow;
    }

    fn parseArrowFunctionFromIdent(self: *Parser) anyerror!*ast.Expr {
        const start_tok = self.cur;
        const param_name = self.cur.lexeme;
        self.advance(); // ident
        try self.expect(.fat_arrow);

        var params: std.ArrayList([]const u8) = .empty;
        defer params.deinit(self.allocator);
        try params.append(self.allocator, param_name);

        return self.parseArrowFunctionBody(start_tok, try params.toOwnedSlice(self.allocator));
    }

    fn parseArrowFunctionFromParenList(self: *Parser) anyerror!*ast.Expr {
        const start_tok = self.cur;
        try self.expect(.lparen);

        var params: std.ArrayList([]const u8) = .empty;
        defer params.deinit(self.allocator);
        if (!self.maybe(.rparen)) {
            while (true) {
                const p = self.cur.lexeme;
                try self.expect(.identifier);
                try params.append(self.allocator, p);
                if (self.maybe(.comma)) continue;
                try self.expect(.rparen);
                break;
            }
        }

        try self.expect(.fat_arrow);
        return self.parseArrowFunctionBody(start_tok, try params.toOwnedSlice(self.allocator));
    }

    fn parseArrowFunctionBody(self: *Parser, start_tok: lex.Token, params: []const []const u8) anyerror!*ast.Expr {
        if (self.cur.tag == .lbrace) {
            const body = try self.parseBlock();
            return self.newExpr(.{
                .function_expr = .{
                    .name = null,
                    .params = params,
                    .is_arrow = true,
                    .body = body,
                    .span = .{
                        .start = start_tok.span.start,
                        .end = body.span.end,
                        .line = start_tok.span.line,
                        .col = start_tok.span.col,
                    },
                },
            });
        }

        const expr = try self.parseAssignment();
        const end_pos: usize = self.cur.span.start;

        var stmts: std.ArrayList(ast.Stmt) = .empty;
        defer stmts.deinit(self.allocator);
        try stmts.append(self.allocator, .{ .return_stmt = .{ .value = expr } });

        const body: ast.Block = .{
            .span = .{ .start = start_tok.span.start, .end = end_pos, .line = start_tok.span.line, .col = start_tok.span.col },
            .statements = try stmts.toOwnedSlice(self.allocator),
        };

        return self.newExpr(.{
            .function_expr = .{
                .name = null,
                .params = params,
                .is_arrow = true,
                .body = body,
                .span = .{ .start = start_tok.span.start, .end = end_pos, .line = start_tok.span.line, .col = start_tok.span.col },
            },
        });
    }

    fn parseOrNullish(self: *Parser) anyerror!*ast.Expr {
        var left = try self.parseAnd();
        while (self.cur.tag == .pipe_pipe or self.cur.tag == .question_question) {
            const op: ast.LogicalOp = if (self.cur.tag == .pipe_pipe) .@"or" else .nullish;
            self.advance();
            const right = try self.parseAnd();
            left = try self.newExpr(.{ .logical = .{ .op = op, .left = left, .right = right } });
        }
        return left;
    }

    fn parseAnd(self: *Parser) anyerror!*ast.Expr {
        var left = try self.parseEquality();
        while (self.cur.tag == .amp_amp) {
            self.advance();
            const right = try self.parseEquality();
            left = try self.newExpr(.{ .logical = .{ .op = .@"and", .left = left, .right = right } });
        }
        return left;
    }

    fn parseEquality(self: *Parser) anyerror!*ast.Expr {
        var left = try self.parseComparison();
        while (self.cur.tag == .equal_equal or self.cur.tag == .bang_equal or self.cur.tag == .equal_equal_equal or self.cur.tag == .bang_equal_equal) {
            const op: ast.BinaryOp = switch (self.cur.tag) {
                .equal_equal => .eq,
                .bang_equal => .neq,
                .equal_equal_equal => .strict_eq,
                .bang_equal_equal => .strict_neq,
                else => unreachable,
            };
            self.advance();
            const right = try self.parseComparison();
            left = try self.newExpr(.{ .binary = .{ .op = op, .left = left, .right = right } });
        }
        return left;
    }

    fn parseComparison(self: *Parser) anyerror!*ast.Expr {
        var left = try self.parseAdd();
        while (self.cur.tag == .lt or self.cur.tag == .lte or self.cur.tag == .gt or self.cur.tag == .gte) {
            const op: ast.BinaryOp = switch (self.cur.tag) {
                .lt => .lt,
                .lte => .lte,
                .gt => .gt,
                .gte => .gte,
                else => unreachable,
            };
            self.advance();
            const right = try self.parseAdd();
            left = try self.newExpr(.{ .binary = .{ .op = op, .left = left, .right = right } });
        }
        return left;
    }

    fn parseAdd(self: *Parser) anyerror!*ast.Expr {
        var left = try self.parseMul();
        while (self.cur.tag == .plus or self.cur.tag == .minus) {
            const op: ast.BinaryOp = if (self.cur.tag == .plus) .add else .sub;
            self.advance();
            const right = try self.parseMul();
            left = try self.newExpr(.{ .binary = .{ .op = op, .left = left, .right = right } });
        }
        return left;
    }

    fn parseMul(self: *Parser) anyerror!*ast.Expr {
        var left = try self.parseUnary();
        while (self.cur.tag == .star or self.cur.tag == .slash or self.cur.tag == .percent) {
            const op: ast.BinaryOp = switch (self.cur.tag) {
                .star => .mul,
                .slash => .div,
                .percent => .mod,
                else => unreachable,
            };
            self.advance();
            const right = try self.parseUnary();
            left = try self.newExpr(.{ .binary = .{ .op = op, .left = left, .right = right } });
        }
        return left;
    }

    fn parseUnary(self: *Parser) anyerror!*ast.Expr {
        if (self.cur.tag == .plus_plus) {
            self.advance();
            const target_tok = self.cur;
            try self.expect(.identifier);
            return self.newExpr(.{ .update = .{ .op = .inc, .target = .{ .name = target_tok.lexeme, .span = target_tok.span }, .prefix = true } });
        }
        if (self.cur.tag == .minus_minus) {
            self.advance();
            const target_tok = self.cur;
            try self.expect(.identifier);
            return self.newExpr(.{ .update = .{ .op = .dec, .target = .{ .name = target_tok.lexeme, .span = target_tok.span }, .prefix = true } });
        }
        if (self.cur.tag == .minus) {
            self.advance();
            const inner = try self.parseUnary();
            return self.newExpr(.{ .unary = .{ .op = .neg, .expr = inner } });
        }
        if (self.cur.tag == .bang) {
            self.advance();
            const inner = try self.parseUnary();
            return self.newExpr(.{ .unary = .{ .op = .not, .expr = inner } });
        }
        return self.parsePostfix();
    }

    fn parsePostfix(self: *Parser) anyerror!*ast.Expr {
        var expr = try self.parsePrimary();
        while (true) {
            // Postfix update operators: `x++` / `x--` (identifier only for now).
            if (self.cur.tag == .plus_plus or self.cur.tag == .minus_minus) {
                const op: ast.UpdateOp = if (self.cur.tag == .plus_plus) .inc else .dec;
                self.advance();
                const target = switch (expr.*) {
                    .ident => |id| id,
                    else => return error.ParseError,
                };
                expr = try self.newExpr(.{ .update = .{ .op = op, .target = target, .prefix = false } });
                continue;
            }

            if (self.maybe(.lbracket)) {
                const idx = try self.parseExpr();
                try self.expect(.rbracket);
                expr = try self.newExpr(.{ .index = .{ .object = expr, .index = idx } });
                continue;
            }

            if (self.maybe(.dot)) {
                const prop = self.cur.lexeme;
                try self.expect(.identifier);
                expr = try self.newExpr(.{ .member = .{ .object = expr, .property = prop } });
                continue;
            }

            if (self.maybe(.lparen)) {
                var args: std.ArrayList(*ast.Expr) = .empty;
                defer args.deinit(self.allocator);

                if (!self.maybe(.rparen)) {
                    while (true) {
                        try args.append(self.allocator, try self.parseExpr());
                        if (self.maybe(.comma)) continue;
                        try self.expect(.rparen);
                        break;
                    }
                }

                expr = try self.newExpr(.{ .call = .{ .callee = expr, .args = try args.toOwnedSlice(self.allocator) } });
                continue;
            }

            break;
        }
        return expr;
    }

    fn parsePrimary(self: *Parser) anyerror!*ast.Expr {
        const t = self.cur;
        switch (t.tag) {
            .number => {
                self.advance();
                const n = try std.fmt.parseFloat(f64, t.lexeme);
                return self.newExpr(.{ .number = n });
            },
            .string => {
                self.advance();
                return self.newExpr(.{ .string = try unquoteString(self.allocator, t.lexeme) });
            },
            .identifier => {
                self.advance();
                return self.newExpr(.{ .ident = .{ .name = t.lexeme, .span = t.span } });
            },
            .kw_this => {
                self.advance();
                return self.newExpr(.this);
            },
            .kw_true => {
                self.advance();
                return self.newExpr(.{ .boolean = true });
            },
            .kw_false => {
                self.advance();
                return self.newExpr(.{ .boolean = false });
            },
            .kw_null => {
                self.advance();
                return self.newExpr(.null);
            },
            .kw_undefined => {
                self.advance();
                return self.newExpr(.undefined);
            },
            .kw_function => return self.parseFunctionExpr(),
            .lparen => {
                self.advance();
                const e = try self.parseExpr();
                try self.expect(.rparen);
                return e;
            },
            .lbrace => return self.parseObjectLiteral(),
            .lbracket => return self.parseArrayLiteral(),
            .invalid => {
                self.noteError(t, "invalid token", null);
                return error.ParseError;
            },
            else => {
                self.noteError(
                    t,
                    "expected an expression",
                    null,
                );
                return error.ParseError;
            },
        }
    }

    fn parseFunctionExpr(self: *Parser) anyerror!*ast.Expr {
        const start_tok = self.cur;
        try self.expect(.kw_function);

        var name: ?[]const u8 = null;
        if (self.cur.tag == .identifier) {
            name = self.cur.lexeme;
            self.advance();
        }

        try self.expect(.lparen);
        var params: std.ArrayList([]const u8) = .empty;
        defer params.deinit(self.allocator);

        if (!self.maybe(.rparen)) {
            while (true) {
                const p = self.cur.lexeme;
                try self.expect(.identifier);
                try params.append(self.allocator, p);
                if (self.maybe(.comma)) continue;
                try self.expect(.rparen);
                break;
            }
        }

        const body = try self.parseBlock();
        return self.newExpr(.{
            .function_expr = .{
                .name = name,
                .params = try params.toOwnedSlice(self.allocator),
                .is_arrow = false,
                .body = body,
                .span = .{
                    .start = start_tok.span.start,
                    .end = body.span.end,
                    .line = start_tok.span.line,
                    .col = start_tok.span.col,
                },
            },
        });
    }

    fn parseObjectLiteral(self: *Parser) anyerror!*ast.Expr {
        try self.expect(.lbrace);

        var props: std.ArrayList(ast.ObjectProp) = .empty;
        defer props.deinit(self.allocator);

        if (self.maybe(.rbrace)) {
            return self.newExpr(.{ .object_literal = .{ .props = try props.toOwnedSlice(self.allocator) } });
        }

        while (true) {
            var key: []const u8 = undefined;
            switch (self.cur.tag) {
                .identifier => {
                    key = self.cur.lexeme;
                    self.advance();
                },
                .string => {
                    key = try unquoteString(self.allocator, self.cur.lexeme);
                    self.advance();
                },
                else => return error.ParseError,
            }

            try self.expect(.colon);
            const value = try self.parseExpr();
            try props.append(self.allocator, .{ .key = key, .value = value });

            if (self.maybe(.comma)) {
                if (self.maybe(.rbrace)) break;
                continue;
            }
            try self.expect(.rbrace);
            break;
        }

        return self.newExpr(.{ .object_literal = .{ .props = try props.toOwnedSlice(self.allocator) } });
    }

    fn parseArrayLiteral(self: *Parser) anyerror!*ast.Expr {
        try self.expect(.lbracket);

        var items: std.ArrayList(*ast.Expr) = .empty;
        defer items.deinit(self.allocator);

        if (self.maybe(.rbracket)) {
            return self.newExpr(.{ .array_literal = .{ .items = try items.toOwnedSlice(self.allocator) } });
        }

        while (true) {
            try items.append(self.allocator, try self.parseExpr());
            if (self.maybe(.comma)) {
                if (self.maybe(.rbracket)) break;
                continue;
            }
            try self.expect(.rbracket);
            break;
        }

        return self.newExpr(.{ .array_literal = .{ .items = try items.toOwnedSlice(self.allocator) } });
    }

    fn newExpr(self: *Parser, v: ast.Expr) anyerror!*ast.Expr {
        const node = try self.allocator.create(ast.Expr);
        node.* = v;
        return node;
    }

    fn maybe(self: *Parser, tag: lex.TokenTag) bool {
        if (self.cur.tag != tag) return false;
        self.advance();
        return true;
    }

    fn expect(self: *Parser, tag: lex.TokenTag) anyerror!void {
        if (self.cur.tag != tag) {
            self.noteError(self.cur, "unexpected token", tag);
            return error.ParseError;
        }
        self.advance();
    }

    fn advance(self: *Parser) void {
        self.cur = self.peek;
        self.peek = self.lexer.next();
    }

    fn noteError(self: *Parser, got: lex.Token, message: []const u8, expected: ?lex.TokenTag) void {
        // Keep the first error; it tends to be the most relevant.
        if (self.last_diagnostic != null) return;
        self.last_diagnostic = .{
            .source_name = self.source_name,
            .source = self.lexer.source,
            .span = got.span,
            .message = message,
            .expected = expected,
            .got = got,
        };
    }
};

fn unquoteString(allocator: std.mem.Allocator, raw: []const u8) anyerror![]const u8 {
    if (raw.len < 2) return error.ParseError;
    const quote = raw[0];
    if ((quote != '"' and quote != '\'') or raw[raw.len - 1] != quote) return error.ParseError;

    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(allocator);

    var i: usize = 1;
    while (i + 1 < raw.len) : (i += 1) {
        const c = raw[i];
        if (c != '\\') {
            try out.append(allocator, c);
            continue;
        }
        if (i + 1 >= raw.len - 1) return error.ParseError;
        const esc = raw[i + 1];
        i += 1;
        switch (esc) {
            'n' => try out.append(allocator, '\n'),
            'r' => try out.append(allocator, '\r'),
            't' => try out.append(allocator, '\t'),
            '\\' => try out.append(allocator, '\\'),
            '"' => try out.append(allocator, '"'),
            '\'' => try out.append(allocator, '\''),
            else => try out.append(allocator, esc),
        }
    }

    return out.toOwnedSlice(allocator);
}

fn findLineBounds(source: []const u8, index: usize) struct { start: usize, end: usize } {
    var start = index;
    while (start > 0 and source[start - 1] != '\n') start -= 1;
    var end = index;
    while (end < source.len and source[end] != '\n') end += 1;
    return .{ .start = start, .end = end };
}

fn isAssignable(e: *const ast.Expr) bool {
    return switch (e.*) {
        .ident, .member, .index => true,
        else => false,
    };
}

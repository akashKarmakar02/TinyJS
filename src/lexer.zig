const std = @import("std");

pub const Span = struct {
    start: usize,
    end: usize,
    line: u32, // 1-based
    col: u32, // 1-based
};

pub const TokenTag = enum {
    eof,
    invalid,

    identifier,
    number,
    string,

    kw_import,
    kw_from,
    kw_function,
    kw_return,
    kw_this,
    kw_if,
    kw_else,
    kw_while,
    kw_for,
    kw_of,
    kw_break,
    kw_continue,
    kw_let,
    kw_const,
    kw_var,
    kw_true,
    kw_false,
    kw_null,
    kw_undefined,

    lparen,
    rparen,
    lbrace,
    rbrace,
    lbracket,
    rbracket,
    dot,
    comma,
    colon,
    semicolon,
    plus,
    minus,
    plus_plus,
    minus_minus,
    plus_equal,
    minus_equal,
    star_equal,
    slash_equal,
    percent_equal,
    star,
    slash,
    percent,
    bang,
    equal,
    fat_arrow,
    equal_equal,
    equal_equal_equal,
    bang_equal,
    bang_equal_equal,
    lt,
    lte,
    gt,
    gte,
    amp_amp,
    pipe_pipe,
    question_question,
};

pub const Token = struct {
    tag: TokenTag,
    lexeme: []const u8,
    span: Span,
};

pub const Lexer = struct {
    source: []const u8,
    index: usize = 0,
    line: u32 = 1,
    col: u32 = 1,

    pub fn init(source: []const u8) Lexer {
        return .{ .source = source };
    }

    pub fn next(self: *Lexer) Token {
        self.skipTrivia();
        const start = self.index;
        const line = self.line;
        const col = self.col;

        if (self.index >= self.source.len) {
            return .{ .tag = .eof, .lexeme = "", .span = .{ .start = start, .end = start, .line = line, .col = col } };
        }

        const c = self.source[self.index];

        if (isIdentStart(c)) {
            self.advanceOne();
            while (self.index < self.source.len and isIdentContinue(self.source[self.index])) self.advanceOne();
            const lexeme = self.source[start..self.index];
            return .{ .tag = keywordTag(lexeme), .lexeme = lexeme, .span = .{ .start = start, .end = self.index, .line = line, .col = col } };
        }

        if (isDigit(c)) {
            self.advanceOne();
            while (self.index < self.source.len and (isDigit(self.source[self.index]) or self.source[self.index] == '.')) self.advanceOne();
            const lexeme = self.source[start..self.index];
            return .{ .tag = .number, .lexeme = lexeme, .span = .{ .start = start, .end = self.index, .line = line, .col = col } };
        }

        if (c == '"' or c == '\'') {
            const quote = c;
            self.advanceOne();
            while (self.index < self.source.len) {
                const ch = self.source[self.index];
                if (ch == '\\') {
                    self.advanceOne();
                    if (self.index < self.source.len) self.advanceOne();
                    continue;
                }
                if (ch == quote) {
                    self.advanceOne();
                    break;
                }
                self.advanceOne();
            }
            const lexeme = self.source[start..self.index];
            return .{ .tag = .string, .lexeme = lexeme, .span = .{ .start = start, .end = self.index, .line = line, .col = col } };
        }

        switch (c) {
            '&' => {
                if (self.peekChar() == '&') return self.doubleToken(line, col, .amp_amp);
                return self.singleToken(line, col, .invalid);
            },
            '|' => {
                if (self.peekChar() == '|') return self.doubleToken(line, col, .pipe_pipe);
                return self.singleToken(line, col, .invalid);
            },
            '?' => {
                if (self.peekChar() == '?') return self.doubleToken(line, col, .question_question);
                return self.singleToken(line, col, .invalid);
            },
            '=' => {
                if (self.peekChar() == '>') return self.doubleToken(line, col, .fat_arrow);
                if (self.peekChar() == '=' and self.peekChar2() == '=') return self.tripleToken(line, col, .equal_equal_equal);
                if (self.peekChar() == '=') return self.doubleToken(line, col, .equal_equal);
                return self.singleToken(line, col, .equal);
            },
            '!' => {
                if (self.peekChar() == '=' and self.peekChar2() == '=') return self.tripleToken(line, col, .bang_equal_equal);
                if (self.peekChar() == '=') return self.doubleToken(line, col, .bang_equal);
                return self.singleToken(line, col, .bang);
            },
            '<' => {
                if (self.peekChar() == '=') {
                    return self.doubleToken(line, col, .lte);
                }
                return self.singleToken(line, col, .lt);
            },
            '>' => {
                if (self.peekChar() == '=') {
                    return self.doubleToken(line, col, .gte);
                }
                return self.singleToken(line, col, .gt);
            },
            '+' => {
                if (self.peekChar() == '+') return self.doubleToken(line, col, .plus_plus);
                if (self.peekChar() == '=') return self.doubleToken(line, col, .plus_equal);
                return self.singleToken(line, col, .plus);
            },
            '%' => {
                if (self.peekChar() == '=') return self.doubleToken(line, col, .percent_equal);
                return self.singleToken(line, col, .percent);
            },
            '-' => {
                if (self.peekChar() == '-') return self.doubleToken(line, col, .minus_minus);
                if (self.peekChar() == '=') return self.doubleToken(line, col, .minus_equal);
                return self.singleToken(line, col, .minus);
            },
            '*' => {
                if (self.peekChar() == '=') return self.doubleToken(line, col, .star_equal);
                return self.singleToken(line, col, .star);
            },
            '/' => {
                if (self.peekChar() == '=') return self.doubleToken(line, col, .slash_equal);
                return self.singleToken(line, col, .slash);
            },
            else => return self.singleToken(line, col, switch (c) {
                '(' => .lparen,
                ')' => .rparen,
                '{' => .lbrace,
                '}' => .rbrace,
                '[' => .lbracket,
                ']' => .rbracket,
                '.' => .dot,
                ',' => .comma,
                ':' => .colon,
                ';' => .semicolon,
                else => .invalid,
            }),
        }
    }

    fn singleToken(self: *Lexer, line: u32, col: u32, tag: TokenTag) Token {
        const start = self.index;
        self.advanceOne();
        return .{ .tag = tag, .lexeme = self.source[start..self.index], .span = .{ .start = start, .end = self.index, .line = line, .col = col } };
    }

    fn doubleToken(self: *Lexer, line: u32, col: u32, tag: TokenTag) Token {
        const start = self.index;
        self.advanceOne();
        self.advanceOne();
        return .{ .tag = tag, .lexeme = self.source[start..self.index], .span = .{ .start = start, .end = self.index, .line = line, .col = col } };
    }

    fn tripleToken(self: *Lexer, line: u32, col: u32, tag: TokenTag) Token {
        const start = self.index;
        self.advanceOne();
        self.advanceOne();
        self.advanceOne();
        return .{ .tag = tag, .lexeme = self.source[start..self.index], .span = .{ .start = start, .end = self.index, .line = line, .col = col } };
    }

    fn peekChar(self: *const Lexer) ?u8 {
        if (self.index + 1 >= self.source.len) return null;
        return self.source[self.index + 1];
    }

    fn peekChar2(self: *const Lexer) ?u8 {
        if (self.index + 2 >= self.source.len) return null;
        return self.source[self.index + 2];
    }

    fn skipTrivia(self: *Lexer) void {
        while (self.index < self.source.len) {
            const c = self.source[self.index];
            if (c == ' ' or c == '\t' or c == '\r') {
                self.advanceOne();
                continue;
            }
            if (c == '\n') {
                self.advanceOne();
                continue;
            }
            if (c == '/' and self.index + 1 < self.source.len and self.source[self.index + 1] == '/') {
                // line comment
                self.advanceOne();
                self.advanceOne();
                while (self.index < self.source.len and self.source[self.index] != '\n') self.advanceOne();
                continue;
            }
            if (c == '/' and self.index + 1 < self.source.len and self.source[self.index + 1] == '*') {
                // block comment
                self.advanceOne();
                self.advanceOne();
                while (self.index + 1 < self.source.len) {
                    if (self.source[self.index] == '*' and self.source[self.index + 1] == '/') {
                        self.advanceOne();
                        self.advanceOne();
                        break;
                    }
                    self.advanceOne();
                }
                continue;
            }
            break;
        }
    }

    fn advanceOne(self: *Lexer) void {
        const c = self.source[self.index];
        self.index += 1;
        if (c == '\n') {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
    }

    fn isIdentStart(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_' or c == '$';
    }

    fn isIdentContinue(c: u8) bool {
        return isIdentStart(c) or isDigit(c);
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn keywordTag(lexeme: []const u8) TokenTag {
        if (std.mem.eql(u8, lexeme, "import")) return .kw_import;
        if (std.mem.eql(u8, lexeme, "from")) return .kw_from;
        if (std.mem.eql(u8, lexeme, "function")) return .kw_function;
        if (std.mem.eql(u8, lexeme, "return")) return .kw_return;
        if (std.mem.eql(u8, lexeme, "this")) return .kw_this;
        if (std.mem.eql(u8, lexeme, "if")) return .kw_if;
        if (std.mem.eql(u8, lexeme, "else")) return .kw_else;
        if (std.mem.eql(u8, lexeme, "while")) return .kw_while;
        if (std.mem.eql(u8, lexeme, "for")) return .kw_for;
        if (std.mem.eql(u8, lexeme, "of")) return .kw_of;
        if (std.mem.eql(u8, lexeme, "break")) return .kw_break;
        if (std.mem.eql(u8, lexeme, "continue")) return .kw_continue;
        if (std.mem.eql(u8, lexeme, "let")) return .kw_let;
        if (std.mem.eql(u8, lexeme, "const")) return .kw_const;
        if (std.mem.eql(u8, lexeme, "var")) return .kw_var;
        if (std.mem.eql(u8, lexeme, "true")) return .kw_true;
        if (std.mem.eql(u8, lexeme, "false")) return .kw_false;
        if (std.mem.eql(u8, lexeme, "null")) return .kw_null;
        if (std.mem.eql(u8, lexeme, "undefined")) return .kw_undefined;
        return .identifier;
    }
};

const std = @import("std");
const Token = @import("Token.zig");

const Scanner = @This();

/// The full source text being scanned.
source: []const u8,
/// Collected tokens.
tokens: std.ArrayList(Token),
/// Allocator for the token list.
allocator: std.mem.Allocator,
/// Start of the current lexeme being scanned.
start: usize,
/// Current position in source (next character to read).
current: usize,
/// Current line number (1-based).
line: usize,

pub fn init(allocator: std.mem.Allocator, source: []const u8) Scanner {
    return .{
        .source = source,
        .tokens = .empty,
        .allocator = allocator,
        .start = 0,
        .current = 0,
        .line = 1,
    };
}

pub fn deinit(self: *Scanner) void {
    self.tokens.deinit(self.allocator);
}

/// Scan the entire source and return the token list.
pub fn scanTokens(self: *Scanner) ![]const Token {
    while (!self.isAtEnd()) {
        // We are at the beginning of the next lexeme.
        self.start = self.current;
        try self.scanToken();
    }

    // Append final EOF token.
    try self.tokens.append(self.allocator, .{
        .tag = .eof,
        .start = self.current,
        .len = 0,
        .line = self.line,
    });

    return self.tokens.items;
}

fn scanToken(self: *Scanner) !void {
    const c = self.advance();
    switch (c) {
        '(' => try self.addToken(.left_paren),
        ')' => try self.addToken(.right_paren),
        '[' => try self.addToken(.left_bracket),
        ']' => try self.addToken(.right_bracket),
        '*' => try self.addToken(.star),
        '/' => try self.addToken(.slash),
        '=' => try self.addToken(if (self.match('/')) .equals_slash else .equals),

        // String literals — "..."
        '"' => {
            while (self.peek() != '"' and !self.isAtEnd()) {
                if (self.peek() == '\n') self.line += 1;
                _ = self.advance();
            }
            if (self.isAtEnd()) {
                try self.addToken(.invalid); // unterminated string
            } else {
                _ = self.advance(); // consume closing "
                try self.addToken(.char_val);
            }
        },

        // Prose values — <...>
        '<' => {
            while (self.peek() != '>' and !self.isAtEnd()) {
                _ = self.advance();
            }
            if (self.isAtEnd()) {
                try self.addToken(.invalid); // unterminated prose
            } else {
                _ = self.advance(); // consume closing >
                try self.addToken(.prose_val);
            }
        },

        // Numeric values — %b, %d, %x
        '%' => {
            const base = self.peek();
            switch (base) {
                'b' => {
                    _ = self.advance(); // consume 'b'
                    self.consumeDigits(isBit);
                    try self.addToken(.bin_val);
                },
                'd' => {
                    _ = self.advance(); // consume 'd'
                    self.consumeDigits(isDigit);
                    try self.addToken(.dec_val);
                },
                'x' => {
                    _ = self.advance(); // consume 'x'
                    self.consumeDigits(isHexDigit);
                    try self.addToken(.hex_val);
                },
                else => try self.addToken(.invalid), // bare % with no base
            }
        },

        // Comments — consume until end of line.
        ';' => {
            while (self.peek() != '\n' and self.peek() != '\r' and !self.isAtEnd()) {
                _ = self.advance();
            }
            try self.addToken(.comment);
        },

        // Whitespace — skip silently.
        ' ', '\t' => {},

        // Newlines — emit token and bump line counter.
        '\r' => {
            _ = self.match('\n'); // consume LF after CR (CRLF)
            self.line += 1;
            try self.addToken(.newline);
        },
        '\n' => {
            self.line += 1;
            try self.addToken(.newline);
        },

        else => {
            if (isAlpha(c)) {
                // Rulename: ALPHA *(ALPHA / DIGIT / "-")
                while (isAlpha(self.peek()) or isDigit(self.peek()) or self.peek() == '-') {
                    _ = self.advance();
                }
                try self.addToken(.rulename);
            } else if (isDigit(c)) {
                // Number: 1*DIGIT (used in repetition)
                while (isDigit(self.peek())) _ = self.advance();
                try self.addToken(.number);
            } else {
                try self.addToken(.invalid);
            }
        },
    }
}

// === Primitive operations ===

/// Consume the current character and return it.
fn advance(self: *Scanner) u8 {
    const c = self.source[self.current];
    self.current += 1;
    return c;
}

/// Look at the current character without consuming it.
/// Returns 0 if at end.
fn peek(self: *Scanner) u8 {
    if (self.isAtEnd()) return 0;
    return self.source[self.current];
}

/// Look one character ahead (past current). Returns 0 if at end.
fn peekNext(self: *Scanner) u8 {
    if (self.current + 1 >= self.source.len) return 0;
    return self.source[self.current + 1];
}

/// Conditional advance: if current char matches `expected`, consume it
/// and return true. Otherwise return false.
fn match(self: *Scanner, expected: u8) bool {
    if (self.isAtEnd()) return false;
    if (self.source[self.current] != expected) return false;
    self.current += 1;
    return true;
}

/// Consume digits for a numeric value, including "." and "-" continuations.
/// e.g. for hex: "41" or "41.42.43" or "41-5A"
fn consumeDigits(self: *Scanner, isValidDigit: *const fn (u8) bool) void {
    // Consume first group of digits.
    while (isValidDigit(self.peek())) _ = self.advance();

    // Check for "." (concatenation) or "-" (range) continuation.
    if (self.peek() == '.') {
        // Dot-separated: %x41.42.43
        while (self.peek() == '.') {
            _ = self.advance(); // consume '.'
            while (isValidDigit(self.peek())) _ = self.advance();
        }
    } else if (self.peek() == '-') {
        // Range: %x41-5A
        _ = self.advance(); // consume '-'
        while (isValidDigit(self.peek())) _ = self.advance();
    }
}

fn isAtEnd(self: *Scanner) bool {
    return self.current >= self.source.len;
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isHexDigit(c: u8) bool {
    return isDigit(c) or (c >= 'A' and c <= 'F') or (c >= 'a' and c <= 'f');
}

fn isBit(c: u8) bool {
    return c == '0' or c == '1';
}

fn isAlpha(c: u8) bool {
    return (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z');
}

/// Append a token with the current lexeme span.
fn addToken(self: *Scanner, tag: Token.Tag) !void {
    try self.tokens.append(self.allocator, .{
        .tag = tag,
        .start = self.start,
        .len = self.current - self.start,
        .line = self.line,
    });
}

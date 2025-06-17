const std = @import("std");

const token = @import("token");
const parser = @import("parser");

const Token = token.Token;
const TokenType = token.TokenType;

pub const Lexer = struct {
    input: []const u8,
    position: u8,
    next_position: u8,
    current_char: u8,

    pub fn New(allocator: std.mem.Allocator, program: []const u8) !*Lexer {
        const lexer = try allocator.create(Lexer);
        lexer.* = Lexer{ .input = program, .position = 0, .next_position = 1, .current_char = program[0] };
        return lexer;
    }

    pub fn NextToken(self: *Lexer, allocator: std.mem.Allocator) !Token {
        var next_token: Token = undefined;
        self.skipWhiteSpace();

        switch (self.current_char) {
            '=' => {
                if (self.peakChar() == '=') {
                    self.readChar();
                    next_token = try newToken(TokenType.EQ, "==", allocator);
                } else {
                    next_token = try newToken(TokenType.ASSIGN, &[_]u8{self.current_char}, allocator);
                }
            },
            ';' => {
                next_token = try newToken(TokenType.SEMICOLON, &[_]u8{self.current_char}, allocator);
            },
            ':' => {
                next_token = try newToken(TokenType.COLON, &[_]u8{self.current_char}, allocator);
            },
            '(' => {
                next_token = try newToken(TokenType.LPAREN, &[_]u8{self.current_char}, allocator);
            },
            ')' => {
                next_token = try newToken(TokenType.RPAREN, &[_]u8{self.current_char}, allocator);
            },
            ',' => {
                next_token = try newToken(TokenType.COMMA, &[_]u8{self.current_char}, allocator);
            },
            '!' => {
                if (self.peakChar() == '=') {
                    self.readChar();
                    next_token = try newToken(TokenType.NOT_EQ, "!=", allocator);
                } else {
                    next_token = try newToken(TokenType.BANG, &[_]u8{self.current_char}, allocator);
                }
            },
            '{' => {
                next_token = try newToken(TokenType.LBRACE, &[_]u8{self.current_char}, allocator);
            },
            '}' => {
                next_token = try newToken(TokenType.RBRACE, &[_]u8{self.current_char}, allocator);
            },
            '+' => {
                next_token = try newToken(TokenType.PLUS, &[_]u8{self.current_char}, allocator);
            },
            '-' => {
                next_token = try newToken(TokenType.MINUS, &[_]u8{self.current_char}, allocator);
            },
            '/' => {
                next_token = try newToken(TokenType.SLASH, &[_]u8{self.current_char}, allocator);
            },
            '*' => {
                next_token = try newToken(TokenType.ASTERISK, &[_]u8{self.current_char}, allocator);
            },
            '<' => {
                next_token = try newToken(TokenType.LT, &[_]u8{self.current_char}, allocator);
            },
            '>' => {
                next_token = try newToken(TokenType.GT, &[_]u8{self.current_char}, allocator);
            },

            0 => {
                next_token = try newToken(TokenType.EOF, "", allocator);
            },
            else => {
                if (isLetter(self.current_char)) {
                    const literal: []const u8 = self.readIdentifier();
                    const token_type = token.lookUpIdentifier(literal);
                    next_token = try newToken(token_type, literal, allocator);
                    return next_token;
                } else if (isDigit(self.current_char)) {
                    return try self.readNumber(allocator);
                } else {
                    next_token = try newToken(TokenType.ILLEGAL, &[_]u8{self.current_char}, allocator);
                }
            },
        }

        self.readChar();

        return next_token;
    }

    fn readNumber(self: *Lexer, allocator: std.mem.Allocator) !Token {
        const start_position: u8 = self.*.position;
        while (isDigit(self.current_char)) {
            self.readChar();
        }
        if (self.current_char == '.') {
            return self.readFloatNumber(start_position, allocator);
        }
        return try newToken(TokenType.INT_VALUE, self.input[start_position..self.position], allocator);
    }

    fn readFloatNumber(self: *Lexer, start_position: u8, allocator: std.mem.Allocator) !Token {
        self.readChar();
        while (isDigit(self.current_char) or self.current_char == '.') {
            self.readChar();
        }
        return try newToken(TokenType.FLOAT_VALUE, self.input[start_position..self.position], allocator);
    }

    fn peakChar(self: *Lexer) u8 {
        if (self.next_position >= self.input.len) {
            return 0;
        } else {
            return self.input[self.next_position];
        }
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        const postion = self.position;
        while (isLetter(self.current_char)) {
            self.readChar();
        }
        return self.input[postion..self.position];
    }

    fn readChar(self: *Lexer) void {
        if (self.next_position >= self.input.len) {
            self.current_char = 0;
        } else {
            self.current_char = self.input[self.next_position];
        }
        self.position = self.next_position;
        self.next_position += 1;
    }

    fn skipWhiteSpace(self: *Lexer) void {
        while (self.current_char == ' ' or self.current_char == '\t' or self.current_char == '\n' or self.current_char == '\r') {
            self.readChar();
        }
    }
};

fn isLetter(ch: u8) bool {
    return ('a' <= ch and ch <= 'z') or ('A' <= ch and ch <= 'Z') or ch == '_';
}

fn isDigit(ch: u8) bool {
    return ('0' <= ch and ch <= '9');
}

fn newToken(token_type: TokenType, current_char: []const u8, allocator: std.mem.Allocator) !Token {
    const literal = try allocator.dupe(u8, current_char);
    return Token{ .Type = token_type, .Literal = literal };
}

const std = @import("std");

const token = @import("token");
const Token = token.Token;

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

    pub fn NextToken(self: *Lexer) Token {
        var next_token: Token = undefined;
        self.skipWhiteSpace();

        switch (self.current_char) {
            '=' => {
                if (self.peakChar() == '=') {
                    self.readChar();
                    next_token = newToken(token.EQ, "==");
                } else {
                    next_token = newToken(token.ASSIGN, &[_]u8{self.current_char});
                }
            },
            ';' => {
                next_token = newToken(token.SEMICOLON, &[_]u8{self.current_char});
            },
            '(' => {
                next_token = newToken(token.LPAREN, &[_]u8{self.current_char});
            },
            ')' => {
                next_token = newToken(token.RPAREN, &[_]u8{self.current_char});
            },
            ',' => {
                next_token = newToken(token.COMMA, &[_]u8{self.current_char});
            },
            '!' => {
                if (self.peakChar() == '=') {
                    self.readChar();
                    next_token = newToken(token.NOT_EQ, "!=");
                } else {
                    next_token = newToken(token.BANG, &[_]u8{self.current_char});
                }
            },
            '{' => {
                next_token = newToken(token.LBRACE, &[_]u8{self.current_char});
            },
            '}' => {
                next_token = newToken(token.RBRACE, &[_]u8{self.current_char});
            },
            '+' => {
                next_token = newToken(token.PLUS, &[_]u8{self.current_char});
            },
            '-' => {
                next_token = newToken(token.MINUS, &[_]u8{self.current_char});
            },
            '/' => {
                next_token = newToken(token.SLASH, &[_]u8{self.current_char});
            },
            '*' => {
                next_token = newToken(token.ASTERISK, &[_]u8{self.current_char});
            },
            '<' => {
                next_token = newToken(token.LT, &[_]u8{self.current_char});
            },
            '>' => {
                next_token = newToken(token.GT, &[_]u8{self.current_char});
            },

            0 => {
                next_token = newToken(token.EOF, "");
            },
            else => {
                if (isLetter(self.current_char)) {
                    const literal: []const u8 = self.readIdentifier();
                    const token_type = token.lookUpIdentifier(literal);
                    next_token = newToken(token_type, literal);
                    return next_token;
                } else if (isDigit(self.current_char)) {
                    const literal: []const u8 = self.readNumber();
                    return newToken(token.INT, literal);
                } else {
                    next_token = newToken(token.ILLEGAL, &[_]u8{self.current_char});
                }
            },
        }

        self.readChar();

        return next_token;
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

    fn readNumber(self: *Lexer) []const u8 {
        const position: u8 = self.*.position;
        while (isDigit(self.current_char)) {
            self.readChar();
        }
        return self.input[position..self.position];
    }
};

fn isLetter(ch: u8) bool {
    return ('a' <= ch and ch >= 'z') or ('A' <= ch and ch >= 'Z') or ch == '_';
}

fn isDigit(ch: u8) bool {
    return ('0' <= ch and ch <= '9');
}

fn newToken(token_type: []const u8, current_char: []const u8) Token {
    return Token{ .Type = token_type, .Literal = current_char };
}

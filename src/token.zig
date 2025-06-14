const std = @import("std");

pub const TokenType = enum {
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    ASSIGN,
    EQ,
    NOT_EQ,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    RETURN,
    TRUE,
    FALSE,
    IF,
    ELSE,

    pub fn toString(token_type: TokenType) []const u8 {
        return switch (token_type) {
            .ILLEGAL => "ILLEGAL",
            .EOF => "EOF",
            .IDENT => "IDENT",
            .INT => "INT",
            .ASSIGN => "=",
            .EQ => "==",
            .NOT_EQ => "!=",
            .PLUS => "+",
            .MINUS => "-",
            .BANG => "!",
            .ASTERISK => "*",
            .SLASH => "/",
            .LT => "<",
            .GT => ">",
            .COMMA => ",",
            .SEMICOLON => ";",
            .LPAREN => "(",
            .RPAREN => ")",
            .LBRACE => "{",
            .RBRACE => "}",
            .FUNCTION => "FUNCTION",
            .LET => "LET",
            .RETURN => "RETURN",
            .TRUE => "TRUE",
            .FALSE => "FALSE",
            .IF => "if",
            .ELSE => "else",
        };
    }
};

pub const Token = struct {
    Type: TokenType,
    Literal: []const u8,
};

pub const Keyword = struct {
    Value: []const u8,
    TokenType: TokenType,
};

pub const keywords = [_]Keyword{
    Keyword{ .TokenType = TokenType.FUNCTION, .Value = "func" },
    Keyword{ .TokenType = TokenType.LET, .Value = "let" },
    Keyword{ .TokenType = TokenType.RETURN, .Value = "return" },
    Keyword{ .TokenType = TokenType.TRUE, .Value = "true" },
    Keyword{ .TokenType = TokenType.FALSE, .Value = "false" },
    Keyword{ .TokenType = TokenType.IF, .Value = "if" },
    Keyword{ .TokenType = TokenType.ELSE, .Value = "else" },
};

pub fn lookUpIdentifier(identifier: []const u8) TokenType {
    for (keywords) |keyword| {
        if (std.mem.eql(u8, keyword.Value, identifier)) {
            return keyword.TokenType;
        }
    }
    return TokenType.IDENT;
}

const std = @import("std");

pub const TokenType = enum {
    ILLEGAL,
    EOF,
    IDENT,
    INT_VALUE,
    FLOAT_VALUE,
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
    COLON,
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
    INT_TYPE,
    FLOAT_TYPE,
    BOOL_TYPE,
    STRING_TYPE,
    CHAR_TYPE,

    pub fn toString(token_type: TokenType) []const u8 {
        return switch (token_type) {
            .ILLEGAL => "ILLEGAL",
            .EOF => "EOF",
            .IDENT => "IDENT",
            .INT_VALUE => "INT_VALUE",
            .FLOAT_VALUE => "FLOAT_VALUE",
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
            .COLON => ":",
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
            .INT_TYPE => "int",
            .FLOAT_TYPE => "float",
            .BOOL_TYPE => "bool",
            .STRING_TYPE => "string",
            .CHAR_TYPE => "char",
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
    Keyword{ .TokenType = TokenType.INT_TYPE, .Value = "int" },
    Keyword{ .TokenType = TokenType.FLOAT_TYPE, .Value = "float" },
    Keyword{ .TokenType = TokenType.BOOL_TYPE, .Value = "bool" },
    Keyword{ .TokenType = TokenType.STRING_TYPE, .Value = "string" },
    Keyword{ .TokenType = TokenType.CHAR_TYPE, .Value = "char" },
};

pub fn lookUpIdentifier(identifier: []const u8) TokenType {
    for (keywords) |keyword| {
        if (std.mem.eql(u8, keyword.Value, identifier)) {
            return keyword.TokenType;
        }
    }
    return TokenType.IDENT;
}

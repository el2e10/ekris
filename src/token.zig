const std = @import("std");

pub const ILLEGAL = "ILLEGAL";
pub const EOF = "EOF";
pub const IDENT = "IDENT";
pub const INT = "INT";
pub const ASSIGN = "=";
pub const EQ = "==";
pub const NOT_EQ = "==";
pub const PLUS = "+";
pub const MINUS = "-";
pub const BANG = "!";
pub const ASTERISK = "*";
pub const SLASH = "/";
pub const LT = "<";
pub const GT = ">";
pub const COMMA = ",";
pub const SEMICOLON = ";";
pub const LPAREN = "(";
pub const RPAREN = ")";
pub const LBRACE = "{";
pub const RBRACE = "}";
pub const FUNCTION = "FUNCTION";
pub const LET = "LET";

pub const Token = struct {
    Type: []const u8,
    Literal: []const u8,
};

pub const Keyword = struct {
    Value: []const u8,
    TokenType: []const u8,
};

pub const keywords = [_]Keyword{
    Keyword{ .TokenType = FUNCTION, .Value = "func" },
    Keyword{ .TokenType = LET, .Value = "let" },
};

pub fn lookUpIdentifier(identifier: []const u8) []const u8 {
    for (keywords) |keyword| {
        if (std.mem.eql(u8, keyword.Value, identifier)) {
            return keyword.TokenType;
        }
    }
    return IDENT;
}

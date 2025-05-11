const std = @import("std");
const token = @import("token");
const lexer = @import("lexer.zig");

const Lexer = lexer.Lexer;

const Token = token.Token;
const TokenType = token.TokenType;

fn _verifyTokens(expected_tokens: []const Token, l: *Lexer) void {
    var index: u8 = 0;
    var current_token: TokenType = l.*.NextToken().Type;
    while (current_token != TokenType.EOF) : ({
        current_token = l.*.NextToken().Type;
        index += 1;
    }) {
        std.testing.expectEqual(current_token, expected_tokens[index].Type) catch {
            std.debug.print("Index -> {d} Expected {s} got {any}\n", .{ index, expected_tokens[index].Type.toString(), current_token });
        };
    }
}

test "one" {
    const program: []const u8 = "=+(){},;";

    const allocator = std.testing.allocator;

    const expected_tokens = [_]Token{
        Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
        Token{ .Type = TokenType.PLUS, .Literal = "+" },
        Token{ .Type = TokenType.LPAREN, .Literal = "(" },
        Token{ .Type = TokenType.RPAREN, .Literal = ")" },
        Token{ .Type = TokenType.LBRACE, .Literal = "{" },
        Token{ .Type = TokenType.RBRACE, .Literal = "}" },
        Token{ .Type = TokenType.COMMA, .Literal = "," },
        Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        Token{ .Type = TokenType.EOF, .Literal = "\"" },
    };
    const l: *Lexer = try Lexer.New(allocator, program);
    defer allocator.destroy(l);

    _verifyTokens(&expected_tokens, l);
}

test "two" {
    const allocator = std.testing.allocator;
    const program: []const u8 =
        \\let five = 5;
        \\ let ten = 10;
        \\ let add = func(x, y) {
        \\ x + y;
        \\ };
        \\ let result = add(five, ten);
    ;
    const l: *Lexer = try Lexer.New(allocator, program);
    defer allocator.destroy(l);

    const expected_tokens = [_]Token{
        Token{ .Type = TokenType.LET, .Literal = "let" },
        Token{ .Type = TokenType.IDENT, .Literal = "five" },
        Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
        Token{ .Type = TokenType.INT, .Literal = "5" },
        Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        Token{ .Type = TokenType.LET, .Literal = "let" },
        Token{ .Type = TokenType.IDENT, .Literal = "ten" },
        Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
        Token{ .Type = TokenType.INT, .Literal = "10" },
        Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        Token{ .Type = TokenType.LET, .Literal = "let" },
        Token{ .Type = TokenType.IDENT, .Literal = "add" },
        Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
        Token{ .Type = TokenType.FUNCTION, .Literal = "fn" },
        Token{ .Type = TokenType.LPAREN, .Literal = "(" },
        Token{ .Type = TokenType.IDENT, .Literal = "x" },
        Token{ .Type = TokenType.COMMA, .Literal = "," },
        Token{ .Type = TokenType.IDENT, .Literal = "y" },
        Token{ .Type = TokenType.RPAREN, .Literal = ")" },
        Token{ .Type = TokenType.LBRACE, .Literal = "{" },
        Token{ .Type = TokenType.IDENT, .Literal = "x" },
        Token{ .Type = TokenType.PLUS, .Literal = "+" },
        Token{ .Type = TokenType.IDENT, .Literal = "y" },
        Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        Token{ .Type = TokenType.RBRACE, .Literal = "}" },
        Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        Token{ .Type = TokenType.LET, .Literal = "let" },
        Token{ .Type = TokenType.IDENT, .Literal = "result" },
        Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
        Token{ .Type = TokenType.IDENT, .Literal = "add" },
        Token{ .Type = TokenType.LPAREN, .Literal = "(" },
        Token{ .Type = TokenType.IDENT, .Literal = "five" },
        Token{ .Type = TokenType.COMMA, .Literal = "," },
        Token{ .Type = TokenType.IDENT, .Literal = "ten" },
        Token{ .Type = TokenType.RPAREN, .Literal = ")" },
        Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        Token{ .Type = TokenType.EOF, .Literal = "" },
    };

    _verifyTokens(&expected_tokens, l);
}

test "three" {
    const program: []const u8 =
        \\let five = 5;
        \\let ten = 10;
        \\let add = func(x, y) {
        \\x + y;
        \\};
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\10 == 10;
        \\5 != 10;
    ;
    const allocator = std.testing.allocator;

    const expected_tokens = [_]Token{
        Token{ .Type = TokenType.LET, .Literal = "let" },
        Token{ .Type = TokenType.IDENT, .Literal = "five" },
        Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
        Token{ .Type = TokenType.INT, .Literal = "5" },
        Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        Token{ .Type = TokenType.LET, .Literal = "let" },
        Token{ .Type = TokenType.IDENT, .Literal = "ten" },
        Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
        Token{ .Type = TokenType.INT, .Literal = "10" },
        Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        Token{ .Type = TokenType.LET, .Literal = "let" },
        Token{ .Type = TokenType.IDENT, .Literal = "add" },
        Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
        Token{ .Type = TokenType.FUNCTION, .Literal = "func" },
        Token{ .Type = TokenType.LPAREN, .Literal = "(" },
        Token{ .Type = TokenType.IDENT, .Literal = "x" },
        Token{ .Type = TokenType.COMMA, .Literal = "," },
        Token{ .Type = TokenType.IDENT, .Literal = "y" },
        Token{ .Type = TokenType.RPAREN, .Literal = ")" },
        Token{ .Type = TokenType.LBRACE, .Literal = "{" },
        Token{ .Type = TokenType.IDENT, .Literal = "x" },
        Token{ .Type = TokenType.PLUS, .Literal = "+" },
        Token{ .Type = TokenType.IDENT, .Literal = "y" },
        Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        Token{ .Type = TokenType.RBRACE, .Literal = "}" },
        Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        Token{ .Type = TokenType.LET, .Literal = "let" },
        Token{ .Type = TokenType.IDENT, .Literal = "result" },
        Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
        Token{ .Type = TokenType.IDENT, .Literal = "add" },
        Token{ .Type = TokenType.LPAREN, .Literal = "(" },
        Token{ .Type = TokenType.IDENT, .Literal = "five" },
        Token{ .Type = TokenType.COMMA, .Literal = "," },
        Token{ .Type = TokenType.IDENT, .Literal = "ten" },
        Token{ .Type = TokenType.RPAREN, .Literal = ")" },
        Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        Token{ .Type = TokenType.BANG, .Literal = "!" },
        Token{ .Type = TokenType.MINUS, .Literal = "-" },
        Token{ .Type = TokenType.SLASH, .Literal = "/" },
        Token{ .Type = TokenType.ASTERISK, .Literal = "*" },
        Token{ .Type = TokenType.INT, .Literal = "5" },
        Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        Token{ .Type = TokenType.INT, .Literal = "5" },
        Token{ .Type = TokenType.LT, .Literal = "<" },
        Token{ .Type = TokenType.INT, .Literal = "10" },
        Token{ .Type = TokenType.GT, .Literal = ">" },
        Token{ .Type = TokenType.INT, .Literal = "5" },
        Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        Token{ .Type = TokenType.INT, .Literal = "10" },
        Token{ .Type = TokenType.EQ, .Literal = "==" },
        Token{ .Type = TokenType.INT, .Literal = "10" },
        Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        Token{ .Type = TokenType.INT, .Literal = "5" },
        Token{ .Type = TokenType.NOT_EQ, .Literal = "!=" },
        Token{ .Type = TokenType.INT, .Literal = "10" },
        Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
    };
    const l: *Lexer = try Lexer.New(allocator, program);
    defer allocator.destroy(l);

    _verifyTokens(&expected_tokens, l);
}

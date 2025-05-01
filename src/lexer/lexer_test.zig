const std = @import("std");
const token = @import("token");
const lexer = @import("lexer.zig");

const Lexer = lexer.Lexer;

const Token = token.Token;

fn _verifyTokens(expected_tokens: []const Token, l: *Lexer) void {
    var index: u8 = 0;
    var current_token: []const u8 = l.*.NextToken().Type;
    while (!std.mem.eql(u8, current_token, token.EOF)) : ({
        current_token = l.*.NextToken().Type;
        index += 1;
    }) {
        std.testing.expectEqual(current_token, expected_tokens[index].Type) catch {
            std.debug.print("Expected {s} got {s}\n", .{ expected_tokens[index].Type, current_token });
        };
    }
}

test "one" {
    const program: []const u8 = "=+(){},;";

    const allocator = std.testing.allocator;

    const expected_tokens = [_]Token{
        Token{ .Type = token.ASSIGN, .Literal = "=" },
        Token{ .Type = token.PLUS, .Literal = "+" },
        Token{ .Type = token.LPAREN, .Literal = "(" },
        Token{ .Type = token.RPAREN, .Literal = ")" },
        Token{ .Type = token.LBRACE, .Literal = "{" },
        Token{ .Type = token.RBRACE, .Literal = "}" },
        Token{ .Type = token.COMMA, .Literal = "," },
        Token{ .Type = token.SEMICOLON, .Literal = ";" },
        Token{ .Type = token.EOF, .Literal = "\"" },
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
        Token{ .Type = token.LET, .Literal = "let" },
        Token{ .Type = token.IDENT, .Literal = "five" },
        Token{ .Type = token.ASSIGN, .Literal = "=" },
        Token{ .Type = token.INT, .Literal = "5" },
        Token{ .Type = token.SEMICOLON, .Literal = ";" },
        Token{ .Type = token.LET, .Literal = "let" },
        Token{ .Type = token.IDENT, .Literal = "ten" },
        Token{ .Type = token.ASSIGN, .Literal = "=" },
        Token{ .Type = token.INT, .Literal = "10" },
        Token{ .Type = token.SEMICOLON, .Literal = ";" },
        Token{ .Type = token.LET, .Literal = "let" },
        Token{ .Type = token.IDENT, .Literal = "add" },
        Token{ .Type = token.ASSIGN, .Literal = "=" },
        Token{ .Type = token.FUNCTION, .Literal = "fn" },
        Token{ .Type = token.LPAREN, .Literal = "(" },
        Token{ .Type = token.IDENT, .Literal = "x" },
        Token{ .Type = token.COMMA, .Literal = "," },
        Token{ .Type = token.IDENT, .Literal = "y" },
        Token{ .Type = token.RPAREN, .Literal = ")" },
        Token{ .Type = token.LBRACE, .Literal = "{" },
        Token{ .Type = token.IDENT, .Literal = "x" },
        Token{ .Type = token.PLUS, .Literal = "+" },
        Token{ .Type = token.IDENT, .Literal = "y" },
        Token{ .Type = token.SEMICOLON, .Literal = ";" },
        Token{ .Type = token.RBRACE, .Literal = "}" },
        Token{ .Type = token.SEMICOLON, .Literal = ";" },
        Token{ .Type = token.LET, .Literal = "let" },
        Token{ .Type = token.IDENT, .Literal = "result" },
        Token{ .Type = token.ASSIGN, .Literal = "=" },
        Token{ .Type = token.IDENT, .Literal = "add" },
        Token{ .Type = token.LPAREN, .Literal = "(" },
        Token{ .Type = token.IDENT, .Literal = "five" },
        Token{ .Type = token.COMMA, .Literal = "," },
        Token{ .Type = token.IDENT, .Literal = "ten" },
        Token{ .Type = token.RPAREN, .Literal = ")" },
        Token{ .Type = token.SEMICOLON, .Literal = ";" },
        Token{ .Type = token.EOF, .Literal = "" },
    };

    _verifyTokens(&expected_tokens, l);
}

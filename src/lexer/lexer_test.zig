const std = @import("std");
const token = @import("token");
const lexer = @import("lexer.zig");

const Lexer = lexer.Lexer;

const Token = token.Token;
const TokenType = token.TokenType;

fn _verifyTokens(expected_tokens: []const Token, l: *Lexer, allocator: std.mem.Allocator) !void {
    var index: u8 = 0;
    var current_token: Token = try l.*.NextToken(allocator);
    while (current_token.Type != TokenType.EOF) : ({
        current_token = try l.*.NextToken(allocator);
        index += 1;
    }) {
        defer allocator.free(current_token.Literal);
        std.testing.expectEqual(expected_tokens[index].Type, current_token.Type) catch {
            std.debug.print("Index -> {d} Expected {s} got {s}\n", .{ index, expected_tokens[index].Type.toString(), current_token.Type.toString() });
        };
        std.testing.expectEqualStrings(expected_tokens[index].Literal, current_token.Literal) catch {
            std.debug.print("Index -> {d} Expected literal {s} got {s}\n", .{ index, expected_tokens[index].Literal, current_token.Literal });
            break;
        };
    }
}

// test "one" {
//     const program: []const u8 = "=+(){},;:";
//
//     const allocator = std.testing.allocator;
//
//     const expected_tokens = [_]Token{
//         Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
//         Token{ .Type = TokenType.PLUS, .Literal = "+" },
//         Token{ .Type = TokenType.LPAREN, .Literal = "(" },
//         Token{ .Type = TokenType.RPAREN, .Literal = ")" },
//         Token{ .Type = TokenType.LBRACE, .Literal = "{" },
//         Token{ .Type = TokenType.RBRACE, .Literal = "}" },
//         Token{ .Type = TokenType.COMMA, .Literal = "," },
//         Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
//         Token{ .Type = TokenType.COLON, .Literal = ":" },
//         Token{ .Type = TokenType.EOF, .Literal = "\"" },
//     };
//     const l: *Lexer = try Lexer.New(allocator, program);
//     defer allocator.destroy(l);
//
//     try _verifyTokens(&expected_tokens, l, allocator);
// }
//
// test "two" {
//     const allocator = std.testing.allocator;
//     const program: []const u8 =
//         \\let five: int = 5;
//         \\ let ten: float = 10.0;
//         \\ func add(x: int, y: float) float {
//         \\ x + y;
//         \\ }
//         \\ let result: float = add(five, ten);
//     ;
//     const l: *Lexer = try Lexer.New(allocator, program);
//     defer allocator.destroy(l);
//
//     const expected_tokens = [_]Token{
//         Token{ .Type = TokenType.LET, .Literal = "let" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "five" },
//         Token{ .Type = TokenType.COLON, .Literal = ":" },
//         Token{ .Type = TokenType.INT_TYPE, .Literal = "int" },
//         Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
//         Token{ .Type = TokenType.INT_VALUE, .Literal = "5" },
//         Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
//         Token{ .Type = TokenType.LET, .Literal = "let" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "ten" },
//         Token{ .Type = TokenType.COLON, .Literal = ":" },
//         Token{ .Type = TokenType.FLOAT_TYPE, .Literal = "float" },
//         Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
//         Token{ .Type = TokenType.FLOAT_VALUE, .Literal = "10.0" },
//         Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
//         Token{ .Type = TokenType.FUNCTION, .Literal = "func" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "add" },
//         Token{ .Type = TokenType.LPAREN, .Literal = "(" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "x" },
//         Token{ .Type = TokenType.COLON, .Literal = ":" },
//         Token{ .Type = TokenType.INT_TYPE, .Literal = "int" },
//         Token{ .Type = TokenType.COMMA, .Literal = "," },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "y" },
//         Token{ .Type = TokenType.COLON, .Literal = ":" },
//         Token{ .Type = TokenType.FLOAT_TYPE, .Literal = "float" },
//         Token{ .Type = TokenType.RPAREN, .Literal = ")" },
//         Token{ .Type = TokenType.FLOAT_TYPE, .Literal = "float" },
//         Token{ .Type = TokenType.LBRACE, .Literal = "{" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "x" },
//         Token{ .Type = TokenType.PLUS, .Literal = "+" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "y" },
//         Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
//         Token{ .Type = TokenType.RBRACE, .Literal = "}" },
//         Token{ .Type = TokenType.LET, .Literal = "let" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "result" },
//         Token{ .Type = TokenType.COLON, .Literal = ":" },
//         Token{ .Type = TokenType.FLOAT_TYPE, .Literal = "float" },
//         Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "add" },
//         Token{ .Type = TokenType.LPAREN, .Literal = "(" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "five" },
//         Token{ .Type = TokenType.COMMA, .Literal = "," },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "ten" },
//         Token{ .Type = TokenType.RPAREN, .Literal = ")" },
//         Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
//         Token{ .Type = TokenType.EOF, .Literal = "" },
//     };
//
//     try _verifyTokens(&expected_tokens, l, allocator);
// }
//
// test "three" {
//     const program: []const u8 =
//         \\let five: int = 5;
//         \\let ten:float = 1.83;
//         \\func add(x: int, y: float) float {
//         \\return x + y;
//         \\}
//         \\let result: float = add(five, ten);
//         \\!-/*5;
//         \\5 < 10 > 5;
//         \\10 == 10;
//         \\5 != 10;
//     ;
//     const allocator = std.testing.allocator;
//
//     const expected_tokens = [_]Token{
//         Token{ .Type = TokenType.LET, .Literal = "let" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "five" },
//         Token{ .Type = TokenType.COLON, .Literal = ":" },
//         Token{ .Type = TokenType.INT_TYPE, .Literal = "int" },
//         Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
//         Token{ .Type = TokenType.INT_VALUE, .Literal = "5" },
//         Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
//         Token{ .Type = TokenType.LET, .Literal = "let" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "ten" },
//         Token{ .Type = TokenType.COLON, .Literal = ":" },
//         Token{ .Type = TokenType.FLOAT_TYPE, .Literal = "float" },
//         Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
//         Token{ .Type = TokenType.FLOAT_VALUE, .Literal = "1.83" },
//         Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
//         Token{ .Type = TokenType.FUNCTION, .Literal = "func" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "add" },
//         Token{ .Type = TokenType.LPAREN, .Literal = "(" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "x" },
//         Token{ .Type = TokenType.COLON, .Literal = ":" },
//         Token{ .Type = TokenType.INT_TYPE, .Literal = "int" },
//         Token{ .Type = TokenType.COMMA, .Literal = "," },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "y" },
//         Token{ .Type = TokenType.COLON, .Literal = ":" },
//         Token{ .Type = TokenType.FLOAT_TYPE, .Literal = "float" },
//         Token{ .Type = TokenType.RPAREN, .Literal = ")" },
//         Token{ .Type = TokenType.FLOAT_TYPE, .Literal = "float" },
//         Token{ .Type = TokenType.LBRACE, .Literal = "{" },
//         Token{ .Type = TokenType.RETURN, .Literal = "return" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "x" },
//         Token{ .Type = TokenType.PLUS, .Literal = "+" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "y" },
//         Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
//         Token{ .Type = TokenType.RBRACE, .Literal = "}" },
//         Token{ .Type = TokenType.LET, .Literal = "let" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "result" },
//         Token{ .Type = TokenType.COLON, .Literal = ":" },
//         Token{ .Type = TokenType.FLOAT_TYPE, .Literal = "float" },
//         Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "add" },
//         Token{ .Type = TokenType.LPAREN, .Literal = "(" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "five" },
//         Token{ .Type = TokenType.COMMA, .Literal = "," },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "ten" },
//         Token{ .Type = TokenType.RPAREN, .Literal = ")" },
//         Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
//         Token{ .Type = TokenType.BANG, .Literal = "!" },
//         Token{ .Type = TokenType.MINUS, .Literal = "-" },
//         Token{ .Type = TokenType.SLASH, .Literal = "/" },
//         Token{ .Type = TokenType.ASTERISK, .Literal = "*" },
//         Token{ .Type = TokenType.INT_VALUE, .Literal = "5" },
//         Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
//         Token{ .Type = TokenType.INT_VALUE, .Literal = "5" },
//         Token{ .Type = TokenType.LT, .Literal = "<" },
//         Token{ .Type = TokenType.INT_VALUE, .Literal = "10" },
//         Token{ .Type = TokenType.GT, .Literal = ">" },
//         Token{ .Type = TokenType.INT_VALUE, .Literal = "5" },
//         Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
//         Token{ .Type = TokenType.INT_VALUE, .Literal = "10" },
//         Token{ .Type = TokenType.EQ, .Literal = "==" },
//         Token{ .Type = TokenType.INT_VALUE, .Literal = "10" },
//         Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
//         Token{ .Type = TokenType.INT_VALUE, .Literal = "5" },
//         Token{ .Type = TokenType.NOT_EQ, .Literal = "!=" },
//         Token{ .Type = TokenType.INT_VALUE, .Literal = "10" },
//         Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
//     };
//     const l: *Lexer = try Lexer.New(allocator, program);
//     defer allocator.destroy(l);
//
//     try _verifyTokens(&expected_tokens, l, allocator);
// }
//
// test "four" {
//     const allocator = std.testing.allocator;
//     const program: []const u8 =
//         \\x: int = 53424;
//         \\y: bool = true;
//         \\if(x){
//         \\   x = x + 10;
//         \\} else {
//         \\   x= x + 37;
//         \\}
//     ;
//
//     const expected_token = [_]Token{
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "x" },
//         Token{ .Type = TokenType.COLON, .Literal = ":" },
//         Token{ .Type = TokenType.INT_TYPE, .Literal = "int" },
//         Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
//         Token{ .Type = TokenType.INT_VALUE, .Literal = "53424" },
//         Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "y" },
//         Token{ .Type = TokenType.COLON, .Literal = ":" },
//         Token{ .Type = TokenType.BOOL_TYPE, .Literal = "bool" },
//         Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
//         Token{ .Type = TokenType.TRUE, .Literal = "true" },
//         Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
//         Token{ .Type = TokenType.IF, .Literal = "if" },
//         Token{ .Type = TokenType.LPAREN, .Literal = "(" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "x" },
//         Token{ .Type = TokenType.RPAREN, .Literal = ")" },
//         Token{ .Type = TokenType.LBRACE, .Literal = "{" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "x" },
//         Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "x" },
//         Token{ .Type = TokenType.PLUS, .Literal = "+" },
//         Token{ .Type = TokenType.INT_VALUE, .Literal = "10" },
//         Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
//         Token{ .Type = TokenType.RBRACE, .Literal = "}" },
//         Token{ .Type = TokenType.ELSE, .Literal = "else" },
//         Token{ .Type = TokenType.LBRACE, .Literal = "{" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "x" },
//         Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
//         Token{ .Type = TokenType.IDENTIFIER, .Literal = "x" },
//         Token{ .Type = TokenType.PLUS, .Literal = "+" },
//         Token{ .Type = TokenType.INT_VALUE, .Literal = "37" },
//         Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
//         Token{ .Type = TokenType.RBRACE, .Literal = "}" },
//     };
//
//     const l: *Lexer = try Lexer.New(allocator, program);
//     defer allocator.destroy(l);
//
//     try _verifyTokens(&expected_token, l, allocator);
// }

test "five" {
    const allocator = std.testing.allocator;
    const program: []const u8 =
        \\if(x){
        \\   x = x + 10;
        \\} else{
        \\   x= x + 37;
        \\}
    ;

    const expected_token = [_]Token{
        Token{ .Type = TokenType.IF, .Literal = "if" },
        Token{ .Type = TokenType.LPAREN, .Literal = "(" },
        Token{ .Type = TokenType.IDENTIFIER, .Literal = "x" },
        Token{ .Type = TokenType.RPAREN, .Literal = ")" },
        Token{ .Type = TokenType.LBRACE, .Literal = "{" },
        Token{ .Type = TokenType.IDENTIFIER, .Literal = "x" },
        Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
        Token{ .Type = TokenType.IDENTIFIER, .Literal = "x" },
        Token{ .Type = TokenType.PLUS, .Literal = "+" },
        Token{ .Type = TokenType.INT_VALUE, .Literal = "10" },
        Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        Token{ .Type = TokenType.RBRACE, .Literal = "}" },
        Token{ .Type = TokenType.ELSE, .Literal = "else" },
        Token{ .Type = TokenType.LBRACE, .Literal = "{" },
        Token{ .Type = TokenType.IDENTIFIER, .Literal = "x" },
        Token{ .Type = TokenType.ASSIGN, .Literal = "=" },
        Token{ .Type = TokenType.IDENTIFIER, .Literal = "x" },
        Token{ .Type = TokenType.PLUS, .Literal = "+" },
        Token{ .Type = TokenType.INT_VALUE, .Literal = "37" },
        Token{ .Type = TokenType.SEMICOLON, .Literal = ";" },
        Token{ .Type = TokenType.RBRACE, .Literal = "}" },
    };

    const l: *Lexer = try Lexer.New(allocator, program);
    defer allocator.destroy(l);

    try _verifyTokens(&expected_token, l, allocator);
}

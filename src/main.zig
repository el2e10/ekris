const std = @import("std");
const parser = @import("parser");
const lexer = @import("lexer");
const ast = @import("ast");
const token = @import("token");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const test_allocator = gpa.allocator();
    const input: []const u8 =
        \\ !5;
    ;
    const lxr: *lexer.Lexer = try lexer.Lexer.New(test_allocator, input);
    defer test_allocator.destroy(lxr);

    const prsr: *parser.Parser = try parser.Parser.New(test_allocator, lxr);
    defer prsr.deinit(test_allocator);

    const program: *ast.Program = try prsr.ParseProgram(test_allocator);
    defer program.deinit(test_allocator);

    // try std.testing.expect(checkParserErrors(prsr));

    if (program.*.statements.len != 1) {
        std.debug.print("program doesn't contain 1 statements", .{});
        return;
    }

    for (program.*.statements) |stmt| {
        const string_rep = try stmt.string(test_allocator);
        std.debug.print("Identifier is {s}", .{string_rep});
        test_allocator.free(string_rep);
    }

    //
    // const stdIn = std.io.getStdIn();
    // const out = std.io.getStdOut().writer();
    //
    // var buffered_reader = std.io.bufferedReader(stdIn.reader());
    //
    // const reader = buffered_reader.reader();
    //
    // var buffer: [1000]u8 = undefined;
    // @memset(buffer[0..], 0);
    //
    // while (true) {
    //     try out.print(">> ", .{});
    //     _ = try reader.readUntilDelimiterOrEof(buffer[0..], '\n') orelse break;
    //     const lex: *lexer.Lexer = try lexer.Lexer.New(allocator, buffer[0..]);
    //     defer allocator.destroy(lex);
    //
    //     var current_token = lex.*.NextToken();
    //     while (!std.mem.eql(u8, current_token.Type.toString(), token.TokenType.EOF.toString())) : (current_token = lex.*.NextToken()) {
    //         try out.print("<< {s} {s}\n", .{ current_token.Type.toString(), current_token.Literal });
    //     }
    // }
}

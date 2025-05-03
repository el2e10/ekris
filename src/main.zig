const std = @import("std");
const lexer = @import("lexer");
const token = @import("token");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const stdIn = std.io.getStdIn();
    const out = std.io.getStdOut().writer();

    var buffered_reader = std.io.bufferedReader(stdIn.reader());

    const reader = buffered_reader.reader();

    var buffer: [1000]u8 = undefined;
    @memset(buffer[0..], 0);

    while (true) {
        try out.print(">> ", .{});
        _ = try reader.readUntilDelimiterOrEof(buffer[0..], '\n') orelse break;
        const lex: *lexer.Lexer = try lexer.Lexer.New(allocator, buffer[0..]);
        defer allocator.destroy(lex);

        var current_token = lex.*.NextToken();
        while (!std.mem.eql(u8, current_token.Type, token.EOF)) : (current_token = lex.*.NextToken()) {
            try out.print("<< {s} {s}\n", .{ current_token.Type, current_token.Literal });
        }
    }
}

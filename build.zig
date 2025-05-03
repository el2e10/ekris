const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    // We will also create a module for our other entry point, 'main.zig'.
    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const token_module = b.addModule("token", .{ .root_source_file = b.path("src/token.zig") });
    const lexer_module = b.addModule("lexer", .{ .root_source_file = b.path("src/lexer/lexer.zig") });

    // This creates another `std.Build.Step.Compile`, but this one builds an executable
    // rather than a static library.
    const exe = b.addExecutable(.{
        .name = "ekris",
        .root_module = exe_mod,
    });

    exe_mod.addImport("token", token_module);
    exe_mod.addImport("lexer", lexer_module);

    lexer_module.addImport("token", token_module);

    token_module.addImport("lexer", lexer_module);

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // -- TESTING --

    const test_module = b.option([]const u8, "test-module", "Module to test(lexer)") orelse "all";

    const lexer_module_test = b.addTest(.{
        .name = "lexer_module_test",
        .root_source_file = b.path("src/lexer/lexer_test.zig"),
        .target = target,
        .optimize = optimize,
    });

    lexer_module_test.root_module.addImport("token", token_module);

    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");

    if (std.mem.eql(u8, test_module, "all")) {
        test_step.dependOn(&lexer_module_test.step);
    } else if (std.mem.eql(u8, test_module, "lexer")) {
        const run_unit_test = b.addRunArtifact(lexer_module_test);
        test_step.dependOn(&run_unit_test.step);
        test_step.dependOn(&lexer_module_test.step);
    } else {
        std.debug.print("Invalid --test-module {s} value: use 'lexer'", .{test_module});
    }
}

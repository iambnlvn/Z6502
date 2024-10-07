const std = @import("std");
const fs = std.fs;
const cpu = @import("Cpu.zig").CPU;

pub fn main() !void {
    var arenaInstance = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arenaInstance.deinit();

    var arena = arenaInstance.allocator();
    const prog = @embedFile("print.bin");
    var cpu6502 = try cpu.init(&arena);

    defer cpu6502.bus.deinit();
    @memcpy(cpu6502.bus.sysMem.data, prog);

    // try dumpVirtualMemory(&cpu6502);
    cpu6502.reset();

    while (true) {
        cpu6502.clock();
    }
}
fn dumpVirtualMemory(proc: *cpu) !void {
    var cwd = fs.cwd();

    var f = try cwd.createFile("dump.bin", .{});
    defer f.close();

    try f.writeAll(proc.bus.sysMem.data);

    // for printing memory
    // for (0..0x10000) |i| {
    //     std.debug.print("{x}", .{proc.bus.sysMem.data[i]});
    // }
}

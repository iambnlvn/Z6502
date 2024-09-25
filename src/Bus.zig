const std = @import("std");
const Ram = @import("mem.zig").Ram;
pub const Bus = struct {
    const Self = @This();
    allocator: *std.mem.Allocator,
    sysMem: Ram,
    rom: []const u8 = undefined,

    pub fn init(allocator: *std.mem.Allocator, romData: ?[]const u8) !Bus {
        var r: []const u8 = undefined;
        if (romData) |rd| {
            r = rd;
        }
        return .{
            .allocator = allocator,
            .sysMem = try Ram.init(allocator, 0, 0xFFFF),
            .rom = r,
        };
    }

    pub fn deinit(self: Self) void {
        self.sysMem.deinit();
    }

    pub fn write(self: *Self, addr: u16, value: u8) void {
        //for debugging
        if (addr == 0xF0) std.debug.print("{c}", .{value});

        self.sysMem.write(addr, value);
    }

    pub fn read(self: *Self, addr: u16) u8 {
        return switch (addr) {
            //TODO: Implement memory mapping for other devices
            0x0000...0xFFFF => self.sysMem.read(addr),

            //Todo: Implement memory mapping for ROM
            // 0xC000...0xFFFF => self.readRomFromAddr(addr),
        };
    }

    // pub fn readRomFromAddr(self: *Self, addr: u16) u8 {
    //     const romStart = 0xC000;
    //     const romIndex = addr - romStart;
    //     std.debug.print("reading from rom\n", .{});
    //     if (romIndex < self.rom.len) {
    //         return self.rom[romIndex];
    //     } else {
    //         std.debug.panic("ROM address out of bounds\n ROM LENGTH: {any}", .{self.rom.len});
    //         return 0;
    //     }
    // }
};

test "init and deinit Bus" {
    var allocator = std.testing.allocator;
    var bus = try Bus.init(&allocator, null);

    try std.testing.expect(bus.sysMem.data.len == 0x10000);
    try std.testing.expect(bus.sysMem.startOffset == 0);
    try std.testing.expect(bus.sysMem.endOffset == 0xFFFF);

    // Deinitialize the bus
    bus.deinit();

    // Verify that sysMem is deinitialized (no specific check here, just ensuring no crash)
}

test "read from Bus" {
    var allocator = std.testing.allocator;
    var bus = try Bus.init(&allocator, &[_]u8{0x01});

    defer bus.deinit();

    // Write a value to a specific address
    bus.write(4000, 42);

    // Read the value from the same address
    const value = bus.read(4000);
    try std.testing.expect(value == 42);
}

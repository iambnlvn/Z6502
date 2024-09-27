const std = @import("std");

pub const Ram = struct {
    const Self = @This();
    allocator: *std.mem.Allocator,
    startOffset: u16,
    endOffset: u16,
    length: u32 = undefined,
    data: []u8, // Z6502  has only 64KB of addressable memory

    pub fn init(allocator: *std.mem.Allocator, startOffset: u16, endOffset: u16) !Ram {
        if (startOffset > endOffset) {
            std.debug.panic("startOffset must be less than or equal to endOffset", .{});
            return 0;
        }
        const len = @as(u32, @intCast(endOffset - startOffset)) + 1;
        return Ram{
            .allocator = allocator,
            .startOffset = startOffset,
            .endOffset = endOffset,
            .length = len,
            .data = try allocator.*.alloc(u8, len),
        };
    }
    pub fn deinit(self: Self) void {
        self.allocator.free(self.data);
    }

    pub fn read(self: *Self, addr: u16) u8 {
        if (addr < self.startOffset or addr > self.endOffset) {
            std.debug.panic("Address out of bounds", .{});
        }
        return self.data[@as(u32, @intCast(addr - self.startOffset))];
    }

    pub fn write(self: *Self, address: u16, value: u8) void {
        if (address < self.startOffset or address > self.endOffset) {
            std.debug.panic("Address out of bounds", .{});
        }

        self.data[address - self.startOffset] = value;
    }

    pub fn load(self: *Self, rom: []const u8) void {
        if (rom.len > self.length) {
            std.debug.panic("ROM too big for RAM", .{});
        }
        for (rom, 0..) |byte, i| {
            self.data[i] = byte;
        }
    }
};

test "read and write" {
    var allocator = std.testing.allocator;
    var ram = try Ram.init(&allocator, 100, 1024);

    defer ram.deinit();
    ram.write(201, 10);
    const actual = ram.read(201);
    try std.testing.expectEqual(10, actual);
}

test "load" {
    var allocator = std.testing.allocator;
    var ram = try Ram.init(&allocator, 132, 1024);

    defer ram.deinit();
    const rom = &[_]u8{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    ram.load(rom);
    for (rom, 0..) |byte, i| {
        const actual = ram.read(132 + @as(u16, @intCast(i)));
        try std.testing.expectEqual(byte, actual);
    }
}

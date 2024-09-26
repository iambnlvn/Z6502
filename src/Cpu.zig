const std = @import("std");
const Bus = @import("Bus.zig").Bus;

const StatusRegister = packed struct {
    C: bool, //c arry flag
    Z: bool, // zero flag
    I: bool, // interrupt disable flag
    D: bool, // decimal mode
    B: bool, // break command
    U: bool, // unused
    V: bool, // overflow flag
    N: bool, // negative flag
};

const CPU = struct {
    bus: Bus = undefined,
    clockCount: u128,
    pc: u16,
    acc: u8,
    xReg: u8,
    yReg: u8,
    sp: u8,
    absAddr: u16,
    relAddr: u16,
    opCode: u8,
    lastFetch: u8,
    cycles: u8,
    statusReg: StatusRegister,

    pub fn init(allocator: *std.mem.Allocator) !CPU {
        return CPU{
            .bus = try Bus.init(allocator, null),
            .statusReg = .{
                .C = false,
                .Z = false,
                .I = false,
                .D = false,
                .B = false,
                .U = false,
                .V = false,
                .N = false,
            },
            .clockCount = 0,
            .pc = 0xFFFC,
            .acc = 0,
            .xReg = 0,
            .yReg = 0,
            .sp = 0,
            .absAddr = 0,
            .relAddr = 0,
            .opCode = 0,
            .lastFetch = 0,
            .cycles = 0,
        };
    }

    pub fn reset(self: *CPU) void {
        self.absAddr = 0xFFFC;
        const low: u16 = @as(u16, @intCast(self.read(self.absAddr + 1)));
        const high: u16 = @as(u16, @intCast(self.read(self.absAddr))) << 8;
        self.pc = high | low;

        self.acc = 0;
        self.pc = 0xFFFC;
        self.sp = 0x01FF;
        self.statusReg.U = true;
    }
    pub fn clock(self: *CPU) void {
        if (self.cycles == 0) {
            self.opCode = self.read(self.pc);
            self.statusReg.U = true;
            self.pc = @addWithOverflow(self.pc, 1)[0];
        }
    }

    pub fn read(self: *CPU, addr: u16) u8 {
        return self.bus.read(addr);
    }

    pub fn write(self: *CPU, addr: u16, value: u8) void {
        self.bus.write(addr, value);
    }

    pub fn irq(self: *CPU) void {
        if (!self.statusReg.I) {
            self.write(0x0100 + @as(u16, @intCast(self.sp)), @truncate(self.pc >> 8));
            self.sp = @subWithOverflow(self.sp, 1)[0];

            self.write(0x0100 + @as(u16, @intCast(self.sp)), @truncate(self.pc));
            self.sp = @subWithOverflow(self.sp, 1)[0];

            self.statusReg.I = true;
            self.statusReg.U = true;
            self.statusReg.B = false;

            self.write(0x0100 + @as(u16, @intCast(self.sp)), @bitCast(self.statusReg));
            self.sp = @subWithOverflow(self.sp, 1)[0];
            self.absAddr = 0xFFFE;

            const low: u16 = self.read(self.absAddr);
            const high: u16 = @as(u16, @intCast(self.read(self.absAddr + 1))) << 8;
            self.pc = high | low;

            self.cycles = 7;
        } else return;
    }

    pub fn nmi(self: *CPU) void {
        self.write(0x0100 + @as(u16, @intCast(self.sp)), @truncate(self.pc >> 8));
        self.sp = @subWithOverflow(self.sp, 1)[0];

        self.write(0x0100 + @as(u16, @intCast(self.sp)), @truncate(self.pc));
        self.sp = @subWithOverflow(self.sp, 1)[0];

        self.statusReg.I = true;
        self.statusReg.U = true;
        self.statusReg.B = false;

        self.write(0x0100 + @as(u16, @intCast(self.sp)), @bitCast(self.statusReg));
        self.sp = @subWithOverflow(self.sp, 1)[0];

        self.absAddr = 0xFFFA;
        const low: u16 = self.read(self.absAddr);
        const high: u16 = @as(u16, @intCast(self.read(self.absAddr + 1))) << 8;

        self.pc = high | low;
        self.cycles = 8;
    }
};

test "cpu read" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();
    cpu.bus.write(0x00A, 0x011);
    try std.testing.expectEqual(cpu.read(0x00A), 0x011);

    // cpu.bus.write(0xFFFD, 0x80); // this one will fail since the ROM memory mapping is not implemented
    // try std.testing.expect(cpu.read(0xFFFD) == 0x80);
}

test "cpu write" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();
    cpu.write(0x00A, 0x011);
    try std.testing.expectEqual(cpu.bus.read(0x00A), 0x011);
}

test "irq" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    // Set initial state
    cpu.pc = 0x1234;
    cpu.sp = 0xFF;

    // Write IRQ vector
    cpu.write(0xFFFE, 0x00);
    cpu.write(0xFFFF, 0x80);

    cpu.irq();

    // Check status register flags
    try std.testing.expectEqual(cpu.statusReg.I, true);
    try std.testing.expectEqual(cpu.statusReg.U, true);
    try std.testing.expectEqual(cpu.statusReg.B, false);

    // Check program counter
    try std.testing.expectEqual(cpu.pc, 0x8000);

    // Check stack pointer
    try std.testing.expectEqual(cpu.sp, 0xFC);

    // Check stack contents
    try std.testing.expectEqual(cpu.read(0x01FD), 36); // High byte of PC
    try std.testing.expectEqual(cpu.read(0x01FC), 170); // Low byte of PC
    try std.testing.expectEqual(cpu.read(0x01FB), 170); // Status register
}

test "nmi" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    // Set initial state
    cpu.pc = 0x1234;
    cpu.sp = 0xFF;
    cpu.statusReg.I = false;
    cpu.statusReg.U = false;
    cpu.statusReg.B = true;

    // Mock read values for the interrupt vector
    cpu.write(0xFFFA, 0x56);
    cpu.write(0xFFFB, 0x78);

    cpu.nmi();

    // Check stack writes
    try std.testing.expectEqual(cpu.read(0x0100 + 0xFF), 0x12);
    try std.testing.expectEqual(cpu.read(0x0100 + 0xFE), 0x34);
    try std.testing.expectEqual(cpu.read(0x0100 + 0xFD), @as(u8, @bitCast(cpu.statusReg)));

    // Check status register
    try std.testing.expectEqual(cpu.statusReg.I, true);
    try std.testing.expectEqual(cpu.statusReg.U, true);
    try std.testing.expectEqual(cpu.statusReg.B, false);

    try std.testing.expectEqual(cpu.pc, 0x7856);

    try std.testing.expectEqual(cpu.cycles, 8);
}

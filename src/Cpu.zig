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

    pub fn IMP(self: *CPU) u8 {
        self.lastFetch = self.acc;
        return 0;
    }

    pub fn IMM(self: *CPU) u8 {
        self.absAddr = self.pc;
        self.pc = @addWithOverflow(self.pc, 1)[0];
        return 0;
    }

    pub fn ZP0(self: *CPU) u8 {
        self.absAddr = self.read(self.pc);
        self.pc = @addWithOverflow(self.pc, 1)[0];
        self.absAddr = self.absAddr & 0x00FF;
        return 0;
    }

    pub fn ZPX(self: *CPU) u8 {
        self.absAddr = @addWithOverflow(self.read(self.pc), self.xReg)[0];
        self.pc = @addWithOverflow(self.pc, 1)[0];
        self.absAddr = self.absAddr & 0x00FF;
        return 0;
    }

    pub fn ZPY(self: *CPU) u8 {
        self.absAddr = @addWithOverflow(self.read(self.pc), self.yReg)[0];
        self.pc = @addWithOverflow(self.pc, 1)[0];
        self.absAddr = self.absAddr & 0x00FF;
        return 0;
    }

    pub fn ABS(self: *CPU) u8 {
        const low: u16 = self.read(self.pc);
        self.pc = @addWithOverflow(self.pc, 1)[0];

        const high: u16 = @as(u16, @intCast(self.read(self.pc))) << 8;
        self.pc = @addWithOverflow(self.pc, 1)[0];

        self.absAddr = high | low;

        return 0;
    }

    pub fn ABX(self: *CPU) u8 {
        const low: u16 = self.read(self.pc);
        self.pc = @addWithOverflow(self.pc, 1)[0];

        const high: u16 = @as(u16, @intCast(self.read(self.pc))) << 8;
        self.pc = @addWithOverflow(self.pc, 1)[0];

        self.absAddr = high | low;

        self.absAddr = @addWithOverflow(self.absAddr, self.xReg)[0];
        if (self.absAddr & 0xFF00 != high) return 1;

        return 0;
    }

    pub fn ABY(self: *CPU) u8 {
        const low: u16 = self.read(self.pc);
        self.pc = @addWithOverflow(self.pc, 1)[0];

        const high: u16 = @as(u16, @intCast(self.read(self.pc))) << 8;
        self.pc = @addWithOverflow(self.pc, 1)[0];

        self.absAddr = high | low;

        self.absAddr = @addWithOverflow(self.absAddr, self.yReg)[0];
        if (self.absAddr & 0xFF00 != high) return 1;

        return 0;
    }

    pub fn IND(self: *CPU) u8 {
        const low: u16 = self.read(self.pc);
        self.pc = @addWithOverflow(self.pc, 1)[0];

        const high: u16 = @as(u16, @intCast(self.read(self.pc))) << 8;
        self.pc = @addWithOverflow(self.pc, 1)[0];

        const ptr: u16 = high | low;

        if (low == 0x00FF) {
            self.absAddr = (@as(u16, self.read(ptr & 0xff00)) << 8) | self.read(ptr);
            return 0;
        }

        self.absAddr = (@as(u16, self.read(ptr + 1)) << 8) | self.read(ptr);
        return 0;
    }

    pub fn IZX(self: *CPU) u8 {
        const tmp: u16 = self.read(self.pc);
        self.pc = @addWithOverflow(self.pc, 1)[0];

        const low: u16 = self.read((tmp + self.xReg) & 0x00FF);
        const high: u16 = self.read((tmp + self.xReg + 1) & 0x00FF);

        self.absAddr = (high << 8) | low;
        return 0;
    }

    pub fn IZY(self: *CPU) u8 {
        const tmp: u16 = self.read(self.pc);
        self.pc = @addWithOverflow(self.pc, 1)[0];

        const low: u16 = self.read(tmp & 0x00FF);
        const high: u16 = self.read((tmp + 1) & 0x00FF);

        self.absAddr = (high << 8) | low;
        self.absAddr = @addWithOverflow(self.absAddr, self.yReg)[0];

        if (self.absAddr & 0xFF00 != high << 8) return 1;
        return 0;
    }

    pub fn REL(self: *CPU) u8 {
        self.relAddr = self.read(self.pc);
        self.pc = @addWithOverflow(self.pc, 1)[0];

        if (self.relAddr & 0x80 != 0) self.relAddr |= 0xFF00 else self.relAddr &= 0x00FF;

        return 0;
    }

    pub fn BCC(self: *CPU) u8 {
        if (self.statusReg.C) return 0;

        self.cycles += 1;

        self.absAddr = @addWithOverflow(self.pc, self.relAddr)[0];

        if (self.absAddr & 0xFF00 != self.pc & 0xFF00) self.cycles += 1;

        self.pc = self.absAddr;
        return 0;
    }

    pub fn BCS(self: *CPU) u8 {
        if (!self.statusReg.C) return 0;

        self.cycles += 1;
        self.absAddr = @addWithOverflow(self.pc, self.relAddr)[0];

        if (self.absAddr & 0xFF00 != self.pc & 0xFF00) self.cycles += 1;
        self.pc = self.absAddr;
        return 0;
    }

    pub fn BEQ(self: *CPU) u8 {
        if (!self.statusReg.Z) return 0;

        self.cycles += 1;
        self.absAddr = @addWithOverflow(self.pc, self.relAddr)[0];

        if (self.absAddr & 0xFF00 != self.pc & 0xFF00) self.cycles += 1;
        self.pc = self.absAddr;
        return 0;
    }

    pub fn BMI(self: *CPU) u8 {
        if (!self.statusReg.N) return 0;

        self.cycles += 1;
        self.absAddr = @addWithOverflow(self.pc, self.relAddr)[0];

        if (self.absAddr & 0xFF00 != self.pc & 0xFF00) self.cycles += 1;
        self.pc = self.absAddr;
        return 0;
    }

    pub fn BPL(self: *CPU) u8 {
        if (self.statusReg.N) return 0;

        self.cycles += 1;
        self.absAddr = @addWithOverflow(self.pc, self.relAddr)[0];

        if (self.absAddr & 0xFF00 != self.pc & 0xFF00) self.cycles += 1;
        self.pc = self.absAddr;
        return 0;
    }

    pub fn BNE(self: *CPU) u8 {
        if (self.statusReg.Z) return 0;

        self.cycles += 1;
        self.absAddr = @addWithOverflow(self.pc, self.relAddr)[0];

        if (self.absAddr & 0xFF00 != self.pc & 0xFF00) self.cycles += 1;
        self.pc = self.absAddr;
        return 0;
    }

    pub fn BVC(self: *CPU) u8 {
        if (self.statusReg.V) return 0;

        self.cycles += 1;
        self.absAddr = @addWithOverflow(self.pc, self.relAddr)[0];

        if (self.absAddr & 0xFF00 != self.pc & 0xFF00) self.cycles += 1;
        self.pc = self.absAddr;
        return 0;
    }
    pub fn BVS(self: *CPU) u8 {
        if (!self.statusReg.V) return 0;

        self.cycles += 1;
        self.absAddr = @addWithOverflow(self.pc, self.relAddr)[0];

        if (self.absAddr & 0xFF00 != self.pc & 0xFF00) self.cycles += 1;
        self.pc = self.absAddr;
        return 0;
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

test "IMP" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.acc = 0x12;
    try std.testing.expectEqual(cpu.IMP(), 0);
    try std.testing.expectEqual(cpu.lastFetch, 0x12);
}

test "IMM" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    try std.testing.expectEqual(cpu.IMM(), 0);
    try std.testing.expectEqual(cpu.absAddr, 0x1234);
    try std.testing.expectEqual(cpu.pc, 0x1235);
}

test "ZP0" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    cpu.bus.write(0x1234, 0x56);
    try std.testing.expectEqual(cpu.ZP0(), 0);
    try std.testing.expectEqual(cpu.absAddr, 0x56);
    try std.testing.expectEqual(cpu.pc, 0x1235);
}

test "ZPX" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    cpu.xReg = 0x12;
    cpu.bus.write(0x1234, 0x34);
    try std.testing.expectEqual(cpu.ZPX(), 0);
    try std.testing.expectEqual(cpu.absAddr, 0x46);
    try std.testing.expectEqual(cpu.pc, 0x1235);
}

test "ZPY" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    cpu.yReg = 0x12;
    cpu.bus.write(0x1234, 0x34);
    try std.testing.expectEqual(cpu.ZPY(), 0);
    try std.testing.expectEqual(cpu.absAddr, 0x46);
    try std.testing.expectEqual(cpu.pc, 0x1235);
}

test "ABS" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    cpu.bus.write(0x1234, 0x56);
    cpu.bus.write(0x1235, 0x78);
    try std.testing.expectEqual(cpu.ABS(), 0);
    try std.testing.expectEqual(cpu.absAddr, 0x7856);
    try std.testing.expectEqual(cpu.pc, 0x1236);
}

test "ABX" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    cpu.xReg = 0x12;
    cpu.bus.write(0x1234, 0x56);
    cpu.bus.write(0x1235, 0x78);
    try std.testing.expectEqual(cpu.ABX(), 0);
    try std.testing.expectEqual(cpu.absAddr, 0x7868);
    try std.testing.expectEqual(cpu.pc, 0x1236);
}

test "ABY" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    cpu.yReg = 0x12;
    cpu.bus.write(0x1234, 0x56);
    cpu.bus.write(0x1235, 0x78);
    try std.testing.expectEqual(cpu.ABY(), 0);
    try std.testing.expectEqual(cpu.absAddr, 0x7868);
    try std.testing.expectEqual(cpu.pc, 0x1236);
}
test "IND" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    cpu.bus.write(0x1234, 0x56);
    cpu.bus.write(0x1235, 0x78);
    cpu.bus.write(0x7856, 0x12);
    cpu.bus.write(0x7857, 0x34);
    try std.testing.expectEqual(cpu.IND(), 0);
    try std.testing.expectEqual(cpu.absAddr, 0x3412);
    try std.testing.expectEqual(cpu.pc, 0x1236);
}

test "IZX" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    cpu.xReg = 0x12;
    cpu.bus.write(0x1234, 0x56);
    cpu.bus.write(0x0068, 0x12);
    cpu.bus.write(0x0069, 0x34);
    try std.testing.expectEqual(cpu.IZX(), 0);
    try std.testing.expectEqual(cpu.absAddr, 0x3412);
    try std.testing.expectEqual(cpu.pc, 0x1235);
}

test "cpu IZY" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1000;

    cpu.bus.write(0x1000, 0x20);
    cpu.bus.write(0x0020, 0x34);
    cpu.bus.write(0x0021, 0x12);

    cpu.yReg = 0x10;

    try std.testing.expectEqual(cpu.IZY(), 0);
    try std.testing.expectEqual(cpu.absAddr, 0x1234 + 0x10);
    try std.testing.expectEqual(cpu.pc, 0x1001);
}

test "REL" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    cpu.bus.write(0x1234, 0x56);
    try std.testing.expectEqual(cpu.REL(), 0);
    try std.testing.expectEqual(cpu.relAddr, 0x56);
    try std.testing.expectEqual(cpu.pc, 0x1235);
}

test "BCC" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    cpu.statusReg.C = false;
    cpu.relAddr = 0x12;
    cpu.bus.write(0x1234, 0x12);
    try std.testing.expectEqual(cpu.BCC(), 0);
    try std.testing.expectEqual(cpu.pc, 0x1246);
    try std.testing.expectEqual(cpu.cycles, 1);
}

test "BCS" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    cpu.statusReg.C = true;
    cpu.relAddr = 0x12;
    cpu.bus.write(0x1234, 0x12);
    try std.testing.expectEqual(cpu.BCS(), 0);
    try std.testing.expectEqual(cpu.pc, 0x1246);
    try std.testing.expectEqual(cpu.cycles, 1);
}

test "BEQ" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    cpu.statusReg.Z = true;
    cpu.relAddr = 0x12;
    cpu.bus.write(0x1234, 0x12);
    try std.testing.expectEqual(cpu.BEQ(), 0);
    try std.testing.expectEqual(cpu.pc, 0x1246);
    try std.testing.expectEqual(cpu.cycles, 1);
}

test "BMI" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    cpu.statusReg.N = true;
    cpu.relAddr = 0x12;
    cpu.bus.write(0x1234, 0x12);
    try std.testing.expectEqual(cpu.BMI(), 0);
    try std.testing.expectEqual(cpu.pc, 0x1246);
    try std.testing.expectEqual(cpu.cycles, 1);
}

test "BPL" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    cpu.statusReg.N = false;
    cpu.relAddr = 0x12;
    cpu.bus.write(0x1234, 0x12);
    try std.testing.expectEqual(cpu.BPL(), 0);
    try std.testing.expectEqual(cpu.pc, 0x1246);
    try std.testing.expectEqual(cpu.cycles, 1);
}

test "BNE" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    cpu.statusReg.Z = false;
    cpu.relAddr = 0x12;
    cpu.bus.write(0x1234, 0x12);
    try std.testing.expectEqual(cpu.BNE(), 0);
    try std.testing.expectEqual(cpu.pc, 0x1246);
    try std.testing.expectEqual(cpu.cycles, 1);
}

test "BVC" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    cpu.statusReg.V = false;
    cpu.relAddr = 0x12;
    cpu.bus.write(0x1234, 0x12);
    try std.testing.expectEqual(cpu.BVC(), 0);
    try std.testing.expectEqual(cpu.pc, 0x1246);
    try std.testing.expectEqual(cpu.cycles, 1);
}

test "BVS" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    cpu.statusReg.V = true;
    cpu.relAddr = 0x12;
    cpu.bus.write(0x1234, 0x12);
    try std.testing.expectEqual(cpu.BVS(), 0);
    try std.testing.expectEqual(cpu.pc, 0x1246);
    try std.testing.expectEqual(cpu.cycles, 1);
}

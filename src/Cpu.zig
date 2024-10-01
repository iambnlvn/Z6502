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
        self.xReg = 0;
        self.yReg = 0;
        self.absAddr = 0;
        self.relAddr = 0;
        self.lastFetch = 0;

        self.sp = 0xFC;
        self.statusReg.U = true;
        self.cycles = 8;
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

    pub fn BRK(self: *CPU) u8 {
        self.pc = @addWithOverflow(self.pc, 1)[0];
        self.statusReg.I = true;

        self.write(0x0100 + @as(u16, self.sp), @truncate(self.pc >> 8));
        self.sp = @subWithOverflow(self.sp, 1)[0];

        self.write(0x0100 + @as(u16, self.sp), @truncate(self.pc));
        self.sp = @subWithOverflow(self.sp, 1)[0];
        self.statusReg.B = true;

        self.write(0x0100 + @as(u16, self.sp), @bitCast(self.statusReg));
        self.sp = @subWithOverflow(self.sp, 1)[0];
        self.statusReg.B = false;

        self.pc = (@as(u16, self.read(0xFFFF)) << 8) | self.read(0xFFFE);
        std.process.exit(0);
        return 0;
    }

    pub fn CLC(self: *CPU) u8 {
        self.statusReg.C = false;
        return 0;
    }

    pub fn CLD(self: *CPU) u8 {
        self.statusReg.D = false;
        return 0;
    }

    pub fn CLI(self: *CPU) u8 {
        self.statusReg.I = false;
        return 0;
    }

    pub fn CLV(self: *CPU) u8 {
        self.statusReg.V = false;
        return 0;
    }

    pub fn JMP(self: *CPU) u8 {
        self.pc = self.absAddr;
        return 0;
    }

    pub fn JSR(self: *CPU) u8 {
        self.pc = @subWithOverflow(self.pc, 1)[0];

        self.write(0x0100 + @as(u16, self.sp), @truncate(self.pc >> 8));
        self.sp = @subWithOverflow(self.sp, 1)[0];

        self.write(0x0100 + @as(u16, self.sp), @truncate(self.pc));
        self.sp = @subWithOverflow(self.sp, 1)[0];

        self.pc = self.absAddr;
        return 0;
    }

    pub fn RTS(self: *CPU) u8 {
        self.sp = @addWithOverflow(self.sp, 1)[0];
        self.pc = self.read(0x0100 + @as(u16, self.sp));
        self.sp = @addWithOverflow(self.sp, 1)[0];

        self.pc = self.pc | (@as(u16, self.read(0x0100 + @as(u16, self.sp))) << 8);

        self.pc = @addWithOverflow(self.pc, 1)[0];

        return 0;
    }

    pub fn NOP(self: *CPU) u8 {
        switch (self.opCode) {
            0x1C, 0x3C, 0x5C, 0x7C, 0xDC, 0xFC => return 1,
            else => return 0,
        }
    }

    pub fn PHA(self: *CPU) u8 {
        self.write(0x0100 + @as(u16, self.sp), self.acc);
        self.sp = @subWithOverflow(self.sp, 1)[0];
        return 0;
    }

    pub fn PHP(self: *CPU) u8 {
        self.statusReg.B = true;
        self.statusReg.U = true;

        self.write(0x0100 + @as(u16, self.sp), @bitCast(self.statusReg));
        self.sp = @subWithOverflow(self.sp, 1)[0];

        self.statusReg.B = false;
        self.statusReg.U = false;
        return 0;
    }

    pub fn PLA(self: *CPU) u8 {
        self.sp = @addWithOverflow(self.sp, 1)[0];
        self.acc = self.read(0x0100 + @as(u16, self.sp));

        self.statusReg.Z = self.acc == 0;
        self.statusReg.N = self.acc & 0x80 != 0;

        return 0;
    }

    pub fn PLP(self: *CPU) u8 {
        self.sp = @addWithOverflow(self.sp, 1)[0];
        self.statusReg = @bitCast(self.read(0x0100 + @as(u16, self.sp)));

        self.statusReg.U = true;
        return 0;
    }

    pub fn RTI(self: *CPU) u8 {
        self.sp = @addWithOverflow(self.sp, 1)[0];
        self.statusReg = @bitCast(self.read(0x0100 + @as(u16, self.sp)));

        self.statusReg.B = false;
        self.statusReg.U = false;

        self.sp = @addWithOverflow(self.sp, 1)[0];
        self.pc = self.read(0x0100 + @as(u16, self.sp));

        self.sp = @addWithOverflow(self.sp, 1)[0];
        self.pc |= @as(u16, self.read(0x0100 + @as(u16, self.sp))) << 8;

        return 0;
    }

    pub fn SEC(self: *CPU) u8 {
        self.statusReg.C = true;
        return 0;
    }

    pub fn SED(self: *CPU) u8 {
        self.statusReg.D = true;
        return 0;
    }

    pub fn SEI(self: *CPU) u8 {
        self.statusReg.I = true;
        return 0;
    }

    pub fn STA(self: *CPU) u8 {
        self.write(self.absAddr, self.acc);
        return 0;
    }

    pub fn STX(self: *CPU) u8 {
        self.write(self.absAddr, self.xReg);
        return 0;
    }

    pub fn STY(self: *CPU) u8 {
        self.write(self.absAddr, self.yReg);
        return 0;
    }

    pub fn TAX(self: *CPU) u8 {
        self.xReg = self.acc;
        self.statusReg.Z = self.xReg == 0;
        self.statusReg.N = self.xReg & 0x80 != 0;

        return 0;
    }

    pub fn TAY(self: *CPU) u8 {
        self.yReg = self.acc;
        self.statusReg.Z = self.yReg == 0;
        self.statusReg.N = self.yReg & 0x80 != 0;

        return 0;
    }

    pub fn TSX(self: *CPU) u8 {
        self.xReg = self.sp;
        self.statusReg.Z = self.xReg == 0;
        self.statusReg.N = self.xReg & 0x80 != 0;

        return 0;
    }

    pub fn TXS(self: *CPU) u8 {
        self.sp = self.xReg;
        return 0;
    }

    pub fn TXA(self: *CPU) u8 {
        self.acc = self.xReg;
        self.statusReg.Z = self.acc == 0;
        self.statusReg.N = self.acc & 0x80 != 0;
        return 0;
    }

    pub fn TYA(self: *CPU) u8 {
        self.acc = self.yReg;
        self.statusReg.Z = self.acc == 0;
        self.statusReg.N = self.acc & 0x80 != 0;
        return 0;
    }

    pub fn XXX(self: *CPU) u8 {
        _ = self;
        return 0;
    }

    pub fn fetch(self: *CPU) u8 {
        if (LOOKUP[self.opCode >> 4][self.opCode & 0x0F].AddrMode != &CPU.IMP) {
            self.lastFetch = self.read(self.absAddr);
        }
        std.debug.print("Fetch: {}\n", .{self.lastFetch});
        return self.lastFetch;
    }

    pub fn AND(self: *CPU) u8 {
        _ = self.fetch();
        self.acc &= self.lastFetch;
        self.statusReg.Z = self.acc == 0;
        self.statusReg.N = self.acc & 0x80 != 0;
        return 1;
    }
};

pub const Instruction = struct {
    Name: []const u8 = "XXX",
    Operator: *const fn (*CPU) u8 = &CPU.XXX,
    AddrMode: *const fn (*CPU) u8 = &CPU.IMP,
    Cycles: u8 = 1,
};

//TODO: This is the lookup table for all the instructions, should be 16*16 in size once all instructions are implemented
pub var LOOKUP: [16][4]Instruction =
    .{
    .{
        .{ .Name = "BRK", .Cycles = 7, .AddrMode = &CPU.IMP, .Operator = &CPU.BRK },
        // .{ .Name = "ORA", .Cycles = 6, .AddrMode = &CPU.IZX, .Operator = &CPU.ORA },
        // .{ .Name = "JAM", .Cycles = 1, .AddrMode = &CPU.IMP, .Operator = &CPU.JAM },
        // .{ .Name = "SLO", .Cycles = 8, .AddrMode = &CPU.IZX, .Operator = &CPU.SLO },
        .{ .Name = "NOP", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.NOP },
        // .{ .Name = "ORA", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.ORA },
        // .{ .Name = "ASL", .Cycles = 5, .AddrMode = &CPU.ZP0, .Operator = &CPU.ASL },
        // .{ .Name = "SLO", .Cycles = 5, .AddrMode = &CPU.ZP0, .Operator = &CPU.SLO },
        .{ .Name = "PHP", .Cycles = 3, .AddrMode = &CPU.IMP, .Operator = &CPU.PHP },
        // .{ .Name = "ORA", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.ORA },
        // .{ .Name = "ASL", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.ASL },
        // .{ .Name = "ANC", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.ANC },
        .{ .Name = "NOP", .Cycles = 4, .AddrMode = &CPU.IMP, .Operator = &CPU.ABS },
        // .{ .Name = "ORA", .Cycles = 4, .AddrMode = &CPU.ABS, .Operator = &CPU.ORA },
        // .{ .Name = "ASL", .Cycles = 6, .AddrMode = &CPU.ABS, .Operator = &CPU.ASL },
        // .{ .Name = "SLO", .Cycles = 6, .AddrMode = &CPU.ABS, .Operator = &CPU.SLO },
    },
    .{
        // .{ .Name = "BPL", .Cycles = 2, .AddrMode = &CPU.REL, .Operator = &CPU.BPL }, // Implemented
        // .{ .Name = "ORA", .Cycles = 5, .AddrMode = &CPU.IZY, .Operator = &CPU.ORA },
        // .{ .Name = "JAM", .Cycles = 1, .AddrMode = &CPU.IMP, .Operator = &CPU.JAM },
        // .{ .Name = "SLO", .Cycles = 8, .AddrMode = &CPU.IZY, .Operator = &CPU.SLO },
        .{ .Name = "NOP", .Cycles = 4, .AddrMode = &CPU.ZPX, .Operator = &CPU.NOP },
        // .{ .Name = "ORA", .Cycles = 4, .AddrMode = &CPU.ZPX, .Operator = &CPU.ORA },
        // .{ .Name = "ASL", .Cycles = 6, .AddrMode = &CPU.ZPX, .Operator = &CPU.ASL },
        // .{ .Name = "SLO", .Cycles = 6, .AddrMode = &CPU.ZPX, .Operator = &CPU.SLO },
        .{ .Name = "CLC", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.CLC },
        // .{ .Name = "ORA", .Cycles = 4, .AddrMode = &CPU.ABY, .Operator = &CPU.ORA },
        .{ .Name = "NOP", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.NOP },
        // .{ .Name = "SLO", .Cycles = 7, .AddrMode = &CPU.ABY, .Operator = &CPU.SLO },
        .{ .Name = "NOP", .Cycles = 4, .AddrMode = &CPU.IMP, .Operator = &CPU.ABX },
        // .{ .Name = "ORA", .Cycles = 4, .AddrMode = &CPU.ABX, .Operator = &CPU.ORA },
        // .{ .Name = "ASL", .Cycles = 7, .AddrMode = &CPU.ABX, .Operator = &CPU.ASL },
        // .{ .Name = "SLO", .Cycles = 7, .AddrMode = &CPU.ABX, .Operator = &CPU.SLO },
    },
    .{
        .{ .Name = "JSR", .Cycles = 6, .AddrMode = &CPU.ABS, .Operator = &CPU.JSR },
        // .{ .Name = "AND", .Cycles = 6, .AddrMode = &CPU.IZX, .Operator = &CPU.AND },
        // .{ .Name = "JAM", .Cycles = 1, .AddrMode = &CPU.IMP, .Operator = &CPU.JAM },
        // .{ .Name = "RLA", .Cycles = 8, .AddrMode = &CPU.IZX, .Operator = &CPU.RLA },
        // .{ .Name = "BIT", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.BIT },
        // .{ .Name = "AND", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.AND },
        // .{ .Name = "ROL", .Cycles = 5, .AddrMode = &CPU.ZP0, .Operator = &CPU.ROL },
        // .{ .Name = "RLA", .Cycles = 5, .AddrMode = &CPU.ZP0, .Operator = &CPU.RLA },
        .{ .Name = "PLP", .Cycles = 4, .AddrMode = &CPU.IMP, .Operator = &CPU.PLP },
        .{ .Name = "PLP", .Cycles = 4, .AddrMode = &CPU.IMP, .Operator = &CPU.PLP }, // dup
        .{ .Name = "PLP", .Cycles = 4, .AddrMode = &CPU.IMP, .Operator = &CPU.PLP }, //dup
        // .{ .Name = "AND", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.AND },
        // .{ .Name = "ROL", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.ROL },
        // .{ .Name = "ANC", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.ANC },
        // .{ .Name = "BIT", .Cycles = 4, .AddrMode = &CPU.ABS, .Operator = &CPU.BIT },
        // .{ .Name = "AND", .Cycles = 4, .AddrMode = &CPU.ABS, .Operator = &CPU.AND },
        // .{ .Name = "ROL", .Cycles = 6, .AddrMode = &CPU.ABS, .Operator = &CPU.ROL },
        // .{ .Name = "RLA", .Cycles = 6, .AddrMode = &CPU.ABS, .Operator = &CPU.RLA },
    },
    .{
        // .{ .Name = "BMI", .Cycles = 2, .AddrMode = &CPU.REL, .Operator = &CPU.BMI }, // Implemented
        // .{ .Name = "AND", .Cycles = 5, .AddrMode = &CPU.IZY, .Operator = &CPU.AND },
        // .{ .Name = "JAM", .Cycles = 1, .AddrMode = &CPU.IMP, .Operator = &CPU.JAM },
        // .{ .Name = "RLA", .Cycles = 8, .AddrMode = &CPU.IZY, .Operator = &CPU.RLA },
        .{ .Name = "NOP", .Cycles = 4, .AddrMode = &CPU.ZPX, .Operator = &CPU.NOP },
        // .{ .Name = "AND", .Cycles = 4, .AddrMode = &CPU.ZPX, .Operator = &CPU.AND },
        // .{ .Name = "ROL", .Cycles = 6, .AddrMode = &CPU.ZPX, .Operator = &CPU.ROL },
        // .{ .Name = "RLA", .Cycles = 6, .AddrMode = &CPU.ZPX, .Operator = &CPU.RLA },
        .{ .Name = "SEC", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.SEC },
        // .{ .Name = "AND", .Cycles = 4, .AddrMode = &CPU.ABY, .Operator = &CPU.AND },
        .{ .Name = "NOP", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.NOP },
        // .{ .Name = "RLA", .Cycles = 7, .AddrMode = &CPU.ABX, .Operator = &CPU.RLA },
        .{ .Name = "NOP", .Cycles = 4, .AddrMode = &CPU.IMP, .Operator = &CPU.ABX },
        // .{ .Name = "AND", .Cycles = 4, .AddrMode = &CPU.ABX, .Operator = &CPU.AND },
        // .{ .Name = "ROL", .Cycles = 7, .AddrMode = &CPU.ABX, .Operator = &CPU.ROL },
        // .{ .Name = "RLA", .Cycles = 7, .AddrMode = &CPU.ABX, .Operator = &CPU.RLA },
    },
    .{
        .{ .Name = "RTI", .Cycles = 6, .AddrMode = &CPU.IMP, .Operator = &CPU.RTI },
        // .{ .Name = "EOR", .Cycles = 6, .AddrMode = &CPU.IZX, .Operator = &CPU.EOR },
        // .{ .Name = "JAM", .Cycles = 1, .AddrMode = &CPU.IMP, .Operator = &CPU.JAM },
        // .{ .Name = "SRE", .Cycles = 8, .AddrMode = &CPU.IZX, .Operator = &CPU.SRE },
        .{ .Name = "NOP", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.NOP },
        // .{ .Name = "EOR", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.EOR },
        // .{ .Name = "LSR", .Cycles = 5, .AddrMode = &CPU.ZP0, .Operator = &CPU.LSR },
        // .{ .Name = "SRE", .Cycles = 5, .AddrMode = &CPU.ZP0, .Operator = &CPU.SRE },
        .{ .Name = "PHA", .Cycles = 3, .AddrMode = &CPU.IMP, .Operator = &CPU.PHA },
        // .{ .Name = "EOR", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.EOR },
        // .{ .Name = "LSR", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.LSR },
        // .{ .Name = "ALR", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.ALR },
        .{ .Name = "JMP", .Cycles = 3, .AddrMode = &CPU.ABS, .Operator = &CPU.JMP },
        // .{ .Name = "EOR", .Cycles = 4, .AddrMode = &CPU.ABS, .Operator = &CPU.EOR },
        // .{ .Name = "LSR", .Cycles = 6, .AddrMode = &CPU.ABS, .Operator = &CPU.LSR },
        // .{ .Name = "SRE", .Cycles = 6, .AddrMode = &CPU.ABS, .Operator = &CPU.SRE },
    },
    .{
        // .{ .Name = "BVC", .Cycles = 2, .AddrMode = &CPU.REL, .Operator = &CPU.BVC }, // implemented
        // .{ .Name = "EOR", .Cycles = 5, .AddrMode = &CPU.IZY, .Operator = &CPU.EOR },
        // .{ .Name = "JAM", .Cycles = 1, .AddrMode = &CPU.IMP, .Operator = &CPU.JAM },
        // .{ .Name = "SRE", .Cycles = 8, .AddrMode = &CPU.IZY, .Operator = &CPU.SRE },
        .{ .Name = "NOP", .Cycles = 4, .AddrMode = &CPU.ZPX, .Operator = &CPU.NOP },
        // .{ .Name = "EOR", .Cycles = 4, .AddrMode = &CPU.ZPX, .Operator = &CPU.EOR },
        // .{ .Name = "LSR", .Cycles = 6, .AddrMode = &CPU.ZPX, .Operator = &CPU.LSR },
        // .{ .Name = "SRE", .Cycles = 6, .AddrMode = &CPU.ZPX, .Operator = &CPU.SRE },
        .{ .Name = "CLI", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.CLI },
        // .{ .Name = "EOR", .Cycles = 4, .AddrMode = &CPU.ABY, .Operator = &CPU.EOR },
        .{ .Name = "NOP", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.NOP },
        // .{ .Name = "SRE", .Cycles = 7, .AddrMode = &CPU.ABY, .Operator = &CPU.SRE },
        .{ .Name = "NOP", .Cycles = 4, .AddrMode = &CPU.IMP, .Operator = &CPU.ABX },
        // .{ .Name = "EOR", .Cycles = 4, .AddrMode = &CPU.ABX, .Operator = &CPU.EOR },
        // .{ .Name = "LSR", .Cycles = 7, .AddrMode = &CPU.ABX, .Operator = &CPU.LSR },
        // .{ .Name = "SRE", .Cycles = 7, .AddrMode = &CPU.ABX, .Operator = &CPU.SRE },
    },
    .{
        .{ .Name = "RTS", .Cycles = 6, .AddrMode = &CPU.IMP, .Operator = &CPU.RTS },
        // .{ .Name = "ADC", .Cycles = 6, .AddrMode = &CPU.IZX, .Operator = &CPU.ADC },
        // .{ .Name = "JAM", .Cycles = 1, .AddrMode = &CPU.IMP, .Operator = &CPU.JAM },
        // .{ .Name = "RRA", .Cycles = 8, .AddrMode = &CPU.IZX, .Operator = &CPU.RRA },
        .{ .Name = "NOP", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.NOP },
        // .{ .Name = "ADC", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.ADC },
        // .{ .Name = "ROR", .Cycles = 5, .AddrMode = &CPU.ZP0, .Operator = &CPU.ROR },
        // .{ .Name = "RRA", .Cycles = 5, .AddrMode = &CPU.ZP0, .Operator = &CPU.RRA },
        .{ .Name = "PLA", .Cycles = 4, .AddrMode = &CPU.IMP, .Operator = &CPU.PLA },
        // .{ .Name = "ADC", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.ADC },
        // .{ .Name = "ROR", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.ROR },
        // .{ .Name = "ARR", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.ARR },
        .{ .Name = "JMP", .Cycles = 5, .AddrMode = &CPU.IND, .Operator = &CPU.JMP },
        // .{ .Name = "ADC", .Cycles = 4, .AddrMode = &CPU.ABS, .Operator = &CPU.ADC },
        // .{ .Name = "ROR", .Cycles = 6, .AddrMode = &CPU.ABS, .Operator = &CPU.ROR },
        // .{ .Name = "RRA", .Cycles = 6, .AddrMode = &CPU.ABS, .Operator = &CPU.RRA },
    },
    .{
        // .{ .Name = "BVS", .Cycles = 2, .AddrMode = &CPU.REL, .Operator = &CPU.BVS }, // implemented
        // .{ .Name = "ADC", .Cycles = 5, .AddrMode = &CPU.IZY, .Operator = &CPU.ADC },
        // .{ .Name = "JAM", .Cycles = 1, .AddrMode = &CPU.IMP, .Operator = &CPU.JAM },
        // .{ .Name = "RRA", .Cycles = 8, .AddrMode = &CPU.IZY, .Operator = &CPU.RRA },
        .{ .Name = "NOP", .Cycles = 4, .AddrMode = &CPU.ZPX, .Operator = &CPU.NOP },
        // .{ .Name = "ADC", .Cycles = 4, .AddrMode = &CPU.ZPX, .Operator = &CPU.ADC },
        // .{ .Name = "ROR", .Cycles = 6, .AddrMode = &CPU.ZPX, .Operator = &CPU.ROR },
        // .{ .Name = "RRA", .Cycles = 6, .AddrMode = &CPU.ZPX, .Operator = &CPU.RRA },
        .{ .Name = "SEI", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.SEI },
        // .{ .Name = "ADC", .Cycles = 4, .AddrMode = &CPU.ABY, .Operator = &CPU.ADC },
        .{ .Name = "NOP", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.NOP },
        // .{ .Name = "RRA", .Cycles = 7, .AddrMode = &CPU.ABY, .Operator = &CPU.RRA },
        .{ .Name = "NOP", .Cycles = 4, .AddrMode = &CPU.IMP, .Operator = &CPU.ABX },
        // .{ .Name = "ADC", .Cycles = 4, .AddrMode = &CPU.ABX, .Operator = &CPU.ADC },
        // .{ .Name = "ROR", .Cycles = 7, .AddrMode = &CPU.ABX, .Operator = &CPU.ROR },
        // .{ .Name = "RRA", .Cycles = 7, .AddrMode = &CPU.ABX, .Operator = &CPU.RRA },
    },
    .{
        // .{ .Name = "NOP", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.NOP }, // implemented
        // .{ .Name = "STA", .Cycles = 6, .AddrMode = &CPU.IZX, .Operator = &CPU.STA }, // implemented
        // .{ .Name = "NOP", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.NOP }, // implemented
        // .{ .Name = "SAX", .Cycles = 6, .AddrMode = &CPU.IZX, .Operator = &CPU.SAX },
        // .{ .Name = "STY", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.STY }, // implemented
        // .{ .Name = "STA", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.STA }, // implemented
        // .{ .Name = "STX", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.STX }, // IMPLEMENTED
        // .{ .Name = "SAX", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.SAX },
        // .{ .Name = "DEY", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.DEY },
        // .{ .Name = "NOP", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.NOP }, // IMPLEMENTED
        .{ .Name = "TXA", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.TXA },
        // .{ .Name = "ANE", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.ANE },
        .{ .Name = "STY", .Cycles = 4, .AddrMode = &CPU.ABS, .Operator = &CPU.STY },
        .{ .Name = "STA", .Cycles = 4, .AddrMode = &CPU.ABS, .Operator = &CPU.STA },
        .{ .Name = "STX", .Cycles = 4, .AddrMode = &CPU.ABS, .Operator = &CPU.STX },
        // .{ .Name = "SAX", .Cycles = 4, .AddrMode = &CPU.ABS, .Operator = &CPU.SAX },
    },
    .{
        // .{ .Name = "BCC", .Cycles = 2, .AddrMode = &CPU.REL, .Operator = &CPU.BCC }, // implemented
        // .{ .Name = "STA", .Cycles = 6, .AddrMode = &CPU.IZY, .Operator = &CPU.STA }, // implemented
        // // .{ .Name = "JAM", .Cycles = 1, .AddrMode = &CPU.IMP, .Operator = &CPU.JAM },
        // // .{ .Name = "SHA", .Cycles = 6, .AddrMode = &CPU.IZY, .Operator = &CPU.SHA },
        // .{ .Name = "STY", .Cycles = 4, .AddrMode = &CPU.ZPX, .Operator = &CPU.STY }, // implemented
        // .{ .Name = "STA", .Cycles = 4, .AddrMode = &CPU.ZPX, .Operator = &CPU.STA }, // implemented
        // .{ .Name = "STX", .Cycles = 4, .AddrMode = &CPU.ZPY, .Operator = &CPU.STX }, // implemented
        // // .{ .Name = "SAX", .Cycles = 4, .AddrMode = &CPU.ZPY, .Operator = &CPU.SAX },
        // .{ .Name = "TYA", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.TYA }, // implemented
        .{ .Name = "STA", .Cycles = 5, .AddrMode = &CPU.ABY, .Operator = &CPU.STA },
        .{ .Name = "TXS", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.TXS },
        // .{ .Name = "TAS", .Cycles = 5, .AddrMode = &CPU.ABY, .Operator = &CPU.TAS },
        .{ .Name = "NOP", .Cycles = 4, .AddrMode = &CPU.IMP, .Operator = &CPU.ABX },
        .{ .Name = "STA", .Cycles = 5, .AddrMode = &CPU.ABX, .Operator = &CPU.STA },
        // .{ .Name = "SHX", .Cycles = 1, .AddrMode = &CPU.ABY, .Operator = &CPU.SHX },
        // .{ .Name = "SHA", .Cycles = 5, .AddrMode = &CPU.ABY, .Operator = &CPU.SHA },
    },
    .{
        // .{ .Name = "LDY", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.LDY },
        // .{ .Name = "LDA", .Cycles = 6, .AddrMode = &CPU.IZX, .Operator = &CPU.LDA },
        // .{ .Name = "LDX", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.LDX },
        // .{ .Name = "LAX", .Cycles = 6, .AddrMode = &CPU.IZX, .Operator = &CPU.LAX },
        // .{ .Name = "LDY", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.LDY },
        // .{ .Name = "LDA", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.LDA },
        // .{ .Name = "LDX", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.LDX },
        // .{ .Name = "LAX", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.LAX },
        .{ .Name = "TAY", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.TAY },
        // .{ .Name = "LDA", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.LDA },
        .{ .Name = "TAX", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.TAX },
        .{ .Name = "TAX", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.TAX }, // DUP
        .{ .Name = "TAX", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.TAX }, // DUP
        // .{ .Name = "LXA", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.LXA },
        // .{ .Name = "LDY", .Cycles = 4, .AddrMode = &CPU.ABS, .Operator = &CPU.LDY },
        // .{ .Name = "LDA", .Cycles = 4, .AddrMode = &CPU.ABS, .Operator = &CPU.LDA },
        // .{ .Name = "LDX", .Cycles = 4, .AddrMode = &CPU.ABS, .Operator = &CPU.LDX },
        // .{ .Name = "LAX", .Cycles = 4, .AddrMode = &CPU.ABS, .Operator = &CPU.LAX },
    },
    .{
        // .{ .Name = "BCS", .Cycles = 2, .AddrMode = &CPU.REL, .Operator = &CPU.BCS },
        // .{ .Name = "LDA", .Cycles = 5, .AddrMode = &CPU.IZY, .Operator = &CPU.LDA },
        // .{ .Name = "JAM", .Cycles = 1, .AddrMode = &CPU.IMP, .Operator = &CPU.JAM },
        // .{ .Name = "LAX", .Cycles = 5, .AddrMode = &CPU.IZY, .Operator = &CPU.LAX },
        // .{ .Name = "LDY", .Cycles = 4, .AddrMode = &CPU.ZPX, .Operator = &CPU.LDY },
        // .{ .Name = "LDA", .Cycles = 4, .AddrMode = &CPU.ZPX, .Operator = &CPU.LDA },
        // .{ .Name = "LDX", .Cycles = 4, .AddrMode = &CPU.ZPY, .Operator = &CPU.LDX },
        // .{ .Name = "LAX", .Cycles = 4, .AddrMode = &CPU.ZPY, .Operator = &CPU.LAX },
        .{ .Name = "CLV", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.CLV },
        // .{ .Name = "LDA", .Cycles = 4, .AddrMode = &CPU.ABY, .Operator = &CPU.LDA },
        .{ .Name = "TSX", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.TSX },
        .{ .Name = "TSX", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.TSX }, // DUP
        .{ .Name = "TSX", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.TSX }, // DUP
        // .{ .Name = "LAS", .Cycles = 4, .AddrMode = &CPU.ABY, .Operator = &CPU.LAS },
        // .{ .Name = "LDY", .Cycles = 4, .AddrMode = &CPU.ABX, .Operator = &CPU.LDY },
        // .{ .Name = "LDA", .Cycles = 4, .AddrMode = &CPU.ABX, .Operator = &CPU.LDA },
        // .{ .Name = "LDX", .Cycles = 4, .AddrMode = &CPU.ABY, .Operator = &CPU.LDX },
        // .{ .Name = "LAX", .Cycles = 4, .AddrMode = &CPU.ABY, .Operator = &CPU.LAX },
    },
    .{
        // .{ .Name = "CPY", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.CPY },
        // .{ .Name = "CMP", .Cycles = 6, .AddrMode = &CPU.IZX, .Operator = &CPU.CMP },
        .{ .Name = "NOP", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.NOP },
        .{ .Name = "NOP", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.NOP }, // DUP
        .{ .Name = "NOP", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.NOP }, // DUP
        .{ .Name = "NOP", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.NOP }, // DUP
        // .{ .Name = "DCP", .Cycles = 8, .AddrMode = &CPU.IZX, .Operator = &CPU.DCP },
        // .{ .Name = "CPY", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.CPY },
        // .{ .Name = "CMP", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.CMP },
        // .{ .Name = "DEC", .Cycles = 5, .AddrMode = &CPU.ZP0, .Operator = &CPU.DEC },
        // .{ .Name = "DCP", .Cycles = 5, .AddrMode = &CPU.ZP0, .Operator = &CPU.DCP },
        // .{ .Name = "INY", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.INY },
        // .{ .Name = "CMP", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.CMP },
        // .{ .Name = "DEX", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.DEX },
        // .{ .Name = "SBX", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.SBX },
        // .{ .Name = "CPY", .Cycles = 4, .AddrMode = &CPU.ABS, .Operator = &CPU.CPY },
        // .{ .Name = "CMP", .Cycles = 4, .AddrMode = &CPU.ABS, .Operator = &CPU.CMP },
        // .{ .Name = "DEC", .Cycles = 6, .AddrMode = &CPU.ABS, .Operator = &CPU.DEC },
        // .{ .Name = "DCP", .Cycles = 6, .AddrMode = &CPU.ABS, .Operator = &CPU.DCP },
    },
    .{
        // .{ .Name = "BNE", .Cycles = 2, .AddrMode = &CPU.REL, .Operator = &CPU.BNE }, // Implemented
        // .{ .Name = "CMP", .Cycles = 5, .AddrMode = &CPU.IZY, .Operator = &CPU.CMP },
        // .{ .Name = "JAM", .Cycles = 1, .AddrMode = &CPU.IMP, .Operator = &CPU.JAM },
        // .{ .Name = "DCP", .Cycles = 8, .AddrMode = &CPU.IZY, .Operator = &CPU.DCP },
        .{ .Name = "NOP", .Cycles = 4, .AddrMode = &CPU.ZPX, .Operator = &CPU.NOP },
        // .{ .Name = "CMP", .Cycles = 4, .AddrMode = &CPU.ZPX, .Operator = &CPU.CMP },
        // .{ .Name = "DEC", .Cycles = 6, .AddrMode = &CPU.ZPX, .Operator = &CPU.DEC },
        // .{ .Name = "DCP", .Cycles = 6, .AddrMode = &CPU.ZPX, .Operator = &CPU.DCP },
        .{ .Name = "CLD", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.CLD },
        // .{ .Name = "CMP", .Cycles = 4, .AddrMode = &CPU.ABY, .Operator = &CPU.CMP },
        .{ .Name = "NOP", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.NOP },
        // .{ .Name = "DCP", .Cycles = 7, .AddrMode = &CPU.ABY, .Operator = &CPU.DCP },
        .{ .Name = "NOP", .Cycles = 4, .AddrMode = &CPU.IMP, .Operator = &CPU.ABX },
        // .{ .Name = "CMP", .Cycles = 4, .AddrMode = &CPU.ABX, .Operator = &CPU.CMP },
        // .{ .Name = "DEC", .Cycles = 7, .AddrMode = &CPU.ABX, .Operator = &CPU.DEC },
        // .{ .Name = "DCP", .Cycles = 7, .AddrMode = &CPU.ABX, .Operator = &CPU.DCP },
    },
    .{
        // .{ .Name = "CPX", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.CPX },
        // .{ .Name = "SBC", .Cycles = 6, .AddrMode = &CPU.IZX, .Operator = &CPU.SBC },
        .{ .Name = "NOP", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.NOP },
        // .{ .Name = "ISC", .Cycles = 8, .AddrMode = &CPU.IZX, .Operator = &CPU.ISC },
        // .{ .Name = "CPX", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.CPX },
        // .{ .Name = "SBC", .Cycles = 3, .AddrMode = &CPU.ZP0, .Operator = &CPU.SBC },
        // .{ .Name = "INC", .Cycles = 5, .AddrMode = &CPU.ZP0, .Operator = &CPU.INC },
        // .{ .Name = "ISC", .Cycles = 5, .AddrMode = &CPU.ZP0, .Operator = &CPU.ISC },
        // .{ .Name = "INX", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.INX },
        // .{ .Name = "SBC", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.SBC },
        .{ .Name = "NOP", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.NOP },
        .{ .Name = "NOP", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.NOP }, //DUP
        .{ .Name = "NOP", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.NOP }, //DUP
        // .{ .Name = "USB", .Cycles = 2, .AddrMode = &CPU.IMM, .Operator = &CPU.USB },
        // .{ .Name = "CPX", .Cycles = 4, .AddrMode = &CPU.ABS, .Operator = &CPU.CPX },
        // .{ .Name = "SBC", .Cycles = 4, .AddrMode = &CPU.ABS, .Operator = &CPU.SBC },
        // .{ .Name = "INC", .Cycles = 6, .AddrMode = &CPU.ABS, .Operator = &CPU.INC },
        // .{ .Name = "ISC", .Cycles = 6, .AddrMode = &CPU.ABS, .Operator = &CPU.ISC },
    },
    .{
        // .{ .Name = "BEQ", .Cycles = 2, .AddrMode = &CPU.REL, .Operator = &CPU.BEQ }, // IMPLEMENTED
        // .{ .Name = "SBC", .Cycles = 5, .AddrMode = &CPU.IZY, .Operator = &CPU.SBC },
        // .{ .Name = "JAM", .Cycles = 1, .AddrMode = &CPU.IMP, .Operator = &CPU.JAM },
        // .{ .Name = "ISC", .Cycles = 8, .AddrMode = &CPU.IZY, .Operator = &CPU.ISC },
        .{ .Name = "NOP", .Cycles = 4, .AddrMode = &CPU.ZPX, .Operator = &CPU.NOP },
        // .{ .Name = "SBC", .Cycles = 4, .AddrMode = &CPU.ZPX, .Operator = &CPU.SBC },
        // .{ .Name = "INC", .Cycles = 6, .AddrMode = &CPU.ZPX, .Operator = &CPU.INC },
        // .{ .Name = "ISC", .Cycles = 6, .AddrMode = &CPU.ZPX, .Operator = &CPU.ISC },
        .{ .Name = "SED", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.SED },
        // .{ .Name = "SBC", .Cycles = 4, .AddrMode = &CPU.ABY, .Operator = &CPU.SBC },
        .{ .Name = "NOP", .Cycles = 2, .AddrMode = &CPU.IMP, .Operator = &CPU.NOP },
        // .{ .Name = "ISC", .Cycles = 7, .AddrMode = &CPU.ABY, .Operator = &CPU.ISC },
        .{ .Name = "NOP", .Cycles = 4, .AddrMode = &CPU.IMP, .Operator = &CPU.ABX },
        // .{ .Name = "SBC", .Cycles = 4, .AddrMode = &CPU.ABX, .Operator = &CPU.SBC },
        // .{ .Name = "INC", .Cycles = 7, .AddrMode = &CPU.ABX, .Operator = &CPU.INC },
        // .{ .Name = "ISC", .Cycles = 7, .AddrMode = &CPU.ABX, .Operator = &CPU.ISC },
    },
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

test "reset" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();
    cpu.write(0xFFFC, 0x00);
    cpu.write(0xFFFD, 0x80);
    cpu.reset();
    try std.testing.expectEqual(cpu.pc, 128);
    try std.testing.expectEqual(cpu.sp, 0xFC);
    try std.testing.expectEqual(cpu.statusReg.I, false);
    try std.testing.expectEqual(cpu.statusReg.U, true);
    try std.testing.expectEqual(cpu.statusReg.B, false);
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

// TODO: this test is working but it will exit the process, figure out how to test it "Correctly"
// test "BRK" {
//     var allocator = std.testing.allocator;
//     var cpu = try CPU.init(&allocator);
//     defer cpu.bus.deinit();

//     cpu.pc = 0x1234;
//     cpu.sp = 0xFF;
//     cpu.bus.write(0x1234, 0x00);
//     cpu.bus.write(0x1235, 0x80);
//     cpu.bus.write(0x8000, 0x56);
//     cpu.bus.write(0x8001, 0x78);

//     _ = cpu.BRK();

//     try std.testing.expectEqual(cpu.pc, 0x7856);
//     try std.testing.expectEqual(cpu.sp, 0xFC);
//     try std.testing.expectEqual(cpu.statusReg.I, true);
//     try std.testing.expectEqual(cpu.statusReg.U, true);
//     try std.testing.expectEqual(cpu.statusReg.B, false);
// }

test "CLC" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.statusReg.C = true;
    _ = cpu.CLC();
    try std.testing.expectEqual(cpu.statusReg.C, false);
}

test "CLD" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.statusReg.D = true;
    _ = cpu.CLD();
    try std.testing.expectEqual(cpu.statusReg.D, false);
}

test "CLI" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.statusReg.I = true;
    _ = cpu.CLI();
    try std.testing.expectEqual(cpu.statusReg.I, false);
}

test "CLV" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.statusReg.V = true;
    _ = cpu.CLV();
    try std.testing.expectEqual(cpu.statusReg.V, false);
}

test "JMP" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.absAddr = 0x1234;
    _ = cpu.JMP();
    try std.testing.expectEqual(cpu.pc, 0x1234);
}
// TODO: Improve this test
test "JSR" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.pc = 0x1234;
    cpu.absAddr = 0x5678;
    _ = cpu.JSR();
    try std.testing.expectEqual(cpu.pc, 0x5678);
    try std.testing.expectEqual(cpu.read(0x01FF), 51);
}

test "NOP" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.opCode = 0x1C;
    try std.testing.expectEqual(cpu.NOP(), 1);

    cpu.opCode = 0x3C;
    try std.testing.expectEqual(cpu.NOP(), 1);

    cpu.opCode = 0x5C;
    try std.testing.expectEqual(cpu.NOP(), 1);

    cpu.opCode = 0x7C;
    try std.testing.expectEqual(cpu.NOP(), 1);

    cpu.opCode = 0xDC;
    try std.testing.expectEqual(cpu.NOP(), 1);

    cpu.opCode = 0xFC;
    try std.testing.expectEqual(cpu.NOP(), 1);
}

test "RTS" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.sp = 0xFC;
    cpu.bus.write(0x0100 + 0xFC, 0x12);
    cpu.bus.write(0x0100 + 0xFD, 0x34);
    cpu.pc = 0x1234;
    _ = cpu.RTS();
    try std.testing.expectEqual(cpu.pc, 43573);
}

test "PHA" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.acc = 0x12;
    cpu.sp = 0xFF;
    _ = cpu.PHA();
    try std.testing.expectEqual(cpu.read(0x0100 + 0xFF), 0x12);
    try std.testing.expectEqual(cpu.sp, 0xFE);
}

test "PHP" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.statusReg.B = true;
    cpu.statusReg.U = true;
    cpu.sp = 0xFF;
    _ = cpu.PHP();
    try std.testing.expectEqual(cpu.read(0x0100 + 0xFF), 0x30);
    try std.testing.expectEqual(cpu.sp, 0xFE);
}

test "PLA" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();
    cpu.sp = 0xFE;
    cpu.write(0x0100 + 0xFF, 0x12);

    _ = cpu.PLA();

    try std.testing.expectEqual(cpu.acc, 0x12);
    try std.testing.expectEqual(cpu.statusReg.Z, false);
    try std.testing.expectEqual(cpu.statusReg.N, false);
}

test "PLP" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();
    cpu.sp = 0xFE;
    cpu.write(0x0100 + 0xFF, 0x30);

    _ = cpu.PLP();
    try std.testing.expectEqual(cpu.statusReg.C, false);
    try std.testing.expectEqual(cpu.statusReg.Z, false);
    try std.testing.expectEqual(cpu.statusReg.I, false);
    try std.testing.expectEqual(cpu.statusReg.D, false);
    try std.testing.expectEqual(cpu.statusReg.B, true);
    try std.testing.expectEqual(cpu.statusReg.U, true);
    try std.testing.expectEqual(cpu.statusReg.V, false);
    try std.testing.expectEqual(cpu.statusReg.N, false);
}

test "RTI" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();
    cpu.sp = 0xFE;
    cpu.write(0x0100 + 0xFE, 0x30);
    cpu.write(0x0100 + 0xFD, 0x12);
    cpu.write(0x0100 + 0xFC, 0x34);

    _ = cpu.RTI();
    try std.testing.expectEqual(cpu.statusReg.C, false);
    try std.testing.expectEqual(cpu.statusReg.Z, true);
    try std.testing.expectEqual(cpu.statusReg.I, false);
    try std.testing.expectEqual(cpu.statusReg.D, true);
    try std.testing.expectEqual(cpu.statusReg.B, false);
    try std.testing.expectEqual(cpu.statusReg.U, false);
    try std.testing.expectEqual(cpu.statusReg.V, false);
    try std.testing.expectEqual(cpu.statusReg.N, true);
    try std.testing.expectEqual(cpu.pc, 43690);
}

test "SEC" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.statusReg.C = false;
    _ = cpu.SEC();
    try std.testing.expectEqual(cpu.statusReg.C, true);
}

test "SED" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.statusReg.D = false;
    _ = cpu.SED();
    try std.testing.expectEqual(cpu.statusReg.D, true);
}

test "SEI" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.statusReg.I = false;
    _ = cpu.SEI();
    try std.testing.expectEqual(cpu.statusReg.I, true);
}

test "STA" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.acc = 0x12;
    cpu.absAddr = 0x1234;
    _ = cpu.STA();
    try std.testing.expectEqual(cpu.read(0x1234), 0x12);
}

test "STX" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.xReg = 0x12;
    cpu.absAddr = 0x1234;
    _ = cpu.STX();
    try std.testing.expectEqual(cpu.read(0x1234), 0x12);
}

test "STY" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.yReg = 0x12;
    cpu.absAddr = 0x1234;
    _ = cpu.STY();
    try std.testing.expectEqual(cpu.read(0x1234), 0x12);
}

test "TAX" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.acc = 0x12;
    _ = cpu.TAX();
    try std.testing.expectEqual(cpu.xReg, 0x12);
    try std.testing.expectEqual(cpu.statusReg.Z, false);
    try std.testing.expectEqual(cpu.statusReg.N, false);
}

test "TAY" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.acc = 0x12;
    _ = cpu.TAY();
    try std.testing.expectEqual(cpu.yReg, 0x12);
    try std.testing.expectEqual(cpu.statusReg.Z, false);
    try std.testing.expectEqual(cpu.statusReg.N, false);
}

test "TSX" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.sp = 0x12;
    _ = cpu.TSX();
    try std.testing.expectEqual(cpu.xReg, 0x12);
    try std.testing.expectEqual(cpu.statusReg.Z, false);
    try std.testing.expectEqual(cpu.statusReg.N, false);
}

test "TXS" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.xReg = 0x12;
    _ = cpu.TXS();
    try std.testing.expectEqual(cpu.sp, 0x12);
}

test "TXA" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.xReg = 0x12;
    _ = cpu.TXA();
    try std.testing.expectEqual(cpu.acc, 0x12);
    try std.testing.expectEqual(cpu.statusReg.Z, false);
    try std.testing.expectEqual(cpu.statusReg.N, false);
}

test "TYA" {
    var allocator = std.testing.allocator;
    var cpu = try CPU.init(&allocator);
    defer cpu.bus.deinit();

    cpu.yReg = 0x12;
    _ = cpu.TYA();
    try std.testing.expectEqual(cpu.acc, 0x12);
    try std.testing.expectEqual(cpu.statusReg.Z, false);
    try std.testing.expectEqual(cpu.statusReg.N, false);
}

//TODO!:FIX these 2 tests, I'm not sure if this is the correct way to test them
// test "cpu fetch" {
//     var allocator = std.testing.allocator;
//     var cpu = try CPU.init(&allocator);
//     defer cpu.bus.deinit();

//     cpu.write(0x8000, 0xA9); // LDA Immediate
//     cpu.write(0x8001, 0x42); // Value to load
//     cpu.pc = 0x8000;

//     _ = cpu.clock(); // Fetch opcode
//     _ = cpu.IMM(); // Set addressing mode
//     const value = cpu.fetch();

//     try std.testing.expectEqual(value, 0x42);
// }

// test "AND" {
//     var allocator = std.testing.allocator;
//     var cpu = try CPU.init(&allocator);
//     defer cpu.bus.deinit();

//     cpu.acc = 0x12;
//     cpu.write(0x1234, 0x34);
//     _ = cpu.ABS();
//     _ = cpu.AND();
//     try std.testing.expectEqual(cpu.acc, 0x12 & 0x34);
//     try std.testing.expectEqual(cpu.statusReg.Z, false);
//     try std.testing.expectEqual(cpu.statusReg.N, false);
// }

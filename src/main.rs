#![allow(unused)]
#![feature(bigint_helper_methods)]

/// === Registers ===
/// R0..R9  - General purpose
/// ZO (R10)- Zero register
/// CF (R11)- Condition Flags
/// IP (R12)- Instruction Pointer
/// SP (R13)- Stack Pointer
/// HI (R14)- High 32-bits (Multiply) / Quotient 32-bits  (Division) 
/// LO (R15)- Low 32-bits  (Multiply) / Remainder 32-bits (Division)

/// === Instructions ===
/// 0000 0000 0000 0000 0000 0000 0000 0000
/// ^^^^ ^^^^ ^^^^ ^^^^ 
/// Code  Dst  Lhs  Rhs Unused -> 

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
enum Code {
    Hlt = 0, // stop program
    Imm,     // a = imm
    Mov,     // a = b
    Add,     // a = a + b
    Sub,     // a = a - b
    Mul,     // a = a * b
    Div,     // a = a / b
}

impl Code {
    #[inline(always)]
    const fn into_u8(self) -> u8 {
        self as u8
    }
 
    #[inline(always)]
    const fn from_u8(byte: u8) -> Code {
        unsafe { std::mem::transmute(byte) }
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
struct Inst(u32);

impl Inst {
    #[inline(always)]
    const fn into_u32(self) -> u32 {
        self.0
    }

    #[inline(always)]
    const fn from_u32(bytes: u32) -> Inst {
        Inst(bytes)
    }

    #[inline(always)]
    const fn code(&self) -> Code {
        Code::from_u8((self.0 >> 28) as u8) 
    }

    #[inline(always)]
    const fn dst(&self) -> usize {
        ((self.0 >> 24) & 0xF) as usize
    }

    #[inline(always)]
    const fn lhs(&self) -> usize {
        ((self.0 >> 20) & 0xF) as usize
    }

    #[inline(always)]
    const fn rhs(&self) -> usize {
        ((self.0 >> 16) & 0xF) as usize
    }

    #[inline(always)]
    const fn imm(&self) -> u16 {
        ((self.0 >> 16) & 0xFF) as u16
    }

    const fn __fmt_reg(reg: usize) -> &'static str {
        match reg {
            0  => "R0",
            1  => "R1",
            2  => "R2",
            3  => "R3",
            4  => "R4",
            5  => "R5",
            6  => "R6",
            7  => "R7",
            8  => "R8",
            9  => "R9",
            10 => "HI",
            11 => "LO",
            12 => "ZO",
            13 => "CF",
            14 => "IP",
            15 => "SP",
            _ => unreachable!(),
        }
    }
}

impl std::fmt::Debug for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.code() {
            Code::Hlt => write!(f, "hlt"),
            Code::Imm => write!(f, "imm {}, #{}",    Inst::__fmt_reg(self.dst()), self.imm()),
            Code::Mov => write!(f, "mov {}, {}",     Inst::__fmt_reg(self.dst()), Inst::__fmt_reg(self.lhs())),
            Code::Add => write!(f, "add {}, {}, {}", Inst::__fmt_reg(self.dst()), Inst::__fmt_reg(self.lhs()), Inst::__fmt_reg(self.rhs())),
            Code::Sub => write!(f, "sub {}, {}, {}", Inst::__fmt_reg(self.dst()), Inst::__fmt_reg(self.lhs()), Inst::__fmt_reg(self.rhs())),
            Code::Mul => write!(f, "mul {}, {}",     Inst::__fmt_reg(self.lhs()), Inst::__fmt_reg(self.rhs())),
            Code::Div => write!(f, "div {}, {}",     Inst::__fmt_reg(self.lhs()), Inst::__fmt_reg(self.rhs())),
        }
    }
}

struct Machine {
    mem: [u8;  65535],
    reg: [u16; 16],
}

impl Machine {
    const HI: usize = 10; const LO: usize = 11;
    const ZO: usize = 12; const CF: usize = 13;
    const IP: usize = 14; const SP: usize = 15;

    fn new() -> Machine {
        Machine {
            mem: [0; 65535],
            reg: [0; 16],
        }
    }

    const fn load(&mut self, code: &[u32]) {
        let mut i = 0;
        while i < code.len() {
            self.set32((i * 4) as u16, code[i]);
            i += 1;
        }
    }

    #[inline(always)]
    const fn get8(&self, idx: u16) -> u8 {
        self.mem[idx as usize]
    }

    #[inline(always)]
    const fn get16(&self, idx: u16) -> u16 {
        u16::from_be_bytes([self.get8(idx), self.get8(idx + 1)])
    }

    #[inline(always)]
    const fn get32(&self, idx: u16) -> u32 {
        u32::from_be_bytes([self.get8(idx + 0), self.get8(idx + 1), 
                            self.get8(idx + 2), self.get8(idx + 3)])
    }

    #[inline(always)]
    const fn set8(&mut self, idx: u16, val: u8) {
        self.mem[idx as usize] = val;
    }

    #[inline(always)]
    const fn set16(&mut self, idx: u16, val: u16) {
        let bytes = val.to_be_bytes();
        self.mem[(idx + 0) as usize] = bytes[0];
        self.mem[(idx + 1) as usize] = bytes[1];
    }

    #[inline(always)]
    const fn set32(&mut self, idx: u16, val: u32) {
        let bytes = val.to_be_bytes();
        self.mem[(idx + 0) as usize] = bytes[0];
        self.mem[(idx + 1) as usize] = bytes[1];
        self.mem[(idx + 2) as usize] = bytes[2];
        self.mem[(idx + 3) as usize] = bytes[3];
    }

    #[inline(never)]
    const fn step(&mut self) -> bool {
        let inst = Inst::from_u32(self.get32(self.reg[Self::IP]));
        self.reg[Self::IP] += 4;
        self.reg[Self::ZO] = 0;

        match inst.code() {
            Code::Hlt => return false,
            Code::Imm => self.reg[inst.dst()] = inst.imm(),
            Code::Mov => self.reg[inst.dst()] = self.reg[inst.lhs()],
            Code::Add => self.reg[inst.dst()] = self.reg[inst.lhs()] + self.reg[inst.rhs()],
            Code::Sub => self.reg[inst.dst()] = self.reg[inst.lhs()] - self.reg[inst.rhs()],

            Code::Mul => {
                let lhs = self.reg[inst.lhs()];
                let rhs = self.reg[inst.rhs()];
                let mul = u16::widening_mul(lhs, rhs);
                
                self.reg[Self::HI] = mul.1;
                self.reg[Self::LO] = mul.0;
            }

            Code::Div => {
                let lhs = self.reg[inst.lhs()];
                let rhs = self.reg[inst.rhs()];
                
                self.reg[Self::HI] = lhs / rhs;
                self.reg[Self::LO] = lhs % rhs;
            }
        }

        true
    }

    const fn run(&mut self) {
        while self.step() {}
    }

    fn dbg(&mut self) {
        loop {
            let inst = Inst::from_u32(self.get32(self.reg[Self::IP]));
            self.reg[Self::IP] += 4;
            println!("{inst:?}");

            if inst.code() == Code::Hlt {
                self.reg[Self::IP] = 0;
                break;
            }
        }
    }
}

fn main() {

}

#![allow(unused)] 
#![feature(bigint_helper_methods)]

/// === Registers ===
/// R0..R9  - General purpose registers
/// 
/// === Zero Register (ZR) ===
/// - always zero
/// ==========================
/// 
/// === Condition Flags (CF) ===
///          0000 0000 0000 0000
/// zero  ---+||| |
/// sign  ----+|| |
/// flow  -----+| |
/// carry ------+ |
/// unused--------+
/// 
/// remove to free a register?
/// put into machine struct?
/// ============================
/// 
/// ==== Instruction Pointer (IP) ===
/// - tracks program execution
/// - points 4 bytes past the to-be executed instruction
/// 
/// remove to free a register?
/// put into machine struct?
/// =================================
/// 
/// === Stack Pointer (SP) ===
/// - tracks stack space
/// - conventionally used for function locals
/// 
/// remove to free a register?
/// have any register act as a stack pointer?
/// ==========================
/// 
/// === High Register (HI) ===
/// - stores overflowing half of multiplication
/// - stores quotient of division
/// ==========================
///
/// === Low Register (LO) ===
/// - stores result (possibly wrapping) of multiplication
/// - stores remainder of division
/// =========================

/// === Instructions ===
/// == Single Data Transfer ==
///         0000 0000 0000 0000 0000 0000 0000 0000
/// cond ---+    |    |||| |    |    |    +----- offset multiplier register
/// code --------+    |||| |    |    +---------- beginning of 12-bit offset or offset register      
/// imm  -------------+||| |    +--------------- memory register        
/// neg  --------------+|| +-------------------- desination register        
/// byte ---------------++---------------------- sign flag, only used in load instruction
/// ==========================
/// 
/// == Multi Data Transfer ==
///  - TODO
/// =========================
/// 
/// == Math Operations ==
///         0000 0000 00XX 0000 0000 0000 0000 0000
/// cond ---+    |    ||   |    |    +----------- beginning of 12-bit value or rhs register
/// code --------+    ||   |    +---------------- left side operand register 
/// imm  -------------+|   +--------------------- destination register  
/// set  --------------+
/// =====================
/// 
/// == Jump ==
///         0000 0000 0000 0000 0000 0000 0000 0000
/// cond ---+    |    ||   +----- immediate value or 4-bit register containing address
/// code --------+    ||   
/// imm  -------------+|   
/// save --------------+   
/// 
/// imm set, save ip clear
/// - absolute static branch
/// 
/// imm clear, save ip clear
/// - absolute dynamic branch
/// 
/// imm set, save ip set
/// - function call
/// 
/// imm clear, save ip set
/// - virtual function call
/// ===============

#[repr(u8)]
#[derive(Clone, Copy)]
enum Cond {
    No, // unconditional
    Eq, // zero
    Ne, // !zero
    Ss, // sign
    Sc, // !sign
    Os, // overflow
    Oc, // !overflow
    Cs, // carry
    Cc, // !carry
    Ge, // sign == overflow
    Lt, // sign != overflow
    Gt, // zero || sign != overflow
    Le, // !zero && sign == overflow
}

impl Cond {
    #[inline(always)]
    const fn from_u8(byte: u8) -> Cond {
        unsafe { std::mem::transmute(byte) }
    }
}

#[repr(u8)]
#[derive(Clone, Copy)]
enum Code {
    Hlt, // stop program
    Ret, // ip = pop
    Jmp, // complicated
    Mov, // a = b
    Ldr, // a = [b]
    Str, // [b] = a
    Add, // a = a + b
    Sub, // a = a - b
    Mul, // a = a * b
    Div, // a = a / b
    And, // a = a & b
    Orr, // a = a | b
    Xor, // a = a ^ b
}

impl Code {
    #[inline(always)]
    const fn from_u8(byte: u8) -> Code {
        unsafe { std::mem::transmute(byte) }
    }
}

#[repr(transparent)]
struct Inst(u32);

impl Inst {
    #[inline(always)]
    const fn cond(&self) -> Cond {
        Cond::from_u8(((self.0 >> 28) & 0xF) as u8) 
    }

    const fn code(&self) -> Code {
        Code::from_u8(((self.0 >> 24) & 0xF) as u8) 
    }
}

impl std::fmt::Display for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.code() {
            Code::Hlt => write!(f, "hlt"),
            Code::Ret => write!(f, "ret"),
            
            Code::Jmp => {
                let jump = Jump(self.0);

                if jump.imm() {
                    write!(f, "jmp .x{:04X}", jump.abs())
                } else {
                    write!(f, "jmp R{}", jump.reg()) 
                }
            }

            Code::Mov => {
                let math = Math(self.0);

                if math.imm() {
                    write!(f, "mov R{}, #{}", math.dst(), math.val())
                } else {
                    write!(f, "cpy R{}, R{}", math.dst(), math.rhs())
                }
            }

            Code::Ldr => {
                let data = Data(self.0);

                let neg  = if data.neg()  { '-' } else { '+' };
                let sign = if data.sign() { '-' } else { ' ' };
                let byte = if data.byte() { 'b' } else { 'w' };

                if data.imm() {
                    write!(f, "ld{byte} R{}, {sign}[R{} {neg} {}]", data.dst(), data.mem(), data.val())
                } else {
                    write!(f, "ld{byte} R{}, {sign}[R{} {neg} R{}]", data.dst(), data.mem(), data.off())
                }
            }

            Code::Str => {
                let data = Data(self.0);

                let neg  = if data.neg()  { '-' } else { '+' };
                let byte = if data.byte() { 'b' } else { 'w' };

                if data.imm() {
                    write!(f, "st{byte} R{}, [R{} {neg} {}]", data.dst(), data.mem(), data.val())
                } else {
                    write!(f, "st{byte} R{}, [R{} {neg} R{}]", data.dst(), data.mem(), data.off())
                }
            }

            Code::Add => {
                let math = Math(self.0);

                if math.imm() {
                    write!(f, "add R{}, R{}, #{}", math.dst(), math.lhs(), math.val())
                } else {
                    write!(f, "add R{}, R{}, R{}", math.dst(), math.lhs(), math.rhs())
                }
            }

            Code::Sub => {
                let math = Math(self.0);

                if math.imm() {
                    write!(f, "sub R{}, R{}, #{}", math.dst(), math.lhs(), math.val())
                } else {
                    write!(f, "sub R{}, R{}, R{}", math.dst(), math.lhs(), math.rhs())
                }
            }

            Code::And => {
                let math = Math(self.0);

                if math.imm() {
                    write!(f, "and R{}, R{}, #{}", math.dst(), math.lhs(), math.val())
                } else {
                    write!(f, "and R{}, R{}, R{}", math.dst(), math.lhs(), math.rhs())
                }
            }

            Code::Orr => {
                let math = Math(self.0);

                if math.imm() {
                    write!(f, "orr R{}, R{}, #{}", math.dst(), math.lhs(), math.val())
                } else {
                    write!(f, "orr R{}, R{}, R{}", math.dst(), math.lhs(), math.rhs())
                }
            }

            Code::Xor => {
                let math = Math(self.0);

                if math.imm() {
                    write!(f, "xor R{}, R{}, #{}", math.dst(), math.lhs(), math.val())
                } else {
                    write!(f, "xor R{}, R{}, R{}", math.dst(), math.lhs(), math.rhs())
                }
            }

            Code::Mul => {
                let math = Math(self.0);

                if math.imm() {
                    write!(f, "mul R{}, #{}", math.lhs(), math.val())
                } else {
                    write!(f, "mul R{}, R{}", math.lhs(), math.rhs())
                }
            }

            Code::Div => {
                let math = Math(self.0);

                if math.imm() {
                    write!(f, "div R{}, #{}", math.lhs(), math.val())
                } else {
                    write!(f, "div R{}, R{}", math.lhs(), math.rhs())
                }
            }
        }
    }
}

#[repr(transparent)]
struct Data(u32);

impl Data {
    #[inline(always)]
    const fn imm(&self) -> bool {
        ((self.0 >> 23) & 1) != 0 
    }

    #[inline(always)]
    const fn neg(&self) -> bool {
        ((self.0 >> 22) & 1) != 0 
    }

    #[inline(always)]
    const fn byte(&self) -> bool {
        ((self.0 >> 21) & 1) != 0 
    }

    #[inline(always)]
    const fn sign(&self) -> bool {
        ((self.0 >> 20) & 1) != 0
    }

    #[inline(always)]
    const fn dst(&self) -> usize {
        ((self.0 >> 16) & 0xF) as usize
    }
        
    #[inline(always)]
    const fn mem(&self) -> usize {
        ((self.0 >> 12) & 0xF) as usize
    }

    // imm is false
    #[inline(always)]
    const fn off(&self) -> usize {
        ((self.0 >> 8) & 0xF) as usize
    }

    #[inline(always)]
    const fn mul(&self) -> usize {
        ((self.0 >> 4) & 0xF) as usize
    }

    // imm is true
    #[inline(always)]
    const fn val(&self) -> u16 {
        (self.0 & 0x0FFF) as u16
    }
}

#[repr(transparent)]
struct Math(u32);

impl Math {
    #[inline(always)]
    const fn imm(&self) -> bool {
        ((self.0 >> 23) & 1) != 0 
    }

    #[inline(always)]
    const fn set(&self) -> bool {
        ((self.0 >> 22) & 1) != 0
    }

    #[inline(always)]
    const fn dst(&self) -> usize {
        ((self.0 >> 16) & 0xF) as usize
    }
        
    #[inline(always)]
    const fn lhs(&self) -> usize {
        ((self.0 >> 12) & 0xF) as usize
    }
        
    #[inline(always)] // imm is false
    const fn rhs(&self) -> usize {
        ((self.0 >> 8) & 0xF) as usize
    }

    #[inline(always)] // imm is true
    const fn val(&self) -> u16 {
        (self.0 & 0x0FFF) as u16
    }
}

#[repr(transparent)]
struct Jump(u32);

impl Jump {
    #[inline(always)]
    const fn imm(&self) -> bool {
        ((self.0 >> 23) & 1) != 0
    }

    #[inline(always)]
    const fn save(&self) -> bool {
        ((self.0 >> 22) & 1) != 0
    }

    // imm is false
    #[inline(always)]
    const fn reg(&self) -> usize {
        (self.0 & 0xF) as usize
    }

    // imm is true
    #[inline(always)]
    const fn abs(&self) -> u16 {
        (self.0 & 0xFFFF) as u16
    }
}

struct Machine {
    mem: [u8;  65535],
    reg: [u16; 16],
}

impl Machine {
    const HI: usize = 10; const LO: usize = 11;
    const ZR: usize = 12; const CF: usize = 13;
    const IP: usize = 14; const SP: usize = 15;

    fn new() -> Machine {
        Machine {
            mem: [0; 65535],
            reg: [0; 16],
        }
    }

    fn load(&mut self, code: &[u32]) {
        for (inst, i) in code.iter().cloned().zip(0..) {
            self.set32(i * 4, inst);
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

    fn step(&mut self) -> bool {
        let inst = Inst(self.get32(self.reg[Self::IP]));
        let cond = inst.cond();
        let code = inst.code();

        println!("{inst}");
        self.reg[Self::IP] += 4;
        self.reg[Self::ZR] = 0;

        let zero  = self.reg[Self::CF] & 1 != 0;
        let sign  = self.reg[Self::CF] & 2 != 0;
        let flow  = self.reg[Self::CF] & 4 != 0;
        let carry = self.reg[Self::CF] & 8 != 0;

        match cond {
            Cond::Eq if !zero                 => return true,
            Cond::Ne if zero                  => return true,
            Cond::Ss if !sign                 => return true,
            Cond::Sc if sign                  => return true,
            Cond::Os if !flow                 => return true,
            Cond::Oc if flow                  => return true,
            Cond::Cs if !carry                => return true,
            Cond::Cc if carry                 => return true,
            Cond::Ge if sign != flow          => return true,
            Cond::Lt if sign == flow          => return true,
            Cond::Gt if !zero && sign != flow => return true,
            Cond::Le if zero || sign != flow  => return true,
            _ => (),
        }

        match code {
            Code::Hlt => return false,

            Code::Ret => {
                self.reg[Self::SP] -= 2;
                self.reg[Self::IP] = self.get16(self.reg[Self::SP]);
            }
            
            Code::Jmp => {
                let jump = Jump(inst.0);

                if jump.save() {
                    self.set16(self.reg[Self::SP], self.reg[Self::IP]);
                    self.reg[Self::SP] += 2;
                }

                self.reg[Self::IP] = if jump.imm() { jump.abs() } else { self.reg[jump.reg()] };
            }

            Code::Mov => {
                let data = Math(inst.0);

                self.reg[data.dst()] = if data.imm() { data.val() } else { self.reg[data.rhs()] };
            }
            
            Code::Ldr => {
                let data = Data(inst.0);
                
                let idx = self.reg[data.mem()];
                let idx = match (data.imm(), data.neg()) {
                    // positive register offset
                    (false, false) => idx + self.reg[data.off()] * self.reg[data.mul()],

                    // positive immediate offset
                    (true, false)  => idx + data.val(),

                    // negative register offset
                    (false, true)  => idx - self.reg[data.off()] * self.reg[data.mul()],

                    // negative immediate offset
                    (true, true)   => idx - data.val(),
                };

                self.reg[data.dst()] = match (data.byte(), data.sign()) {
                    // 16-bit signed/unsigned load
                    (false, _)    => self.get16(idx),
                    (true, false) => self.get8(idx) as u16,
                    (true, true)  => self.get8(idx).cast_signed() as u16,
                };
            }

            Code::Str => {
                let data = Data(inst.0);

                let val = self.reg[data.dst()];
                let idx = self.reg[data.mem()];
                let idx = match (data.imm(), data.neg()) {
                    // positive register offset
                    (false, false) => idx + self.reg[data.off()] * self.reg[data.mul()],

                    // positive immediate offset
                    (true, false)  => idx + data.val(),

                    // negative register offset
                    (false, true)  => idx - self.reg[data.off()] * self.reg[data.mul()],

                    // negative immediate offset
                    (true, true)   => idx - data.val(),
                };

                if data.byte() { self.set8(idx, val as u8); } else { self.set16(idx, val); }
            }

            Code::Add | Code::Sub | 
            Code::Mul | Code::Div | 
            Code::And | Code::Orr | Code::Xor => {
                let math = Math(inst.0);

                let lhs = self.reg[math.lhs()];
                let rhs = if math.imm() { math.val() } else { self.reg[math.rhs()] };

                match code {
                    Code::Add => self.reg[math.dst()] = u16::wrapping_add(lhs, rhs),
                    Code::Sub => self.reg[math.dst()] = u16::wrapping_sub(lhs, rhs),
                    Code::And => self.reg[math.dst()] = lhs & rhs,
                    Code::Orr => self.reg[math.dst()] = lhs | rhs,
                    Code::Xor => self.reg[math.dst()] = lhs ^ rhs,

                    Code::Mul => {
                        let mul = u16::widening_mul(lhs, rhs);                
                        self.reg[Self::HI] = mul.1;
                        self.reg[Self::LO] = mul.0;
                    }
                    
                    Code::Div => {
                        self.reg[Self::HI] = lhs % rhs;
                        self.reg[Self::LO] = lhs / rhs;
                    }

                    _ => unreachable!(),
                }

                if math.set() {
                    // TODO: overflow
                    // TODO: carry
                    if matches!(code, Code::Mul | Code::Div) {
                        self.reg[Self::CF] |= ((self.reg[Self::HI] == 0) as u16);
                        self.reg[Self::CF] |= (((self.reg[Self::HI] & 0x8000) != 0) as u16) << 1;
                    } else {
                        self.reg[Self::CF] |= ((self.reg[math.dst()] != 0) as u16);
                        self.reg[Self::CF] |= (((self.reg[math.dst()] & 0x8000) != 0) as u16) << 1;
                    }
                }
            }
        }

        true
    }

    fn run(&mut self) {
        while self.step() {}
    }

    fn dbg(&mut self) {
        loop {
            let inst = Inst(self.get32(self.reg[Self::IP]));
            println!("{:04X}: {}", self.reg[Self::IP], inst);
            self.reg[Self::IP] += 4;

            if matches!(inst.code(), Code::Hlt) {
                self.reg[Self::IP] = 0;
                break;
            }
        } 
    }
}

fn main() {
    let code = [
        0x03_81_00_0A,
        0x06_80_00_01,
        0x07_4C_01_00,
        0x12_80_00_00,
    ];

    let mut m = Machine::new();
    m.load(&code);
    m.dbg();
    m.run();

    for (i, val) in m.reg.iter().enumerate() {
        println!("R{i}: {val}");
    }
}

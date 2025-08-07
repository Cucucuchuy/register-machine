#![allow(unused)] 
#![feature(bigint_helper_methods)]

/// === Instructions ===
/// == System call ==
///         0000 0000 0000 0000 0000 0000 0000 0000
/// cond ---+    |    |    |    |    |    |    +----  4-bit value or bar register
/// code --------+    |    |    |    |    +---------  8-bit value or foo register
/// kind -------------+    |    |    +-------------- 12-bit value or rhs register
///                        |    +------------------- 16-bit value or lhs register
///                        +------------------------ 20-bit value or dst register
/// 
/// - not set in stone / completely arbitrary
/// + which is why the arguments are as generic as possible
/// 
/// - for use by system calls to have a slightly more structured approach
/// + then passing arguments in set registers, but it'll go back to that if neccessary
/// =================
/// 
/// == Single Data Transfer ==
///         0000 0000 0000 0000 0000 0000 0000 0000
/// cond ---+    |    |||| |    |    |    |    +---- unused in non-immediate mode
/// code --------+    |||| |    |    |    +--------- offset multiplier register (soon to be deprecated)
/// imm  -------------+||| |    |    +-------------- beginning of 12-bit offset or offset register      
/// neg  --------------+|| |    +------------------- memory register        
/// byte ---------------+| +------------------------ destination register        
///                      +-------------------------- sign flag, only used in load instruction
/// ==========================
/// 
/// == Multi Data Transfer ==
///         0000 0000 0000 0000 0000 0000 0000 0000
/// cond ---+    |    |
/// code --------+    |
/// imm  -------------+
/// 
/// - TODO
/// =========================
/// 
/// == Stack operations ==
///         0000 0000 0000 0000 0000 0000 0000 0000
/// cond ---+    |    |
/// code --------+    |
/// imm  -------------+
/// 
/// - TODO
/// ======================
/// 
/// == Math Operations ==
///         0000 0000 00XX 0000 0000 0000 0000 0000
/// cond ---+    |    ||   |    |    |    +--------- unused in non-immediate mode
/// code --------+    ||   |    |    +-------------- beginning of 12-bit value or rhs register
/// imm  -------------+|   |    +------------------- left side operand register 
///                    |   +------------------------ destination register  
///                    +------- sign flag (by mul/div)
///                             cin  flag (by add/sub)
///                             not  flag (by and/orr)
/// =====================
/// 
/// == Jump operation ==
///         0000 0000 00XX XXXX 0000 0000 0000 0000
///         |    |    ||   |    |    +-------------- unused in non-immediate mode
/// cond ---+    |    ||   |    +------------------- 16-bit address/offset or register address/offset
/// code --------+    ||   +------------------------ base register
/// imm  -------------+|   
/// save --------------+   
/// 
/// - 0b0000 -- address in register, save off
/// - dynamic branch
/// 
/// - 0b0001 -- address in register, save set
/// - dynamic function call
/// 
/// - 0b0010 -- address is immediate, save off
/// - static branch
/// 
/// - 0b0011 -- address is immediate, save set
/// - static function call
/// ====================
mod inst {
    use std::fmt::Display;
    pub use build::*;
    
    // TODO: Complete builder methods
    mod build {
        use std::marker::PhantomData as Boo;
        use super::*;

        pub struct Add; 
        pub struct Sub; 

        pub struct Mul; 
        pub struct Div; 
 
        pub struct And; 
        pub struct Orr; 
        pub struct Xor; 

        pub trait Cin    {}
        impl Cin for Add {}
        impl Cin for Sub {}

        pub trait Neg    {}
        impl Neg for Mul {}
        impl Neg for Div {}

        pub trait Not    {}
        impl Not for And {}
        impl Not for Orr {}
        impl Not for Xor {}

        // TODO: Fix builder methods
        pub trait Build    { const CODE: Code; }
        // impl Build for Add { const CODE: Code = Code::Add; }
        // impl Build for Sub { const CODE: Code = Code::Sub; }
        // impl Build for Mul { const CODE: Code = Code::Mul; }
        // impl Build for Div { const CODE: Code = Code::Div; }
        // impl Build for And { const CODE: Code = Code::And; }
        // impl Build for Orr { const CODE: Code = Code::Orr; }
        // impl Build for Xor { const CODE: Code = Code::Xor; }

        /* States */
        pub struct Nil; /* -> Bit | Dst */
        pub struct Bit; /* -> Dst | Lhs */
        pub struct Dst; /* -> Lhs */
        pub struct Lhs; /* -> Reg | Imm */
        pub struct Reg; /* -> Complete */
        pub struct Imm; /* -> Complete */

        pub struct Bin<O, S> {
            bit: u8,
            dst: u8,
            lhs: u8,
            rhs: u8,
            imm: u16,
            boo: Boo<(O, S)>,
        }

        impl<O, S> Bin<O, S> {
            #[inline(always)]
            const fn new() -> Bin<O, S> {
                Bin { bit: 0, dst: 0, lhs: 0, rhs: 0, imm: 0, boo: Boo }
            }
        }

        impl<O> Bin<O, Nil> {
            #[inline(always)]
            pub const fn dst(self, dst: u8) -> Bin<O, Dst> {
                Bin {
                    bit: self.bit,
                    dst,
                    lhs: self.lhs,
                    rhs: self.rhs,
                    imm: self.imm,
                    boo: Boo,
                }
            }
        }

        impl<O> Bin<O, Bit> {
            #[inline(always)]
            pub const fn dst(self, dst: u8) -> Bin<O, Dst> {
                Bin {
                    bit: self.bit,
                    dst,
                    lhs: self.lhs,
                    rhs: self.rhs,
                    imm: self.imm,
                    boo: Boo,
                }
            }
        }

        impl<O> Bin<O, Dst> {
            #[inline(always)]
            pub const fn lhs(self, lhs: u8) -> Bin<O, Lhs> {
                Bin {
                    bit: self.bit,
                    dst: self.dst,
                    lhs,
                    rhs: self.rhs,
                    imm: self.imm,
                    boo: Boo,
                }
            }
        }

        impl<O> Bin<O, Lhs> {
            #[inline(always)]
            pub const fn rhs(self, rhs: u8) -> Bin<O, Reg> {
                Bin {
                    bit: self.bit,
                    dst: self.dst,
                    lhs: self.lhs,
                    rhs,
                    imm: self.imm,
                    boo: Boo,
                }
            }

            #[inline(always)]
            pub const fn imm(self, imm: u16) -> Bin<O, Imm> {
                Bin {
                    bit: self.bit,
                    dst: self.dst,
                    lhs: self.lhs,
                    rhs: self.rhs,
                    imm,
                    boo: Boo,
                }
            }
        }

        impl<O: Cin> Bin<O, Nil> {
            #[inline(always)]
            pub const fn cin(self, bit: bool) -> Bin<Add, Bit> {
                Bin {
                    bit: bit as u8,
                    dst: self.dst,
                    lhs: self.lhs,
                    rhs: self.rhs,
                    imm: self.imm,
                    boo: Boo,
                }
            }   
        }

        impl<O: Neg> Bin<O, Nil> {
            #[inline(always)]
            pub const fn neg(self, bit: bool) -> Bin<Mul, Bit> {
                Bin {
                    bit: bit as u8,
                    dst: self.dst,
                    lhs: self.lhs,
                    rhs: self.rhs,
                    imm: self.imm,
                    boo: Boo,
                }
            }
        }

        impl<O: Not> Bin<O, Nil> {
            #[inline(always)]
            pub const fn not(self, bit: bool) -> Bin<And, Bit> { 
                Bin {
                    bit: bit as u8,
                    dst: self.dst,
                    lhs: self.lhs,
                    rhs: self.rhs,
                    imm: self.imm,
                    boo: Boo,
                }
            }
        }
        
        impl<O: Build> Bin<O, Reg> {
            pub fn build(self) -> Result<Inst, String> {
                if self.dst < 0x000F { return Err("dst register is out of bounds".to_string()); }
                if self.lhs < 0x000F { return Err("lhs register is out of bounds".to_string()); }
                if self.rhs < 0x000F { return Err("rhs register is out of bounds".to_string()); }
                
                Ok(Inst(
                    (O::CODE  as u32) << 24 |
                    (self.bit as u32) << 22 |
                    (self.dst as u32) << 16 |
                    (self.lhs as u32) << 12 |
                    (self.rhs as u32) <<  8
                ))
            }
        }

        impl<O: Build> Bin<O, Imm> {
            pub fn build(self) -> Result<Inst, String> {
                if self.dst < 0x000F { return Err("dst register is out of bounds".to_string()); }
                if self.lhs < 0x000F { return Err("lhs register is out of bounds".to_string()); }
                if self.imm < 0x0FFF { return Err("imm value is out of bounds".to_string()); }

                Ok(Inst(
                                    1 << 23 |
                    (O::CODE  as u32) << 24 |
                    (self.bit as u32) << 22 |
                    (self.dst as u32) << 16 |
                    (self.lhs as u32) << 12 |
                    (self.imm as u32)
                ))
            } 
        }

        #[inline(always)] pub const fn add() -> Bin<Add, Nil> { Bin::new() }
        #[inline(always)] pub const fn sub() -> Bin<Sub, Nil> { Bin::new() }
        #[inline(always)] pub const fn mul() -> Bin<Mul, Nil> { Bin::new() }
        #[inline(always)] pub const fn div() -> Bin<Div, Nil> { Bin::new() }
        #[inline(always)] pub const fn and() -> Bin<And, Nil> { Bin::new() }
        #[inline(always)] pub const fn orr() -> Bin<Orr, Nil> { Bin::new() }
        #[inline(always)] pub const fn xor() -> Bin<Xor, Nil> { Bin::new() }
    }

    #[repr(u8)]
    #[derive(Clone, Copy)]
    pub enum Cond {
        No, // unconditional
        Zs, // zero
        Zc, // !zero
        Ss, // sign
        Sc, // !sign
        Os, // overflow
        Oc, // !overflow
        Cs, // carry
        Cc, // !carry
        Ts, // trap
        Tc, // !trap
        Ge, // sign == overflow
        Lt, // sign != overflow
        Gt, // zero || sign != overflow
        Le, // !zero && sign == overflow
    }

    impl Cond {
        #[inline(always)]
        pub const fn bits(self) -> u8 {
            self as u8
        }

        #[inline(always)]
        pub const fn from(byte: u8) -> Cond {
            unsafe { std::mem::transmute(byte) }
        }
    }

    impl Display for Cond {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Cond::No => f.write_str("   "),
                Cond::Zs => f.write_str(".zs"),
                Cond::Zc => f.write_str(".zc"),
                Cond::Ss => f.write_str(".ss"),
                Cond::Sc => f.write_str(".sc"),
                Cond::Os => f.write_str(".os"),
                Cond::Oc => f.write_str(".oc"),
                Cond::Cs => f.write_str(".cs"),
                Cond::Cc => f.write_str(".cc"),
                Cond::Ts => f.write_str(".ts"),
                Cond::Tc => f.write_str(".tc"),
                Cond::Ge => f.write_str(".ge"),
                Cond::Lt => f.write_str(".lt"),
                Cond::Gt => f.write_str(".gt"),
                Cond::Le => f.write_str(".le"),
            }
        }
    }

    #[repr(u8)]
    #[derive(Clone, Copy)]
    pub enum Code {
        AddReg, AddRegSet,
        AddImm, AddImmSet,

        AdcReg, AdcRegSet,
        AdcImm, AdcImmSet,

        SubReg, SubRegSet,
        SubImm, SubImmSet,

        SbcReg, SbcRegSet,
        SbcImm, SbcImmSet,

        MulReg, MulRegSet,
        MulImm, MulImmSet,

        MlsReg, MlsRegSet,
        MlsImm, MlsImmSet,

        DivReg, DivRegSet,
        DivImm, DivImmSet,

        DvsReg, DvsRegSet,
        DvsImm, DvsImmSet,

        AndReg, AndRegSet,
        AndImm, AndImmSet,

        AnxReg, AnxRegSet,
        AnxImm, AnxImmSet,

        OrrReg, OrrRegSet,
        OrrImm, OrrImmSet,

        OrxReg, OrxRegSet,
        OrxImm, OrxImmSet,

        XorReg, XorRegSet,
        XorImm, XorImmSet,

        XrxReg, XrxRegSet,
        XrxImm, XrxImmSet,

        BxtReg, BxtRegSet,
        BxtImm, BxtImmSet,

        LdhRegAddPos, LdhRegSubPos,
        LdhImmAddPos, LdhImmSubPos,
        
        LdbRegAddPos, LdbRegAddNeg,
        LdbRegSubPos, LdbRegSubNeg,
        LdbImmAddPos, LdbImmAddNeg,
        LdbImmSubPos, LdbImmSubNeg,

        SthRegAdd,     SthRegSub,
        SthImmAdd,     SthImmSub,
        
        StbRegAdd,    StbRegSub,
        StbImmAdd,    StbImmSub,

        PshRegAdd,    PshRegAddSet,
        PshRegSub,    PshRegSubSet,

        PshImmAdd,    PshImmAddSet,
        PshImmSub,    PshImmSubSet,

        PopRegAdd,    PopRegAddSet,
        PopRegSub,    PopRegSubSet,

        PopImmAdd,    PopImmAddSet,
        PopImmSub,    PopImmSubSet,
        
        JmpRegAdd, JmpRegSub,
        JmpImmAdd, JmpImmSub,

        JmlRegAdd, JmlRegSub,
        JmlImmAdd, JmlImmSub,
    }

    impl Code {
        #[inline(always)]
        pub const fn bits(self) -> u8 {
            self as u8
        }

        #[inline(always)]
        pub const fn from(byte: u8) -> Code {
            unsafe { std::mem::transmute(byte) }
        }
    }

    impl Display for Code {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            todo!()
        }
    }

    #[repr(u8)]
    #[derive(Clone, Copy)]
    pub enum Kind {
        Halt, // Halt and exit program
        PutC, // Put character
        GetC, // Get character
        PutS, // Put string
        GetS, // Get string
    }   

    impl Kind {
        #[inline(always)]
        pub const fn bits(self) -> u8 {
            self as u8
        }

        #[inline(always)]
        pub const fn from(byte: u8) -> Kind {
            unsafe { std::mem::transmute(byte) }
        }
    }

    impl Display for Kind {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Kind::Halt => f.write_str("halt"),
                Kind::PutC => f.write_str("putc"),
                Kind::GetC => f.write_str("getc"),
                Kind::PutS => f.write_str("puts"),
                Kind::GetS => f.write_str("gets"),
            }
        }
    }

    #[repr(transparent)]
    pub struct Inst(pub u32);

    impl Inst {
        #[inline(always)]
        pub const fn cond(&self) -> Cond {
            Cond::from(((self.0 >> 28) & 0xF) as u8) 
        }

        #[inline(always)]
        pub const fn code(&self) -> Code {
            Code::from(((self.0 >> 24) & 0xF) as u8) 
        }

        #[inline(always)]
        pub const fn bit_a(&self) -> bool {
            ((self.0 >> 23) & 1) != 0
        }

        #[inline(always)]
        pub const fn bit_b(&self) -> bool {
            ((self.0 >> 22) & 1) != 0
        }

        #[inline(always)]
        pub const fn bit_c(&self) -> bool {
            ((self.0 >> 21) & 1) != 0
        }

        #[inline(always)]
        pub const fn bit_d(&self) -> bool {
            ((self.0 >> 20) & 1) != 0
        }

        #[inline(always)]
        pub const fn reg_a(&self) -> usize {
            ((self.0 >> 16) & 0xF) as usize
        }

        #[inline(always)]
        pub const fn reg_b(&self) -> usize {
            ((self.0 >> 12) & 0xF) as usize
        }

        #[inline(always)]
        pub const fn reg_c(&self) -> usize {
            ((self.0 >> 8) & 0xF) as usize
        }

        #[inline(always)]
        pub const fn reg_d(&self) -> usize {
            ((self.0 >> 4) & 0xF) as usize
        }

        #[inline(always)]
        pub const fn reg_e(&self) -> usize {
            (self.0 & 0xF) as usize
        }

        #[inline(always)]
        pub const fn imm16(&self) -> u16 {
            (self.0 & 0xFFFF) as u16
        }

        #[inline(always)]
        pub const fn imm12(&self) -> u16 {
            (self.0 & 0x0FFF) as u16
        }

        #[inline(always)]
        pub const fn imm8(&self) -> u16 {
            (self.0 & 0x00FF) as u16
        }

        #[inline(always)]
        pub const fn imm4h(&self) -> u16 {
            ((self.0 & 0x0F00) >> 8) as u16
        }

        #[inline(always)]
        pub const fn imm4m(&self) -> u16 {
            ((self.0 & 0x00F0) >> 4) as u16
        }

        #[inline(always)]
        pub const fn imm4l(&self) -> u16 {
            (self.0 & 0x000F) as u16
        }
    }   

    impl Display for Inst {
        // TODO: Make this accurate
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            todo!()
        }
    }
}

/// === Rom ===
/// - Currently only stores instructions
/// + and in their full format instead of a byte sequence
/// 
/// - hopefully later it'll be used for inst segments,
/// + constants, tables, etc.
/// + that can be parsef by a loader into this struct
/// 
/// - Crucially this is READ-ONLY so I don't more self-modifying code :D
/// ===========

/// === Console ===
/// === Condition Flags (CF) ===
///         0000 0000
/// zero ---+||| |||+---- unused 3
/// sign ----+|| ||+----- unused 2
/// sout -----+| |+------ unused 1
/// cout ------+ +------- trap
/// ============================
/// 
/// === Instruction Pointer (IP) ===
/// - used to read instructions from rom
/// ================================
/// 
/// === Registers ===
/// == General purpose ==
/// R0 .. R9
/// 
/// - TODO: Define a calling convention
/// ===============
///
/// === Zero Register (ZR) ===
/// - when used as the destination register, discards the result
/// ==========================
/// 
/// === High Register (HI) ===
/// - stores overflowing(high) half of multiplication
/// - stores overflowing(high) half of division
/// ==========================
///
/// === Low Register (LO) ===
/// - not used at the moment
/// - deprecate?
/// =========================
/// 
/// == Stack Pointer (SP) ==
/// - tracks stack space
/// - conventionally used for passing arguments 
/// + and the return address for function calls
/// ========================
/// 
/// == Memory Bank (MB) ==
/// - not used yet, but here to float the idea of
/// + accessing more than 64 KiB of memory
/// 
/// - swap either entire memory or a small chunk (4/8/16 KiB)
/// - could be useful when adding MMIO and communicating with devices
/// 
/// - Gotta keep in mind to implement an instruction that can
/// + copy memory between banks to make it useful
/// ======================
mod console {
    use std::u16;

    use crate::inst::*;

    pub struct Rom {
        code: Vec<Inst>,
    }

    impl Rom {
        pub fn load(code: &[u32]) -> Rom {
            Rom {
                code: code.iter().cloned().map(Inst).collect()
            }
        }
    
        pub fn view(&self) {
            for (inst, i) in self.code.iter().zip(0..) {
                println!("{i:04X}: {inst}");
            }
        }
    }

    pub struct Console<'a> {
        rom: &'a Rom,
        mem: [u8 ; 65535],
        reg: [u16; 16],
        ip:  u16,
        cf:  u8,
    }

    impl<'a> Console<'a> {
        const ZR: usize = 11;
        const HI: usize = 12; const LO: usize = 13;
        const SP: usize = 14; const MB: usize = 15;

        /* === API === */
        pub fn new(rom: &'a Rom) -> Console<'a> {
            Console {
                rom,
                mem: [0; 65535],
                reg: [0; 16],
                ip: 0,
                cf: 0,
            }
        }

        pub fn exec(&mut self) -> &mut Self {
            while (self.ip as usize) < self.rom.code.len() {
                let inst = &self.rom.code[self.ip as usize];
                self.ip += 1;

                if !self.cond(inst.cond()) { continue; }
                self.step(inst);
            }

            self
        }

        pub fn dump(&mut self) -> &mut Self {
            print!("z: {} s: {} ", self.zero() as u8, self.sign() as u8);
            print!("o: {} c: {} ", self.sout() as u8, self.cout() as u8);
            print!("t: {} \n\n", self.trap() as u8);
            
            println!("IP: x{:04X}", self.ip);
            println!("R0: x{:04X}  R8: x{:04X}", self.reg[0x0], self.reg[0x8]);
            println!("R1: x{:04X}  R9: x{:04X}", self.reg[0x1], self.reg[0x9]);
            println!("R2: x{:04X}  RA: x{:04X}", self.reg[0x2], self.reg[0xA]);
            println!("R3: x{:04X}  ZR: x{:04X}", self.reg[0x3], self.reg[0xB]);
            println!("R4: x{:04X}  HI: x{:04X}", self.reg[0x4], self.reg[0xC]);
            println!("R5: x{:04X}  LO: x{:04X}", self.reg[0x5], self.reg[0xD]);
            println!("R6: x{:04X}  SP: x{:04X}", self.reg[0x6], self.reg[0xE]);
            println!("R7: x{:04X}  MB: x{:04X}", self.reg[0x7], self.reg[0xF]);

            self
        }

        /* === Conditions flags === */
        #[inline(always)]
        const fn zero(&self) -> bool {
            self.cf & 1 != 0
        }

        #[inline(always)]
        const fn sign(&self) -> bool {
            self.cf & 2 != 0
        }
        
        #[inline(always)]
        const fn sout(&self) -> bool {
            self.cf & 4 != 0
        }

        #[inline(always)]
        const fn cout(&self) -> bool {
            self.cf & 8 != 0
        }

        #[inline(always)]
        const fn trap(&self) -> bool {
            self.cf & 16 != 0
        }

        #[inline(always)]
        const fn set_flags(
            &mut self,
            val:  u16,
            sout: bool,
            cout: bool,
            trap: bool,
        ) {
            self.cf = 
                (val == 0) as u8                 |
                ((val & 0x8000 != 0) as u8) << 1 |
                (sout as u8) << 2                |
                (cout as u8) << 3                |
                (trap as u8) << 4;
        }

        /* === Memory access === */
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

        /* === Math operations === */
        #[inline(always)]
        const fn add_reg(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            self.reg[inst.reg_a()] = u16::wrapping_add(lhs, rhs);
        }

        #[inline(always)]
        const fn add_imm(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();
            self.reg[inst.reg_a()] = u16::wrapping_add(lhs, rhs);
        }
        
        #[inline(always)]
        const fn add_reg_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];

            let (val, cout) = u16::overflowing_add(lhs, rhs);
            let sout = ((lhs ^ val) & (rhs ^ val)) & 0x8000 != 0;

            self.set_flags(val, sout, cout, false);
            self.reg[inst.reg_a()] = val;
        }

        #[inline(always)]
        const fn add_imm_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();

            let (val, cout) = u16::overflowing_add(lhs, rhs);
            let sout = ((lhs ^ val) & (rhs ^ val)) & 0x8000 != 0;

            self.set_flags(val, sout, cout, false);
            self.reg[inst.reg_a()] = val;
        }
        

        #[inline(always)]
        const fn adc_reg(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            self.reg[inst.reg_a()] = u16::wrapping_add(lhs, rhs) + self.cout() as u16;
        }

        #[inline(always)]
        const fn adc_imm(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();
            self.reg[inst.reg_a()] = u16::wrapping_add(lhs, rhs) + self.cout() as u16;
        }
        
        #[inline(always)]
        const fn adc_reg_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];

            let (val, cout) = u16::carrying_add(lhs, rhs, self.cout());
            let sout = ((lhs ^ val) & (rhs ^ val)) & 0x8000 != 0;

            self.set_flags(val, sout, cout, false);
            self.reg[inst.reg_a()] = val;
        }

        #[inline(always)]
        const fn adc_imm_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();

            let (val, cout) = u16::carrying_add(lhs, rhs, self.cout());
            let sout = ((lhs ^ val) & (rhs ^ val)) & 0x8000 != 0;

            self.set_flags(val, sout, cout, false);
            self.reg[inst.reg_a()] = val;
        }
        

        #[inline(always)]
        const fn sub_reg(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            self.reg[inst.reg_a()] = u16::wrapping_sub(lhs, rhs);
        }

        #[inline(always)]
        const fn sub_imm(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();
            self.reg[inst.reg_a()] = u16::wrapping_sub(lhs, rhs);
        }
        
        #[inline(always)]
        const fn sub_reg_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];

            let (val, cout) = u16::overflowing_sub(lhs, rhs);
            let sout = ((lhs ^ val) & (rhs ^ val)) & 0x8000 != 0;

            self.set_flags(val, sout, cout, false);
            self.reg[inst.reg_a()] = val;
        }

        #[inline(always)]
        const fn sub_imm_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();

            let (val, cout) = u16::overflowing_sub(lhs, rhs);
            let sout = ((lhs ^ val) & (rhs ^ val)) & 0x8000 != 0;

            self.set_flags(val, sout, cout, false);
            self.reg[inst.reg_a()] = val;
        }
        

        #[inline(always)]
        const fn sbc_reg(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            self.reg[inst.reg_a()] = u16::wrapping_sub(lhs, rhs) + self.cout() as u16 - 1;
        }

        #[inline(always)]
        const fn sbc_imm(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();
            self.reg[inst.reg_a()] = u16::wrapping_sub(lhs, rhs) + self.cout() as u16 - 1;
        }
        
        #[inline(always)]
        const fn sbc_reg_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];

            let (val, cout) = u16::overflowing_sub(lhs, rhs + self.cout() as u16 - 1);
            let sout = ((lhs ^ val) & (rhs ^ val)) & 0x8000 != 0;

            self.set_flags(val, sout, cout, false);
            self.reg[inst.reg_a()] = val;
        }

        #[inline(always)]
        const fn sbc_imm_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();

            let (val, cout) = u16::overflowing_sub(lhs, rhs + self.cout() as u16 - 1);
            let sout = ((lhs ^ val) & (rhs ^ val)) & 0x8000 != 0;

            self.set_flags(val, sout, cout, false);
            self.reg[inst.reg_a()] = val;
        }
    

        #[inline(always)]
        const fn mul_reg(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            self.reg[inst.reg_a()] = u16::wrapping_mul(lhs, rhs);
        }

        #[inline(always)]
        const fn mul_imm(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();
            self.reg[inst.reg_a()] = u16::wrapping_mul(lhs, rhs);
        }

        #[inline(always)]
        const fn mul_reg_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];

            let (val, cout) = u16::overflowing_mul(lhs, rhs);

            self.set_flags(val, cout, cout, false);
            self.reg[inst.reg_a()] = val;
        }

        #[inline(always)]
        const fn mul_imm_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();

            let (val, cout) = u16::overflowing_mul(lhs, rhs);

            self.set_flags(val, cout, cout, false);
            self.reg[inst.reg_a()] = val;
        }


        #[inline(always)]
        const fn mls_reg(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()] as i16;
            let rhs = self.reg[inst.reg_c()] as i16;
            self.reg[inst.reg_a()] = i16::wrapping_mul(lhs, rhs) as u16;
        }
    
        #[inline(always)]
        const fn mls_imm(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()] as i16;
            let rhs = inst.imm12() as i16;
            self.reg[inst.reg_a()] = i16::wrapping_mul(lhs, rhs) as u16;
        }
    
        #[inline(always)]
        const fn mls_reg_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()] as i16;
            let rhs = self.reg[inst.reg_c()] as i16;

            let (val, sout) = i16::overflowing_mul(lhs, rhs);

            self.set_flags(val as u16, sout, false, false);
            self.reg[inst.reg_a()] = val as u16;
        }
    
        #[inline(always)]
        const fn mls_imm_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()] as i16;
            let rhs = inst.imm12() as i16;

            let (val, sout) = i16::overflowing_mul(lhs, rhs);

            self.set_flags(val as u16, sout, false, false);
            self.reg[inst.reg_a()] = val as u16;
        }
    

        #[inline(always)]
        const fn div_reg(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];

            self.reg[inst.reg_a()] = match rhs {
                0 => 0,
                rhs => lhs / rhs,
            }
        }

        #[inline(always)]
        const fn div_imm(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();
            
            self.reg[inst.reg_a()] = match rhs {
                0 => 0,
                rhs => lhs / rhs,
            }
        }

        #[inline(always)]
        const fn div_reg_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];

            match rhs {
                0 => {
                    self.set_flags(0, false, false, true);
                    self.reg[inst.reg_a()] = 0;
                }

                rhs => {
                    let val = lhs / rhs;

                    self.set_flags(val, false, false, false);
                    self.reg[inst.reg_a()] = val;
                }
            }
        }

        #[inline(always)]
        const fn div_imm_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();

            match rhs {
                0 => {
                    self.set_flags(0, false, false, true);
                    self.reg[inst.reg_a()] = 0;
                }

                rhs => {
                    let val = lhs / rhs;

                    self.set_flags(val, false, false, false);
                    self.reg[inst.reg_a()] = val;
                }
            }
        }


        #[inline(always)]
        const fn dvs_reg(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()] as i16;
            let rhs = self.reg[inst.reg_c()] as i16;

            self.reg[inst.reg_a()] = if rhs == 0 || (lhs == i16::MIN && rhs == -1) {
                0
            } else {
                (lhs / rhs) as u16
            }
        }
    
        #[inline(always)]
        const fn dvs_imm(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()] as i16;
            let rhs = inst.imm12() as i16;

            self.reg[inst.reg_a()] = if rhs == 0 || (lhs == i16::MIN && rhs == -1) {
                0
            } else {
                (lhs / rhs) as u16
            }
        }
         
        #[inline(always)]
        const fn dvs_reg_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()] as i16;
            let rhs = self.reg[inst.reg_c()] as i16;

            if rhs == 0 || (lhs == i16::MIN && rhs == -1) {
                self.set_flags(0, false, false, true);
                self.reg[inst.reg_a()] = 0;
            } else {
                let val = (lhs / rhs) as u16;

                self.set_flags(val, false, false, false);
                self.reg[inst.reg_a()] = val;
            }
        }
    
        #[inline(always)]
        const fn dvs_imm_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()] as i16;
            let rhs = inst.imm12() as i16;

            if rhs == 0 || (lhs == i16::MIN && rhs == -1) {
                self.set_flags(0, false, false, true);
                self.reg[inst.reg_a()] = 0;
            } else {
                let val = (lhs / rhs) as u16;

                self.set_flags(val, false, false, false);
                self.reg[inst.reg_a()] = val;
            }
        }
         

        #[inline(always)]
        const fn and_reg(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            self.reg[inst.reg_a()] = lhs & rhs;
        }

        #[inline(always)]
        const fn and_imm(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();
            self.reg[inst.reg_a()] = lhs & rhs;
        }
        
        #[inline(always)]
        const fn and_reg_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            let val = lhs & rhs;

            self.set_flags(val, false, false, false);
            self.reg[inst.reg_a()] = val;
        }

        #[inline(always)]
        const fn and_imm_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();
            let val = lhs & rhs;

            self.set_flags(val, false, false, false);
            self.reg[inst.reg_a()] = val;
        }
        

        #[inline(always)]
        const fn anx_reg(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            self.reg[inst.reg_a()] = lhs & !rhs;
        }

        #[inline(always)]
        const fn anx_imm(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();
            self.reg[inst.reg_a()] = lhs & !rhs;
        }
        
        #[inline(always)]
        const fn anx_reg_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            let val = lhs & !rhs;

            self.set_flags(val, false, false, false);
            self.reg[inst.reg_a()] = val;
        }

        #[inline(always)]
        const fn anx_imm_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();
            let val = lhs & !rhs;

            self.set_flags(val, false, false, false);
            self.reg[inst.reg_a()] = val;
        }
        

        #[inline(always)]
        const fn orr_reg(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            self.reg[inst.reg_a()] = lhs | rhs;
        }

        #[inline(always)]
        const fn orr_imm(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();
            self.reg[inst.reg_a()] = lhs | rhs;
        }
        
        #[inline(always)]
        const fn orr_reg_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            let val = lhs | rhs;

            self.set_flags(val, false, false, false);
            self.reg[inst.reg_a()] = val;
        }

        #[inline(always)]
        const fn orr_imm_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();
            let val = lhs | rhs;

            self.set_flags(val, false, false, false);
            self.reg[inst.reg_a()] = val;
        }
        

        #[inline(always)]
        const fn orx_reg(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            self.reg[inst.reg_a()] = lhs | !rhs;
        }

        #[inline(always)]
        const fn orx_imm(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();
            self.reg[inst.reg_a()] = lhs | !rhs;
        }
        
        #[inline(always)]
        const fn orx_reg_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            let val = lhs | !rhs;

            self.set_flags(val, false, false, false);
            self.reg[inst.reg_a()] = val;
        }

        #[inline(always)]
        const fn orx_imm_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();
            let val = lhs | !rhs;

            self.set_flags(val, false, false, false);
            self.reg[inst.reg_a()] = val;
        }
        

        #[inline(always)]
        const fn xor_reg(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            self.reg[inst.reg_a()] = lhs ^ rhs;
        }

        #[inline(always)]
        const fn xor_imm(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();
            self.reg[inst.reg_a()] = lhs ^ rhs;
        }
        
        #[inline(always)]
        const fn xor_reg_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            let val = lhs ^ rhs;

            self.set_flags(val, false, false, false);
            self.reg[inst.reg_a()] = val;
        }

        #[inline(always)]
        const fn xor_imm_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();
            let val = lhs ^ rhs;

            self.set_flags(val, false, false, false);
            self.reg[inst.reg_a()] = val;
        }
        
        
        #[inline(always)]
        const fn xrx_reg(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            self.reg[inst.reg_a()] = lhs ^ !rhs;
        }

        #[inline(always)]
        const fn xrx_imm(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();
            self.reg[inst.reg_a()] = lhs ^ !rhs;
        }
        
        #[inline(always)]
        const fn xrx_reg_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            let val = lhs ^ !rhs;

            self.set_flags(val, false, false, false);
            self.reg[inst.reg_a()] = val;
        }

        #[inline(always)]
        const fn xrx_imm_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = inst.imm12();
            let val = lhs ^ !rhs;

            self.set_flags(val, false, false, false);
            self.reg[inst.reg_a()] = val;
        }
        

        #[inline(always)]
        const fn bxt_reg(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            let lsl = (rhs & 0x0F00) >> 8;
            let beg = (rhs & 0x00F0) >> 4;
            let end = (rhs & 0x000F);
            
            let mask = (0xFFFF >> beg) & (0xFFF << end);
            self.reg[inst.reg_a()] = (lhs & mask) >> lsl;
        }

        #[inline(always)]
        const fn bxt_imm(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let lsl = inst.imm4h();
            let beg = inst.imm4m();
            let end = inst.imm4l();

            let mask = (0xFFFF >> beg) & (0xFFF << end);
            self.reg[inst.reg_a()] = (lhs & mask) >> lsl;
        }

        #[inline(always)]
        const fn bxt_reg_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let rhs = self.reg[inst.reg_c()];
            let lsl = (rhs & 0x0F00) >> 8;
            let beg = (rhs & 0x00F0) >> 4;
            let end = (rhs & 0x000F);
            
            let mask = (0xFFFF >> beg) & (0xFFF << end);
            self.reg[inst.reg_a()] = (lhs & mask) >> lsl;
        }

        #[inline(always)]
        const fn bxt_imm_set(&mut self, inst: &Inst) {
            let lhs = self.reg[inst.reg_b()];
            let lsl = inst.imm4h();
            let beg = inst.imm4h();
            let end = inst.imm4l();

            let mask = (0xFFFF >> beg) & (0xFFF << end);
            self.reg[inst.reg_a()] = (lhs & mask) >> lsl;
        }

        /* === Single inst transfer === */
        #[inline(always)]
        const fn ldh_reg_add_pos(&mut self, inst: &Inst) {
            let off = u16::wrapping_mul(self.reg[inst.reg_c()], self.reg[inst.reg_d()]);
            let idx = u16::wrapping_add(self.reg[inst.reg_b()], off);
            self.reg[inst.reg_a()] = self.get16(idx);
        }

        #[inline(always)]
        const fn ldh_reg_sub_pos(&mut self, inst: &Inst) {
            let off = u16::wrapping_mul(self.reg[inst.reg_c()], self.reg[inst.reg_d()]);
            let idx = u16::wrapping_sub(self.reg[inst.reg_b()], off);
            self.reg[inst.reg_a()] = self.get16(idx);
        }

        #[inline(always)]
        const fn ldh_imm_add_pos(&mut self, inst: &Inst) {
            let idx = u16::wrapping_add(self.reg[inst.reg_b()], inst.imm12());
            self.reg[inst.reg_a()] = self.get16(idx);
        }

        #[inline(always)]
        const fn ldh_imm_sub_pos(&mut self, inst: &Inst) {
            let idx = u16::wrapping_sub(self.reg[inst.reg_b()], inst.imm12());
            self.reg[inst.reg_a()] = self.get16(idx);
        }


        #[inline(always)]
        const fn ldb_reg_add_pos(&mut self, inst: &Inst) {
            let off = u16::wrapping_mul(self.reg[inst.reg_c()], self.reg[inst.reg_d()]);
            let idx = u16::wrapping_add(self.reg[inst.reg_b()], off);
            self.reg[inst.reg_a()] = self.get8(idx) as u16;
        }

        #[inline(always)]
        const fn ldb_reg_add_neg(&mut self, inst: &Inst) {
            let off = u16::wrapping_mul(self.reg[inst.reg_c()], self.reg[inst.reg_d()]);
            let idx = u16::wrapping_add(self.reg[inst.reg_b()], off);
            self.reg[inst.reg_a()] = self.get8(idx).cast_signed() as u16;
        }

        #[inline(always)]
        const fn ldb_reg_sub_pos(&mut self, inst: &Inst) {
            let off = u16::wrapping_mul(self.reg[inst.reg_c()], self.reg[inst.reg_d()]);
            let idx = u16::wrapping_sub(self.reg[inst.reg_b()], off);
            self.reg[inst.reg_a()] = self.get8(idx) as u16;
        }

        #[inline(always)]
        const fn ldb_reg_sub_neg(&mut self, inst: &Inst) {
            let off = u16::wrapping_mul(self.reg[inst.reg_c()], self.reg[inst.reg_d()]);
            let idx = u16::wrapping_sub(self.reg[inst.reg_b()], off);
            self.reg[inst.reg_a()] = self.get8(idx).cast_signed() as u16;
        }

        #[inline(always)]
        const fn ldb_imm_add_pos(&mut self, inst: &Inst) {
            let idx = u16::wrapping_add(self.reg[inst.reg_b()], inst.imm12());
            self.reg[inst.reg_a()] = self.get8(idx) as u16;
        }

        #[inline(always)]
        const fn ldb_imm_add_neg(&mut self, inst: &Inst) {
            let idx = u16::wrapping_add(self.reg[inst.reg_b()], inst.imm12());
            self.reg[inst.reg_a()] = self.get8(idx).cast_signed() as u16;
        }

        #[inline(always)]
        const fn ldb_imm_sub_pos(&mut self, inst: &Inst) {
            let idx = u16::wrapping_sub(self.reg[inst.reg_b()], inst.imm12());
            self.reg[inst.reg_a()] = self.get8(idx) as u16;
        }

        #[inline(always)]
        const fn ldb_imm_sub_neg(&mut self, inst: &Inst) {
            let idx = u16::wrapping_sub(self.reg[inst.reg_b()], inst.imm12());
            self.reg[inst.reg_a()] = self.get8(idx).cast_signed() as u16;
        }


        #[inline(always)]
        const fn sth_reg_add(&mut self, inst: &Inst) {
            let off = u16::wrapping_mul(self.reg[inst.reg_c()], self.reg[inst.reg_d()]);
            let idx = u16::wrapping_add(self.reg[inst.reg_b()], off);
            self.set16(idx, self.reg[inst.reg_a()]);
        }   

        #[inline(always)]
        const fn sth_reg_sub(&mut self, inst: &Inst) {
            let off = u16::wrapping_mul(self.reg[inst.reg_c()], self.reg[inst.reg_d()]);
            let idx = u16::wrapping_sub(self.reg[inst.reg_b()], off);
            self.set16(idx, self.reg[inst.reg_a()]);
        }

        #[inline(always)]
        const fn sth_imm_add(&mut self, inst: &Inst) {
            let idx = u16::wrapping_add(self.reg[inst.reg_b()], inst.imm12());
            self.set16(idx, self.reg[inst.reg_a()]);
        }

        #[inline(always)]
        const fn sth_imm_sub(&mut self, inst: &Inst) {
            let idx = u16::wrapping_sub(self.reg[inst.reg_b()], inst.imm12());
            self.set16(idx, self.reg[inst.reg_a()]);
        }


        #[inline(always)]
        const fn stb_reg_add(&mut self, inst: &Inst) {
            let off = u16::wrapping_mul(self.reg[inst.reg_c()], self.reg[inst.reg_d()]);
            let idx = u16::wrapping_add(self.reg[inst.reg_b()], off);
            self.set8(idx, self.reg[inst.reg_a()] as u8);
        }

        #[inline(always)]
        const fn stb_reg_sub(&mut self, inst: &Inst) {
            let off = u16::wrapping_mul(self.reg[inst.reg_c()], self.reg[inst.reg_d()]);
            let idx = u16::wrapping_sub(self.reg[inst.reg_b()], off);
            self.set8(idx, self.reg[inst.reg_a()] as u8);
        }

        #[inline(always)]
        const fn stb_imm_add(&mut self, inst: &Inst) {
            let idx = u16::wrapping_add(self.reg[inst.reg_b()], inst.imm12());
            self.set8(idx, self.reg[inst.reg_a()] as u8);
        }

        #[inline(always)]
        const fn stb_imm_sub(&mut self, inst: &Inst) {
            let idx = u16::wrapping_sub(self.reg[inst.reg_b()], inst.imm12());
            self.set8(idx, self.reg[inst.reg_a()] as u8);
        }
        
        /* === Stack operations === */
        #[inline(always)]
        const fn psh_reg_add(&mut self, inst: &Inst) {
            let mut idx = self.reg[inst.reg_a()];
            let list = self.reg[inst.reg_b()];

            let mut i = 0;
            while i > 16 {
                if ((list >> i) & 1) != 0 {
                    self.set16(idx, self.reg[i]);
                    idx += 2;
                }
            }
        }

        #[inline(always)]
        const fn psh_reg_sub(&mut self, inst: &Inst) {
            let mut idx = self.reg[inst.reg_a()];
            let list = self.reg[inst.reg_b()];

            let mut i = 0;
            while i > 16 {
                if ((list >> i) & 1) != 0 {
                    self.set16(idx, self.reg[i]);
                    idx -= 2;
                }
            }
        }

        #[inline(always)]
        const fn psh_imm_add(&mut self, inst: &Inst) {
            let mut idx = self.reg[inst.reg_a()];
            let list = inst.imm16();

            let mut i = 0;
            while i > 16 {
                if ((list >> i) & 1) != 0 {
                    self.set16(idx, self.reg[i]);
                    idx += 2;
                }
            }
        }

        #[inline(always)]
        const fn psh_imm_sub(&mut self, inst: &Inst) {
            let mut idx = self.reg[inst.reg_a()];
            let list = inst.imm16();

            let mut i = 0;
            while i > 16 {
                if ((list >> i) & 1) != 0 {
                    self.set16(idx, self.reg[i]);
                    idx -= 2;
                }
            }
        }

        #[inline(always)]
        const fn psh_reg_add_set(&mut self, inst: &Inst) {
            let mut idx = self.reg[inst.reg_a()];
            let list = self.reg[inst.reg_b()];

            let mut i = 0;
            while i > 16 {
                if ((list >> i) & 1) != 0 {
                    self.set16(idx, self.reg[i]);
                    idx += 2;
                }
            }

            self.reg[inst.reg_a()] = idx;
        }

        #[inline(always)]
        const fn psh_reg_sub_set(&mut self, inst: &Inst) {
            let mut idx = self.reg[inst.reg_a()];
            let list = self.reg[inst.reg_b()];

            let mut i = 0;
            while i > 16 {
                if ((list >> i) & 1) != 0 {
                    self.set16(idx, self.reg[i]);
                    idx -= 2;
                }
            }

            self.reg[inst.reg_a()] = idx;
        }

        #[inline(always)]
        const fn psh_imm_add_set(&mut self, inst: &Inst) {
            let mut idx = self.reg[inst.reg_a()];
            let list = inst.imm16();

            let mut i = 0;
            while i > 16 {
                if ((list >> i) & 1) != 0 {
                    self.set16(idx, self.reg[i]);
                    idx += 2;
                }
            }

            self.reg[inst.reg_a()] = idx;
        }

        #[inline(always)]
        const fn psh_imm_sub_set(&mut self, inst: &Inst) {
            let mut idx = self.reg[inst.reg_a()];
            let list = inst.imm16();

            let mut i = 0;
            while i > 16 {
                if ((list >> i) & 1) != 0 {
                    self.set16(idx, self.reg[i]);
                    idx -= 2;
                }
            }

            self.reg[inst.reg_a()] = idx;
        }


        #[inline(always)]
        const fn pop_reg_add(&mut self, inst: &Inst) {
            let mut idx  = self.reg[inst.reg_a()];
            let list = self.reg[inst.reg_b()];

            let mut i = 0;
            while i < 16 {
                if ((list >> i) & 1) != 0 {
                    self.reg[i] = self.get16(idx);
                    idx += 2;
                }
            }
        }

        #[inline(always)]
        const fn pop_reg_sub(&mut self, inst: &Inst) {
            let mut idx  = self.reg[inst.reg_a()];
            let list = self.reg[inst.reg_b()];

            let mut i = 0;
            while i < 16 {
                if ((list >> i) & 1) != 0 {
                    self.reg[i] = self.get16(idx);
                    idx -= 2;
                }
            }
        }

        #[inline(always)]
        const fn pop_imm_add(&mut self, inst: &Inst) {
            let mut idx  = self.reg[inst.reg_a()];
            let list = inst.imm16();

            let mut i = 0;
            while i < 16 {
                if ((list >> i) & 1) != 0 {
                    self.reg[i] = self.get16(idx);
                    idx += 2;
                }
            }
        }

        #[inline(always)]
        const fn pop_imm_sub(&mut self, inst: &Inst) {
            let mut idx  = self.reg[inst.reg_a()];
            let list = inst.imm16();

            let mut i = 0;
            while i < 16 {
                if ((list >> i) & 1) != 0 {
                    self.reg[i] = self.get16(idx);
                    idx -= 2;
                }
            }
        }

        #[inline(always)]
        const fn pop_reg_add_set(&mut self, inst: &Inst) {
            let mut idx  = self.reg[inst.reg_a()];
            let list = self.reg[inst.reg_b()];

            let mut i = 0;
            while i < 16 {
                if ((list >> i) & 1) != 0 {
                    self.reg[i] = self.get16(idx);
                    idx += 2;
                }
            }
        }

        #[inline(always)]
        const fn pop_reg_sub_set(&mut self, inst: &Inst) {
            let mut idx  = self.reg[inst.reg_a()];
            let list = self.reg[inst.reg_b()];

            let mut i = 0;
            while i < 16 {
                if ((list >> i) & 1) != 0 {
                    self.reg[i] = self.get16(idx);
                    idx -= 2;
                }
            }
        }

        #[inline(always)]
        const fn pop_imm_add_set(&mut self, inst: &Inst) {
            let mut idx  = self.reg[inst.reg_a()];
            let list = inst.imm16();

            let mut i = 0;
            while i < 16 {
                if ((list >> i) & 1) != 0 {
                    self.reg[i] = self.get16(idx);
                    idx += 2;
                }
            }
        }

        #[inline(always)]
        const fn pop_imm_sub_set(&mut self, inst: &Inst) {
            let mut idx  = self.reg[inst.reg_a()];
            let list = inst.imm16();

            let mut i = 0;
            while i < 16 {
                if ((list >> i) & 1) != 0 {
                    self.reg[i] = self.get16(idx);
                    idx -= 2;
                }
            }
        }

        /* === Jump operations === */
        #[inline(always)]
        const fn jmp_reg_add(&mut self, inst: &Inst) {
            let off = self.reg[inst.reg_a()];
            let mem = self.reg[inst.reg_b()];
            self.ip = u16::wrapping_add(mem, off);
        }

        #[inline(always)]
        const fn jmp_reg_sub(&mut self, inst: &Inst) {
            let off = self.reg[inst.reg_a()];
            let mem = self.reg[inst.reg_b()];
            self.ip = u16::wrapping_sub(mem, off);
        }

        #[inline(always)]
        const fn jmp_imm_add(&mut self, inst: &Inst) {
            let off = self.reg[inst.reg_a()];
            let mem = inst.imm16();
            self.ip = u16::wrapping_add(mem, off);
        }

        #[inline(always)]
        const fn jmp_imm_sub(&mut self, inst: &Inst) {
            let off = self.reg[inst.reg_a()];
            let mem = inst.imm16();
            self.ip = u16::wrapping_sub(mem, off);
        }


        #[inline(always)]
        const fn jml_reg_add(&mut self, inst: &Inst) {
            let off = self.reg[inst.reg_a()];
            let mem = self.reg[inst.reg_b()];

            self.reg[inst.reg_a()] = self.ip;
            self.ip = u16::wrapping_add(mem, off);
        }

        #[inline(always)]
        const fn jml_reg_sub(&mut self, inst: &Inst) {
            let off = self.reg[inst.reg_a()];
            let mem = self.reg[inst.reg_b()];
            
            self.reg[inst.reg_a()] = self.ip;
            self.ip = u16::wrapping_sub(mem, off);
        }

        #[inline(always)]
        const fn jml_imm_add(&mut self, inst: &Inst) {
            let off = self.reg[inst.reg_a()];
            let mem = inst.imm16();
            
            self.reg[inst.reg_a()] = self.ip;
            self.ip = u16::wrapping_add(mem, off);
        }

        #[inline(always)]
        const fn jml_imm_sub(&mut self, inst: &Inst) {
            let off = self.reg[inst.reg_a()];
            let mem = inst.imm16();
            
            self.reg[inst.reg_a()] = self.ip;
            self.ip = u16::wrapping_sub(mem, off);
        }


        /* === Instruction Decoding */
        #[inline(always)]
        const fn cond(&self, cond: Cond) -> bool {
            match cond {
                Cond::No                                               => true,
                Cond::Zs if self.zero()                                => true,
                Cond::Zc if !self.zero()                               => true,
                Cond::Ss if self.sign()                                => true,
                Cond::Sc if !self.sign()                               => true,
                Cond::Os if self.sout()                                => true,
                Cond::Oc if !self.sout()                               => true,
                Cond::Cs if self.cout()                                => true,
                Cond::Cc if !self.cout()                               => true,
                Cond::Ts if self.trap()                                => true,
                Cond::Tc if !self.trap()                               => true,
                Cond::Ge if self.sign() == self.sout()                 => true,
                Cond::Lt if self.sign() != self.sout()                 => true,
                Cond::Gt if !self.zero() && self.sign() == self.sout() => true,
                Cond::Le if self.zero()  || self.sign() != self.sout() => true,
                _ => false,
            }
        }

        #[inline(always)]
        fn step(&mut self, inst: &Inst) {
            self.reg[Self::ZR] = 0;

            match inst.code() {
                Code::AddReg    => self.add_reg(inst),
                Code::AddImm    => self.add_imm(inst),
                Code::AddRegSet => self.add_reg_set(inst),
                Code::AddImmSet => self.add_imm_set(inst),

                Code::AdcReg    => self.adc_reg(inst),
                Code::AdcImm    => self.adc_imm(inst),
                Code::AdcRegSet => self.adc_reg_set(inst),
                Code::AdcImmSet => self.adc_imm_set(inst),

                Code::SubReg    => self.sub_reg(inst),
                Code::SubImm    => self.sub_imm(inst),
                Code::SubRegSet => self.sub_reg_set(inst),
                Code::SubImmSet => self.sub_imm_set(inst),

                Code::SbcReg    => self.sbc_reg(inst),
                Code::SbcImm    => self.sbc_imm(inst),
                Code::SbcRegSet => self.sbc_reg_set(inst),
                Code::SbcImmSet => self.sbc_imm_set(inst),

                Code::MulReg    => self.mul_reg(inst),
                Code::MulImm    => self.mul_imm(inst),
                Code::MulRegSet => self.mul_reg_set(inst),
                Code::MulImmSet => self.mul_imm_set(inst),

                Code::MlsReg    => self.mls_reg(inst),
                Code::MlsImm    => self.mls_imm(inst),
                Code::MlsRegSet => self.mls_reg_set(inst),
                Code::MlsImmSet => self.mls_imm_set(inst),

                Code::DivReg    => self.div_reg(inst),
                Code::DivImm    => self.div_imm(inst),
                Code::DivRegSet => self.div_reg_set(inst),
                Code::DivImmSet => self.div_imm_set(inst),

                Code::DvsReg    => self.dvs_reg(inst),
                Code::DvsImm    => self.dvs_imm(inst),
                Code::DvsRegSet => self.dvs_reg_set(inst),
                Code::DvsImmSet => self.dvs_imm_set(inst),

                Code::AndReg    => self.and_reg(inst),
                Code::AndImm    => self.and_imm(inst),
                Code::AndRegSet => self.and_reg_set(inst),
                Code::AndImmSet => self.and_imm_set(inst),

                Code::AnxReg    => self.anx_reg(inst),
                Code::AnxImm    => self.anx_imm(inst),
                Code::AnxRegSet => self.anx_reg_set(inst),
                Code::AnxImmSet => self.anx_imm_set(inst),

                Code::OrrReg    => self.orr_reg(inst),
                Code::OrrImm    => self.orr_imm(inst),
                Code::OrrRegSet => self.orr_reg_set(inst),
                Code::OrrImmSet => self.orr_imm_set(inst),

                Code::OrxReg    => self.orx_reg(inst),
                Code::OrxImm    => self.orx_imm(inst),
                Code::OrxRegSet => self.orx_reg_set(inst),
                Code::OrxImmSet => self.orx_imm_set(inst),

                Code::XorReg    => self.xor_reg(inst),
                Code::XorImm    => self.xor_imm(inst),
                Code::XorRegSet => self.xor_reg_set(inst),
                Code::XorImmSet => self.xor_imm_set(inst),

                Code::XrxReg    => self.xrx_reg(inst),
                Code::XrxImm    => self.xrx_imm(inst),
                Code::XrxRegSet => self.xrx_reg_set(inst),
                Code::XrxImmSet => self.xrx_imm_set(inst),

                Code::BxtReg    => self.bxt_reg(inst),
                Code::BxtImm    => self.bxt_imm(inst),
                Code::BxtRegSet => self.bxt_reg_set(inst),
                Code::BxtImmSet => self.bxt_imm_set(inst),

                Code::LdhRegAddPos => self.ldh_reg_add_pos(inst),
                Code::LdhRegSubPos => self.ldh_reg_sub_pos(inst),
                Code::LdhImmAddPos => self.ldh_imm_add_pos(inst),
                Code::LdhImmSubPos => self.ldh_imm_sub_pos(inst),

                Code::LdbRegAddPos => self.ldb_reg_add_pos(inst),
                Code::LdbRegAddNeg => self.ldb_reg_add_neg(inst),
                Code::LdbRegSubPos => self.ldb_reg_sub_pos(inst),
                Code::LdbRegSubNeg => self.ldb_reg_sub_neg(inst),
                Code::LdbImmAddPos => self.ldb_imm_add_pos(inst),
                Code::LdbImmAddNeg => self.ldb_imm_add_neg(inst),
                Code::LdbImmSubPos => self.ldb_imm_sub_pos(inst),
                Code::LdbImmSubNeg => self.ldb_imm_sub_neg(inst),

                Code::SthRegAdd => self.sth_reg_add(inst),
                Code::SthRegSub => self.sth_reg_sub(inst),
                Code::SthImmAdd => self.sth_imm_add(inst),
                Code::SthImmSub => self.sth_imm_sub(inst),

                Code::StbRegAdd => self.stb_reg_add(inst),
                Code::StbRegSub => self.stb_reg_sub(inst),
                Code::StbImmAdd => self.stb_imm_add(inst),
                Code::StbImmSub => self.stb_imm_sub(inst),

                Code::PshRegAdd    => self.psh_reg_add(inst),
                Code::PshRegSub    => self.psh_reg_sub(inst),
                Code::PshImmAdd    => self.psh_imm_add(inst),
                Code::PshImmSub    => self.psh_imm_sub(inst),
                Code::PshRegAddSet => self.psh_reg_add_set(inst),
                Code::PshRegSubSet => self.psh_reg_sub_set(inst),
                Code::PshImmAddSet => self.psh_imm_add_set(inst),
                Code::PshImmSubSet => self.psh_imm_sub_set(inst),

                Code::PopRegAdd    => self.pop_reg_add(inst),
                Code::PopRegSub    => self.pop_reg_sub(inst),
                Code::PopImmAdd    => self.pop_imm_add(inst),
                Code::PopImmSub    => self.pop_imm_sub(inst),
                Code::PopRegAddSet => self.pop_reg_add_set(inst),
                Code::PopRegSubSet => self.pop_reg_sub_set(inst),
                Code::PopImmAddSet => self.pop_imm_add_set(inst),
                Code::PopImmSubSet => self.pop_imm_sub_set(inst),

                Code::JmpRegAdd => self.jmp_reg_add(inst),
                Code::JmpRegSub => self.jmp_reg_sub(inst),
                Code::JmpImmAdd => self.jmp_imm_add(inst),
                Code::JmpImmSub => self.jmp_imm_sub(inst),

                Code::JmlRegAdd => self.jml_reg_add(inst),
                Code::JmlRegSub => self.jml_reg_sub(inst),
                Code::JmlImmAdd => self.jml_imm_add(inst),
                Code::JmlImmSub => self.jml_imm_sub(inst),
            }
        }
    }
}

fn main() {

}

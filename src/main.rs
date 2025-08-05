#![allow(unused)] 

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

    pub mod build {
        use std::marker::PhantomData as Boo;
        use super::*;

        pub struct Nil;
        pub struct Set;

        pub struct Add; pub struct Sub;
        pub struct And; pub struct Orr;
        pub struct Xor;

        pub struct Bin<Ops, Dst, Lhs, Rhs, Imm> {
            dst: u8,
            lhs: u8,
            rhs: u8,
            imm: u16,
            _pd: Boo<(Ops, Dst, Lhs, Rhs, Imm)>,
        }

        impl<Ops, Lhs, Rhs, Imm> Bin<Ops, Nil, Lhs, Rhs, Imm> {
            #[inline(always)]
            pub const fn dst(self, dst: u8) -> Bin<Ops, Set, Lhs, Rhs, Imm> {
                Bin {
                    dst,
                    lhs: self.lhs,
                    rhs: self.rhs,
                    imm: self.imm,
                    _pd: Boo::<(Ops, Set, Lhs, Rhs, Imm)>,
                }
            }
        }

        impl<Ops, Dst, Rhs, Imm> Bin<Ops, Dst, Nil, Rhs, Imm> {
            #[inline(always)]
            pub const fn lhs(self, lhs: u8) -> Bin<Ops, Dst, Set, Rhs, Imm> {
                Bin {
                    dst: self.dst,
                    lhs,
                    rhs: self.rhs,
                    imm: self.imm,
                    _pd: Boo::<(Ops, Dst, Set, Rhs, Imm)>,
                }
            }
        }

        impl<Ops, Dst, Lhs> Bin<Ops, Dst, Lhs, Nil, Nil> {
            #[inline(always)]
            pub const fn rhs(self, rhs: u8) -> Bin<Ops, Dst, Lhs, Set, Nil> {
                Bin {
                    dst: self.dst,
                    lhs: self.lhs,
                    rhs,
                    imm: self.imm,
                    _pd: Boo::<(Ops, Dst, Lhs, Set, Nil)>,
                }
            }
            
            #[inline(always)]
            pub const fn imm(self, imm: u16) -> Bin<Ops, Dst, Lhs, Nil, Set> {
                Bin {
                    dst: self.dst,
                    lhs: self.lhs,
                    rhs: self.rhs,
                    imm,
                    _pd: Boo::<(Ops, Dst, Lhs, Nil, Set)>,
                }
            }
        }
    
        // Register value
        impl Bin<Add, Set, Set, Set, Nil> {
            #[inline(always)]
            pub fn build(self) -> Result<Inst, String> {
                if self.dst > 0x000F { return Err("dst register is out of bounds".to_string()); }
                if self.lhs > 0x000F { return Err("lhs register is out of bounds".to_string()); }
                if self.rhs > 0x000F { return Err("rhs register is out of bounds".to_string()); }

                Ok(Inst(
                    ((Code::Add as u32) << 24) |
                    ((self.dst  as u32) << 16) |
                    ((self.lhs  as u32) << 12) |
                    ((self.rhs  as u32) <<  8)
                ))
            }
        } 

        impl Bin<Sub, Set, Set, Set, Nil> {
            #[inline(always)]
            pub fn build(self) -> Result<Inst, String> {
                if self.dst > 0x000F { return Err("dst register is out of bounds".to_string()); }
                if self.lhs > 0x000F { return Err("lhs register is out of bounds".to_string()); }
                if self.rhs > 0x000F { return Err("rhs register is out of bounds".to_string()); }

                Ok(Inst(
                    ((Code::Sub as u32) << 24) |
                    ((self.dst  as u32) << 16) |
                    ((self.lhs  as u32) << 12) |
                    ((self.rhs  as u32) <<  8)
                ))
            }
        } 

        impl Bin<And, Set, Set, Set, Nil> {
            #[inline(always)]
            pub fn build(self) -> Result<Inst, String> {
                if self.dst > 0x000F { return Err("dst register is out of bounds".to_string()); }
                if self.lhs > 0x000F { return Err("lhs register is out of bounds".to_string()); }
                if self.rhs > 0x000F { return Err("rhs register is out of bounds".to_string()); }

                Ok(Inst(
                    ((Code::And as u32) << 24) |
                    ((self.dst  as u32) << 16) |
                    ((self.lhs  as u32) << 12) |
                    ((self.rhs  as u32) <<  8)
                ))
            }
        } 

        impl Bin<Orr, Set, Set, Set, Nil> {
            #[inline(always)]
            pub fn build(self) -> Result<Inst, String> {
                if self.dst > 0x000F { return Err("dst register is out of bounds".to_string()); }
                if self.lhs > 0x000F { return Err("lhs register is out of bounds".to_string()); }
                if self.rhs > 0x000F { return Err("rhs register is out of bounds".to_string()); }

                Ok(Inst(
                    ((Code::Orr as u32) << 24) |
                    ((self.dst  as u32) << 16) |
                    ((self.lhs  as u32) << 12) |
                    ((self.rhs  as u32) <<  8)
                ))
            }
        } 

        impl Bin<Xor, Set, Set, Set, Nil> {
            #[inline(always)]
            pub fn build(self) -> Result<Inst, String> {
                if self.dst > 0x000F { return Err("dst register is out of bounds".to_string()); }
                if self.lhs > 0x000F { return Err("lhs register is out of bounds".to_string()); }
                if self.rhs > 0x000F { return Err("rhs register is out of bounds".to_string()); }

                Ok(Inst(
                    ((Code::Xor as u32) << 24) |
                    ((self.dst  as u32) << 16) |
                    ((self.lhs  as u32) << 12) |
                    ((self.rhs  as u32) <<  8)
                ))
            }
        } 

        // Immediate value
        impl Bin<Add, Set, Set, Nil, Set> {
            #[inline(always)]
            pub fn build(self) -> Result<Inst, String> {
                if self.dst > 0x000F { return Err("dst register is out of bounds".to_string()); }
                if self.lhs > 0x000F { return Err("lhs register is out of bounds".to_string()); }
                if self.imm > 0x0FFF { return Err("imm value is out of bounds".to_string());    }

                Ok(Inst(
                                     (1 << 23) |
                    ((Code::Add as u32) << 24) |
                    ((self.dst  as u32) << 16) |
                    ((self.lhs  as u32) << 12) | 
                    ((self.imm  as u32))
                ))
            }
        } 

        impl Bin<Sub, Set, Set, Nil, Set> {
            #[inline(always)]
            pub fn build(self) -> Result<Inst, String> {
                if self.dst > 0x000F { return Err("dst register is out of bounds".to_string()); }
                if self.lhs > 0x000F { return Err("lhs register is out of bounds".to_string()); }
                if self.imm > 0x0FFF { return Err("imm value is out of bounds".to_string());    }

                Ok(Inst(
                                     (1 << 23) |
                    ((Code::Sub as u32) << 24) |
                    ((self.dst  as u32) << 16) |
                    ((self.lhs  as u32) << 12) |
                    ((self.imm  as u32))
                ))
            }
        } 

        impl Bin<And, Set, Set, Nil, Set> {
            #[inline(always)]
            pub fn build(self) -> Result<Inst, String> {
                if self.dst > 0x000F { return Err("dst register is out of bounds".to_string()); }
                if self.lhs > 0x000F { return Err("lhs register is out of bounds".to_string()); }
                if self.imm > 0x0FFF { return Err("imm value is out of bounds".to_string());    }

                Ok(Inst(
                                     (1 << 23) |
                    ((Code::And as u32) << 24) |
                    ((self.dst  as u32) << 16) |
                    ((self.lhs  as u32) << 12) |
                    ((self.imm  as u32))
                ))
            }
        } 

        impl Bin<Orr, Set, Set, Nil, Set> {
            #[inline(always)]
            pub fn build(self) -> Result<Inst, String> {
                if self.dst > 0x000F { return Err("dst register is out of bounds".to_string()); }
                if self.lhs > 0x000F { return Err("lhs register is out of bounds".to_string()); }
                if self.imm > 0x0FFF { return Err("imm value is out of bounds".to_string());    }

                Ok(Inst(
                                     (1 << 23) |
                    ((Code::Orr as u32) << 24) |
                    ((self.dst  as u32) << 16) |
                    ((self.lhs  as u32) << 12) |
                    ((self.imm  as u32))
                ))
            }
        } 

        impl Bin<Xor, Set, Set, Nil, Set> {
            #[inline(always)]
            pub fn build(self) -> Result<Inst, String> {
                if self.dst > 0x000F { return Err("dst register is out of bounds".to_string()); }
                if self.lhs > 0x000F { return Err("lhs register is out of bounds".to_string()); }
                if self.imm > 0x0FFF { return Err("imm value is out of bounds".to_string());    }

                Ok(Inst(
                                     (1 << 23) |
                    ((Code::Xor as u32) << 24) |
                    ((self.dst  as u32) << 16) |
                    ((self.lhs  as u32) << 12) |
                    ((self.imm  as u32))
                ))
            }
        } 

        #[inline(always)]
        pub const fn add() -> Bin<Add, Nil, Nil, Nil, Nil> {
            Bin { dst: 0, lhs: 0, rhs: 0, imm: 0, _pd: Boo }
        }

        #[inline(always)]
        pub const fn sub() -> Bin<Sub, Nil, Nil, Nil, Nil> {
            Bin { dst: 0, lhs: 0, rhs: 0, imm: 0, _pd: Boo }
        }

        #[inline(always)]
        pub const fn and() -> Bin<And, Nil, Nil, Nil, Nil> {
            Bin { dst: 0, lhs: 0, rhs: 0, imm: 0, _pd: Boo }
        }

        #[inline(always)]
        pub const fn orr() -> Bin<Orr, Nil, Nil, Nil, Nil> {
            Bin { dst: 0, lhs: 0, rhs: 0, imm: 0, _pd: Boo }
        }

        #[inline(always)]
        pub const fn xor() -> Bin<Xor, Nil, Nil, Nil, Nil> {
            Bin { dst: 0, lhs: 0, rhs: 0, imm: 0, _pd: Boo }
        }
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
        pub const fn into_u8(self) -> u8 {
            self as u8
        }

        #[inline(always)]
        pub const fn from_u8(byte: u8) -> Cond {
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
        // TODO: 
        // Jmp table instrution
        // Stack manipulation instructions
        // Multi data instructions
        // Add/Sub with carry operations
        // And/Orr Not operations
        // Mul/Div High value 
        // Shift instruction

        Sys, // system call
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
        pub const fn into_u8(self) -> u8 {
            self as u8
        }

        #[inline(always)]
        pub const fn from_u8(byte: u8) -> Code {
            unsafe { std::mem::transmute(byte) }
        }
    }

    impl Display for Code {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Code::Sys => f.write_str("sys"),
                Code::Ret => f.write_str("ret"),
                Code::Jmp => f.write_str("jmp"),
                Code::Mov => f.write_str("mov"),
                Code::Ldr => f.write_str("ldr"),
                Code::Str => f.write_str("str"),
                Code::Add => f.write_str("add"),
                Code::Sub => f.write_str("sub"),
                Code::Mul => f.write_str("mul"),
                Code::Div => f.write_str("div"),
                Code::And => f.write_str("and"),
                Code::Orr => f.write_str("orr"),
                Code::Xor => f.write_str("xor"),
            }
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
        pub const fn into_u8(self) -> u8 {
            self as u8
        }

        #[inline(always)]
        pub const fn from_u8(byte: u8) -> Kind {
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
    #[derive(Clone, Copy)]
    pub struct Inst(u32);

    impl Inst {
        #[inline(always)]
        pub const fn cond(&self) -> Cond {
            Cond::from_u8(((self.0 >> 28) & 0xF) as u8) 
        }

        #[inline(always)]
        pub const fn code(&self) -> Code {
            Code::from_u8(((self.0 >> 24) & 0xF) as u8) 
        }

        #[inline(always)]
        pub const fn into_bits(self) -> u32 {
            self.0
        }

        #[inline(always)]
        pub const fn into_data(self) -> Data {
            Data(self.0)
        }

        #[inline(always)]
        pub const fn into_math(self) -> Math {
            Math(self.0)
        }

        #[inline(always)]
        pub const fn into_jump(self) -> Jump {
            Jump(self.0)
        }
    
        #[inline(always)]
        pub const fn into_call(self) -> Call {
            Call(self.0)
        }

        #[inline(always)]
        pub const fn from_u32(bytes: u32) -> Inst {
            Inst(bytes)
        }
    }

    impl Display for Inst {
        // TODO: Make this accurate
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}{}", self.code(), self.cond());

            match self.code() {
                Code::Sys => write!(f, " {}", Call(self.0).kind()),
                Code::Ret => Ok(()),
                
                Code::Jmp => {
                    let jump = Jump(self.0);

                    if jump.imm() {
                        write!(f, " .{:04X}", jump.abs())
                    } else {
                        write!(f, " R{:X}", jump.reg()) 
                    }
                }

                Code::Mov => {
                    let math = Math(self.0);

                    if math.imm() {
                        write!(f, " R{:X}, #{}", math.dst(), math.val())
                    } else {
                        write!(f, " R{:X}, R{:X}", math.dst(), math.rhs())
                    }
                }

                Code::Ldr | Code::Str => {
                    let data = Data(self.0);

                    let neg  = if data.neg()  { '-' } else { '+' };
                    let sign = if data.sign() { '-' } else { ' ' };
                    let byte = if data.byte() { 'b' } else { 'w' };

                    if data.imm() {
                        write!(f, " R{:X}, {sign}[R{:X} {neg} #{}]", data.dst(), data.mem(), data.val())
                    } else {
                        write!(f, " R{:X}, {sign}[R{:X} {neg} R{:X}]", data.dst(), data.mem(), data.off())
                    }
                }

                Code::Add | Code::Sub |
                Code::Mul | Code::Div |
                Code::And | Code::Orr | Code::Xor => {
                    let math = Math(self.0);

                    if math.imm() {
                        write!(f, " R{:X}, R{:X}, #{}", math.dst(), math.lhs(), math.val())
                    } else {
                        write!(f, " R{:X}, R{:X}, R{:X}", math.dst(), math.lhs(), math.rhs())
                    }
                }
            }
        }
    }

    #[repr(transparent)]
    pub struct Data(u32);

    impl Data {
        #[inline(always)]
        pub const fn imm(&self) -> bool {
            ((self.0 >> 23) & 1) != 0 
        }

        #[inline(always)]
        pub const fn neg(&self) -> bool {
            ((self.0 >> 22) & 1) != 0 
        }

        #[inline(always)]
        pub const fn byte(&self) -> bool {
            ((self.0 >> 21) & 1) != 0 
        }

        #[inline(always)]
        pub const fn sign(&self) -> bool {
            ((self.0 >> 20) & 1) != 0
        }

        #[inline(always)]
        pub const fn dst(&self) -> usize {
            ((self.0 >> 16) & 0xF) as usize
        }
            
        #[inline(always)]
        pub const fn mem(&self) -> usize {
            ((self.0 >> 12) & 0xF) as usize
        }

        // imm is false
        #[inline(always)]
        pub const fn off(&self) -> usize {
            ((self.0 >> 8) & 0xF) as usize
        }

        #[inline(always)]
        pub const fn mul(&self) -> usize {
            ((self.0 >> 4) & 0xF) as usize
        }

        // imm is true
        #[inline(always)]
        pub const fn val(&self) -> u16 {
            (self.0 & 0x0FFF) as u16
        }
    }

    #[repr(transparent)]
    pub struct Math(u32);

    impl Math {
        #[inline(always)]
        pub const fn imm(&self) -> bool {
            ((self.0 >> 23) & 1) != 0 
        }

        #[inline(always)]
        pub const fn sign(&self) -> bool {
            ((self.0 >> 22) & 1) != 0
        }

        #[inline(always)]
        pub const fn dst(&self) -> usize {
            ((self.0 >> 16) & 0xF) as usize
        }
            
        #[inline(always)]
        pub const fn lhs(&self) -> usize {
            ((self.0 >> 12) & 0xF) as usize
        }
            
        #[inline(always)] // imm is false
        pub const fn rhs(&self) -> usize {
            ((self.0 >> 8) & 0xF) as usize
        }

        #[inline(always)] // imm is true
        pub const fn val(&self) -> u16 {
            (self.0 & 0x0FFF) as u16
        }
    }

    #[repr(transparent)]
    pub struct Jump(u32);

    impl Jump {
        #[inline(always)]
        pub const fn imm(&self) -> bool {
            ((self.0 >> 23) & 1) != 0
        }

        #[inline(always)]
        pub const fn save(&self) -> bool {
            ((self.0 >> 22) & 1) != 0
        }

        // imm is false
        #[inline(always)]
        pub const fn reg(&self) -> usize {
            ((self.0 >> 16) & 0xF) as usize
        }

        // imm is true
        #[inline(always)]
        pub const fn abs(&self) -> u16 {
            (self.0 & 0xFFFF) as u16
        }
    }

    #[repr(transparent)]
    pub struct Call(u32);

    impl Call {
        #[inline(always)]
        pub const fn kind(&self) -> Kind {
            Kind::from_u8(((self.0 >> 20) & 0xF) as u8)
        }

        #[inline(always)]
        pub const fn dst(&self) -> usize {
            ((self.0 >> 16) & 0xF) as usize
        }

        pub const fn lhs(&self) -> usize {
            ((self.0 >> 12) & 0xF) as usize
        }

        pub const fn rhs(&self) -> usize {
            ((self.0 >> 8) & 0x4) as usize
        }



    }
}

/// === Rom ===
/// - Currently only stores instructions
/// + and in their full format instead of a byte sequence
/// 
/// - hopefully later it'll be used for data segments,
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
    use crate::inst::*;

    pub struct Rom {
        code: Vec<Inst>,
    }

    impl Rom {
        pub fn load(code: &[u32]) -> Rom {
            Rom {
                code: code.iter().cloned().map(Inst::from_u32).collect()
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
                let inst = self.rom.code[self.ip as usize];
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

        /* === Jump === */
        #[inline(always)]
        const fn jmp_reg(&mut self, jump: Jump) {
            self.ip = self.reg[jump.reg()];
        }

        #[inline(always)]
        const fn jmp_imm(&mut self, jump: Jump) {
            self.ip = jump.abs();
        }

        #[inline(always)]
        const fn jmp_reg_save(&mut self, jump: Jump) {
            self.set16(self.reg[Self::SP], self.ip);
            self.reg[Self::SP] += 2;

            self.ip = self.reg[jump.reg()];
        }

        #[inline(always)]
        const fn jmp_imm_save(&mut self, jump: Jump) {
            self.set16(self.reg[Self::SP], self.ip);
            self.reg[Self::SP] += 2;

            self.ip = jump.abs();

        }

        /* === Single data transfer === */
        #[inline(always)]
        const fn ldr_reg_pos_u16(&mut self, data: Data) {
            let off = u16::wrapping_mul(self.reg[data.off()], self.reg[data.mul()]);
            let idx = u16::wrapping_add(self.reg[data.mem()], off);
            self.reg[data.dst()] = self.get16(idx);
        }

        #[inline(always)]
        const fn ldr_reg_neg_u16(&mut self, data: Data) {
            let off = u16::wrapping_mul(self.reg[data.off()], self.reg[data.mul()]);
            let idx = u16::wrapping_sub(self.reg[data.mem()], off);
            self.reg[data.dst()] = self.get16(idx);
        }

        #[inline(always)]
        const fn ldr_reg_pos_u8(&mut self, data: Data) {
            let off = u16::wrapping_mul(self.reg[data.off()], self.reg[data.mul()]);
            let idx = u16::wrapping_add(self.reg[data.mem()], off);
            self.reg[data.dst()] = self.get8(idx) as u16;
        }

        #[inline(always)]
        const fn ldr_reg_pos_i8(&mut self, data: Data) {
            let off = u16::wrapping_mul(self.reg[data.off()], self.reg[data.mul()]);
            let idx = u16::wrapping_add(self.reg[data.mem()], off);
            self.reg[data.dst()] = self.get8(idx).cast_signed() as u16;
        }

        #[inline(always)]
        const fn ldr_reg_neg_u8(&mut self, data: Data) {
            let off = u16::wrapping_mul(self.reg[data.off()], self.reg[data.mul()]);
            let idx = u16::wrapping_sub(self.reg[data.mem()], off);
            self.reg[data.dst()] = self.get8(idx) as u16;
        }

        #[inline(always)]
        const fn ldr_reg_neg_i8(&mut self, data: Data) {
            let off = u16::wrapping_mul(self.reg[data.off()], self.reg[data.mul()]);
            let idx = u16::wrapping_sub(self.reg[data.mem()], off);
            self.reg[data.dst()] = self.get8(idx).cast_signed() as u16;
        }

        #[inline(always)]
        const fn ldr_imm_pos_u16(&mut self, data: Data) {
            let idx = u16::wrapping_add(self.reg[data.mem()], data.val());
            self.reg[data.dst()] = self.get16(idx);
        }

        #[inline(always)]
        const fn ldr_imm_neg_u16(&mut self, data: Data) {
            let idx = u16::wrapping_sub(self.reg[data.mem()], data.val());
            self.reg[data.dst()] = self.get16(idx);
        }

        #[inline(always)]
        const fn ldr_imm_pos_u8(&mut self, data: Data) {
            let idx = u16::wrapping_add(self.reg[data.mem()], data.val());
            self.reg[data.dst()] = self.get8(idx) as u16;
        }

        #[inline(always)]
        const fn ldr_imm_pos_i8(&mut self, data: Data) {
            let idx = u16::wrapping_add(self.reg[data.mem()], data.val());
            self.reg[data.dst()] = self.get8(idx).cast_signed() as u16;
        }

        #[inline(always)]
        const fn ldr_imm_neg_u8(&mut self, data: Data) {
            let idx = u16::wrapping_sub(self.reg[data.mem()], data.val());
            self.reg[data.dst()] = self.get8(idx) as u16;
        }

        #[inline(always)]
        const fn ldr_imm_neg_i8(&mut self, data: Data) {
            let idx = u16::wrapping_sub(self.reg[data.mem()], data.val());
            self.reg[data.dst()] = self.get8(idx).cast_signed() as u16;
        }

        #[inline(always)]
        const fn str_reg_pos_u16(&mut self, data: Data) {
            let off = u16::wrapping_mul(self.reg[data.off()], self.reg[data.mul()]);
            let idx = u16::wrapping_add(self.reg[data.mem()], off);
            self.set16(idx, self.reg[data.dst()]);
        }   

        #[inline(always)]
        const fn str_reg_neg_u16(&mut self, data: Data) {
            let off = u16::wrapping_mul(self.reg[data.off()], self.reg[data.mul()]);
            let idx = u16::wrapping_sub(self.reg[data.mem()], off);
            self.set16(idx, self.reg[data.dst()]);
        }

        #[inline(always)]
        const fn str_reg_pos_u8(&mut self, data: Data) {
            let off = u16::wrapping_mul(self.reg[data.off()], self.reg[data.mul()]);
            let idx = u16::wrapping_add(self.reg[data.mem()], off);
            self.set8(idx, self.reg[data.dst()] as u8);
        }

        #[inline(always)]
        const fn str_reg_neg_u8(&mut self, data: Data) {
            let off = u16::wrapping_mul(self.reg[data.off()], self.reg[data.mul()]);
            let idx = u16::wrapping_sub(self.reg[data.mem()], off);
            self.set8(idx, self.reg[data.dst()] as u8);
        }

        #[inline(always)]
        const fn str_imm_pos_u16(&mut self, data: Data) {
            let idx = u16::wrapping_add(self.reg[data.mem()], data.val());
            self.set16(idx, self.reg[data.dst()]);
        }

        #[inline(always)]
        const fn str_imm_neg_u16(&mut self, data: Data) {
            let idx = u16::wrapping_sub(self.reg[data.mem()], data.val());
            self.set16(idx, self.reg[data.dst()]);
        }

        #[inline(always)]
        const fn str_imm_pos_u8(&mut self, data: Data) {
            let idx = u16::wrapping_add(self.reg[data.mem()], data.val());
            self.set8(idx, self.reg[data.dst()] as u8);
        }

        #[inline(always)]
        const fn str_imm_neg_u8(&mut self, data: Data) {
            let idx = u16::wrapping_sub(self.reg[data.mem()], data.val());
            self.set8(idx, self.reg[data.dst()] as u8);
        }
        
        /* === Math operations === */
        #[inline(always)]
        const fn add_reg(&mut self, math: Math) {
            let lhs = self.reg[math.lhs()];
            let rhs = self.reg[math.rhs()];

            let (val, cout) = u16::overflowing_add(lhs, rhs);
            let sout = ((lhs ^ val) & (rhs ^ val)) & 0x8000 != 0;

            self.set_flags(val, sout, cout, false);
            self.reg[math.dst()] = val;
        }

        #[inline(always)]
        const fn add_imm(&mut self, math: Math) {
            let lhs = self.reg[math.lhs()];
            let rhs = math.val();

            let (val, cout) = u16::overflowing_add(lhs, rhs);
            let sout = ((lhs ^ val) & (rhs ^ val)) & 0x8000 != 0;

            self.set_flags(val, sout, cout, false);
            self.reg[math.dst()] = val;
        }
        
        #[inline(always)]
        const fn sub_reg(&mut self, math: Math) {
            let lhs = self.reg[math.lhs()];
            let rhs = self.reg[math.rhs()];

            let (val, cout) = u16::overflowing_sub(lhs, rhs);
            let sout = ((lhs ^ val) & (rhs ^ val)) & 0x8000 != 0;

            self.set_flags(val, sout, cout, false);
            self.reg[math.dst()] = val;
        }

        #[inline(always)]
        const fn sub_imm(&mut self, math: Math) {
            let lhs = self.reg[math.lhs()];
            let rhs = math.val();

            let (val, cout) = u16::overflowing_sub(lhs, rhs);
            let sout = ((lhs ^ val) & (rhs ^ val)) & 0x8000 != 0;

            self.set_flags(val, sout, cout, false);
            self.reg[math.dst()] = val;
        }
        
        #[inline(always)]
        const fn and_reg(&mut self, math: Math) {
            let lhs = self.reg[math.lhs()];
            let rhs = self.reg[math.rhs()];
            let val = lhs & rhs;

            self.set_flags(val, false, false, false);
            self.reg[math.dst()] = val;
        }

        #[inline(always)]
        const fn and_imm(&mut self, math: Math) {
            let lhs = self.reg[math.lhs()];
            let rhs = math.val();
            let val = lhs & rhs;

            self.set_flags(val, false, false, false);
            self.reg[math.dst()] = val;
        }
        
        #[inline(always)]
        const fn orr_reg(&mut self, math: Math) {
            let lhs = self.reg[math.lhs()];
            let rhs = self.reg[math.rhs()];
            let val = lhs | rhs;

            self.set_flags(val, false, false, false);
            self.reg[math.dst()] = val;
        }

        #[inline(always)]
        const fn orr_imm(&mut self, math: Math) {
            let lhs = self.reg[math.lhs()];
            let rhs = math.val();
            let val = lhs | rhs;

            self.set_flags(val, false, false, false);
            self.reg[math.dst()] = val;
        }
        
        #[inline(always)]
        const fn xor_reg(&mut self, math: Math) {
            let lhs = self.reg[math.lhs()];
            let rhs = self.reg[math.rhs()];
            let val = lhs ^ rhs;

            self.set_flags(val, false, false, false);
            self.reg[math.dst()] = val;
        }

        #[inline(always)]
        const fn xor_imm(&mut self, math: Math) {
            let lhs = self.reg[math.lhs()];
            let rhs = math.val();
            let val = lhs ^ rhs;

            self.set_flags(val, false, false, false);
            self.reg[math.dst()] = val;
        }
        
        #[inline(always)]
        const fn mul_reg_u16(&mut self, math: Math) {
            let lhs = self.reg[math.lhs()];
            let rhs = self.reg[math.rhs()];

            let (val, cout) = u16::overflowing_mul(lhs, rhs);

            self.set_flags(val, cout, cout, false);
            self.reg[math.dst()] = val;
        }

        #[inline(always)]
        const fn mul_reg_i16(&mut self, math: Math) {
            let lhs = self.reg[math.lhs()] as i16;
            let rhs = self.reg[math.rhs()] as i16;

            let (val, sout) = i16::overflowing_mul(lhs, rhs);

            self.set_flags(val as u16, sout, false, false);
            self.reg[math.dst()] = val as u16;
        }
    
        #[inline(always)]
        const fn mul_imm_u16(&mut self, math: Math) {
            let lhs = self.reg[math.lhs()];
            let rhs = math.val();

            let (val, cout) = u16::overflowing_mul(lhs, rhs);

            self.set_flags(val, cout, cout, false);
            self.reg[math.dst()] = val;
        }

        #[inline(always)]
        const fn mul_imm_i16(&mut self, math: Math) {
            let lhs = self.reg[math.lhs()] as i16;
            let rhs = math.val() as i16;

            let (val, sout) = i16::overflowing_mul(lhs, rhs);

            self.set_flags(val as u16, sout, false, false);
            self.reg[math.dst()] = val as u16;
        }
    
        #[inline(always)]
        const fn div_reg_u16(&mut self, math: Math) {
            let lhs = self.reg[math.lhs()];
            let rhs = self.reg[math.rhs()];

            match rhs {
                0 => {
                    self.set_flags(0, false, false, true);
                    self.reg[math.dst()] = 0;
                }

                rhs => {
                    let val = lhs / rhs;

                    self.set_flags(val, false, false, false);
                    self.reg[math.dst()] = val;
                }
            }
        }

        #[inline(always)]
        const fn div_reg_i16(&mut self, math: Math) {
            let lhs = self.reg[math.lhs()] as i16;
            let rhs = self.reg[math.rhs()] as i16;

            if rhs == 0 || (lhs == i16::MIN && rhs == -1) {
                self.set_flags(0, false, false, true);
                self.reg[math.dst()] = 0;
            } else {
                let val = (lhs / rhs) as u16;

                self.set_flags(val, false, false, false);
                self.reg[math.dst()] = val;
            }
        }
    
        #[inline(always)]
        const fn div_imm_u16(&mut self, math: Math) {
            let lhs = self.reg[math.lhs()];
            let rhs = math.val();

            match rhs {
                0 => {
                    self.set_flags(0, false, false, true);
                    self.reg[math.dst()] = 0;
                }

                rhs => {
                    let val = lhs / rhs;

                    self.set_flags(val, false, false, false);
                    self.reg[math.dst()] = val;
                }
            }
        }

        #[inline(always)]
        const fn div_imm_i16(&mut self, math: Math) {
            let lhs = self.reg[math.lhs()] as i16;
            let rhs = math.val() as i16;

            if rhs == 0 || (lhs == i16::MIN && rhs == -1) {
                self.set_flags(0, false, false, true);
                self.reg[math.dst()] = 0;
            } else {
                let val = (lhs / rhs) as u16;

                self.set_flags(val, false, false, false);
                self.reg[math.dst()] = val;
            }
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
        fn step(&mut self, inst: Inst) {
            self.reg[Self::ZR] = 0;

            match inst.code() {
                Code::Sys => {
                    let call = inst.into_call();

                    match call.kind() {
                        Kind::Halt => (),
                        Kind::PutC => todo!(),
                        Kind::GetC => todo!(),
                        Kind::PutS => todo!(),
                        Kind::GetS => todo!(),
                    }
                } 

                Code::Ret => {
                    self.reg[Self::SP] -= 2;
                    self.ip = self.get16(self.reg[Self::SP]);
                }
                
                Code::Jmp => {
                    let jump = inst.into_jump();

                    match (jump.imm(), jump.save()) {
                        (false, false) => self.jmp_reg(jump),
                        (true , false) => self.jmp_imm(jump),
                        (false, true ) => self.jmp_reg_save(jump),
                        (true , true ) => self.jmp_imm_save(jump),
                    }
                }

                Code::Mov => {
                    let math = inst.into_math();

                    self.reg[math.dst()] = if math.imm() { math.val() } else { self.reg[math.rhs()] };
                }
                
                Code::Ldr => {
                    let data = inst.into_data();
                    
                    match (data.imm(), data.neg(), data.byte(), data.sign()) {
                        (false, false, false, _    ) => self.ldr_reg_pos_u16(data),
                        (false, true , false, _    ) => self.ldr_reg_neg_u16(data),
                        (false, false, true , false) => self.ldr_reg_pos_u8(data),
                        (false, false, true , true ) => self.ldr_reg_pos_i8(data),
                        (false, true , true , false) => self.ldr_reg_neg_u8(data),
                        (false, true , true , true ) => self.ldr_reg_neg_i8(data),
                        (true , false, false, _    ) => self.ldr_imm_pos_u16(data),
                        (true , true , false, _    ) => self.ldr_imm_neg_u16(data),
                        (true , false, true , false) => self.ldr_imm_pos_u8(data),
                        (true , false, true , true ) => self.ldr_imm_pos_i8(data),
                        (true , true , true , false) => self.ldr_imm_neg_u8(data),
                        (true , true , true , true ) => self.ldr_imm_neg_i8(data),
                    }
                }

                Code::Str => {
                    let data = inst.into_data();

                    match (data.imm(), data.neg(), data.byte()) {
                        (false, false, false) => self.str_reg_pos_u16(data),
                        (false, true , false) => self.str_reg_neg_u16(data),
                        (false, false, true ) => self.str_reg_pos_u8(data),
                        (false, true , true ) => self.str_reg_neg_u8(data),
                        (true , false, false) => self.str_imm_pos_u16(data),
                        (true , true , false) => self.str_imm_neg_u16(data),
                        (true , false, true ) => self.str_imm_pos_u8(data),
                        (true , true , true ) => self.str_imm_neg_u8(data),
                    }
                }

                Code::Add => {
                    let math = inst.into_math();
                    if math.imm() { self.add_imm(math); } else { self.add_reg(math); }
                }

                Code::Sub => {
                    let math = inst.into_math();
                    if math.imm() { self.sub_imm(math); } else { self.sub_reg(math); }
                }

                Code::And => {
                    let math = inst.into_math();
                    if math.imm() { self.and_imm(math); } else { self.and_reg(math); }
                }

                Code::Orr => {
                    let math = inst.into_math();
                    if math.imm() { self.orr_imm(math); } else { self.orr_reg(math); }
                }

                Code::Xor => {
                    let math = inst.into_math();
                    if math.imm() { self.xor_imm(math); } else { self.xor_reg(math); }
                }

                Code::Mul => {
                    let math = inst.into_math();

                    match (math.imm(), math.sign()) {
                        (false, false) => self.mul_reg_u16(math),
                        (false, true)  => self.mul_reg_i16(math),
                        (true, false)  => self.mul_imm_u16(math),
                        (true, true)   => self.mul_imm_i16(math),
                    }
                }

                Code::Div => {
                    let math = inst.into_math();

                    match (math.imm(), math.sign()) {
                        (false, false) => self.div_reg_u16(math),
                        (false, true)  => self.div_reg_i16(math),
                        (true, false)  => self.div_imm_u16(math),
                        (true, true)   => self.div_imm_i16(math),
                    }
                }
            }
        }
    }
}

fn main() {
    use console::{Rom, Console};
    use inst::*;

    let code = [
        inst::orr().dst(1).lhs(0).imm(1).build().unwrap().into_bits(),
        inst::orr().dst(2).lhs(0).imm(2).build().unwrap().into_bits(),
        inst::orr().dst(3).lhs(0).imm(3).build().unwrap().into_bits(),
        inst::orr().dst(4).lhs(0).imm(4).build().unwrap().into_bits(),
        inst::add().dst(5).lhs(1).rhs(2).build().unwrap().into_bits(),
        inst::add().dst(6).lhs(3).rhs(4).build().unwrap().into_bits(),
        inst::add().dst(7).lhs(5).rhs(6).build().unwrap().into_bits(),
    ];

    let rom = Rom::load(&code);
    rom.view();
    Console::new(&rom).exec().dump();
}

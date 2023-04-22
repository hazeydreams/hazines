use core::panic;

use lazy_static::lazy_static;


#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Accumulator,
    Immediate,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect,
    Indirect_X,
    Indirect_Y,
    NoAddressing
}

#[derive(Debug)]
pub struct Flags {
    pub carry             : bool,
    pub zero              : bool,
    pub interrupt_disable : bool,
    pub decimal_mode      : bool,
    pub overflow          : bool,
    pub negative          : bool,
}

impl Flags {
    fn new() -> Self {
        Self { carry: false, zero: false, interrupt_disable: false, decimal_mode: false, overflow: false, negative: false }
    }

    fn update_zero_negative_flags(&mut self, data: u8)
    {
        self.zero = (data == 0);
        self.negative = (data & 0x80) != 0;
    }

    fn update_carry_flag(&mut self, data: u16)
    {
        self.carry = (data & 0x100) != 0;
    }
}

struct OpCode {
    opcode          : u8,
    tag             : &'static str,
    width           : u8,
    cycles          : u8,
    addressing_mode : AddressingMode 
}

impl OpCode {
    fn new(opcode: u8, tag: &'static str, width: u8, cycles: u8, addressing_mode: AddressingMode) -> Self {
        Self { opcode, tag, width, cycles, addressing_mode }
    }
}

lazy_static! {
    static ref CPU_INSTRUCTION_SET: Vec<OpCode> = vec![
        OpCode::new(0x00, "BRK", 1, 7, AddressingMode::NoAddressing),
        OpCode::new(0xEA, "NOP", 1, 2, AddressingMode::NoAddressing),

        // ADC
        OpCode::new(0x69, "ADC", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x65, "ADC", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x75, "ADC", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x6D, "ADC", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x7D, "ADC", 3, 4, AddressingMode::Absolute_X),
        OpCode::new(0x79, "ADC", 3, 4, AddressingMode::Absolute_Y),
        OpCode::new(0x61, "ADC", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0x71, "ADC", 2, 5, AddressingMode::Indirect_Y),
        
        // LDA
        OpCode::new(0xa9, "LDA", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xa5, "LDA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xb5, "LDA", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0xad, "LDA", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xbd, "LDA", 3, 4, AddressingMode::Absolute_X),
        OpCode::new(0xb9, "LDA", 3, 4, AddressingMode::Absolute_Y),
        OpCode::new(0xa1, "LDA", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0xb1, "LDA", 2, 5, AddressingMode::Indirect_Y),

        // AND
        OpCode::new(0x29, "AND", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x25, "AND", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x35, "AND", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x2d, "AND", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x2d, "AND", 3, 4, AddressingMode::Absolute_X),
        OpCode::new(0x39, "AND", 3, 4, AddressingMode::Absolute_Y),
        OpCode::new(0x11, "AND", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0x11, "AND", 2, 5, AddressingMode::Indirect_Y),

        // ASL
        OpCode::new(0x0A, "ASL", 1, 2, AddressingMode::Accumulator),
        OpCode::new(0x06, "ASL", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x16, "ASL", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0x0E, "ASL", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x1E, "ASL", 3, 7, AddressingMode::Absolute_X),

        // Set FLAG Opcodes
        OpCode::new(0x38, "SEC", 1, 2, AddressingMode::NoAddressing),
        OpCode::new(0xF8, "SED", 1, 2, AddressingMode::NoAddressing),
        OpCode::new(0x78, "SEI", 1, 2, AddressingMode::NoAddressing),


    ];
}

#[derive(Debug)]
pub struct Cpu {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub stack_pointer: u8,
    pub program_counter: u16,
    pub flags: Flags,
    memory: [u8; 0xFFFF]
}

impl Cpu {
    pub fn new() -> Self {
        Cpu {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            stack_pointer: 0,
            program_counter: 0,
            flags: Flags::new(),
            memory: [0; 0xFFFF]
        }
    }

    pub fn load_program(&mut self, program: Vec<u8>) {
        for i in 0..program.len() {
            self.mem_write(i as u16, program[i]);
        }
    }

    /// Run the program currently loaded into memory
    pub fn run(&mut self)
    {
        loop {
            let instruction = self.mem_read(self.program_counter);
            self.program_counter += 1;

            if let Some(opcode) = CPU_INSTRUCTION_SET.iter().find(|opcode| opcode.opcode == instruction) {
                match opcode.tag {
                    "BRK" => return,
                    "ADC" => self.adc(&opcode.addressing_mode),
                    "LDA" => self.lda(&opcode.addressing_mode),
                    "AND" => self.and(&opcode.addressing_mode),
                    "ASL" => self.asl(&opcode.addressing_mode),
                    "SEC" => self.sec(&opcode.addressing_mode),
                    "SED" => self.sed(&opcode.addressing_mode),
                    "SEI" => self.sei(&opcode.addressing_mode),
                    _ => panic!("No routine to handle instruction {}", opcode.tag),
                }

                self.program_counter += opcode.width.saturating_sub(1) as u16;
            } else {
                panic!("Unknown instruction {}!", instruction);
            }
        }
    }

    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    fn mem_read_u16(&self, addr: u16) -> u16 {
        let lo = self.mem_read(addr) as u16;
        let hi = self.mem_read(addr + 1) as u16;
        (hi << 8) | (lo as u16)
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.mem_write(addr, lo);
        self.mem_write(addr + 1, hi);
    }

    fn find_operand_address(&self, address_mode: &AddressingMode) -> Option<u16>
    {
        match address_mode {
            AddressingMode::Immediate => Some(self.program_counter),
            AddressingMode::ZeroPage => Some(self.mem_read(self.program_counter) as u16),
            AddressingMode::Absolute => Some(self.mem_read_u16(self.program_counter)),
            AddressingMode::ZeroPage_X => Some((self.mem_read(self.program_counter) as u16).wrapping_add(self.register_x as u16)),
            AddressingMode::ZeroPage_Y => Some((self.mem_read(self.program_counter) as u16).wrapping_add(self.register_y as u16)),
            AddressingMode::Absolute_X => Some((self.mem_read(self.program_counter) as u16).wrapping_add(self.register_x as u16)),
            AddressingMode::Absolute_Y => Some((self.mem_read(self.program_counter) as u16).wrapping_add(self.register_y as u16)),
            AddressingMode::Indirect => {
                let p_addr = self.mem_read_u16(self.program_counter);
                Some(self.mem_read_u16(p_addr))
            }
            AddressingMode::Indirect_X => {
                let p_addr = self.mem_read_u16(self.program_counter);
                Some(self.mem_read_u16(p_addr).wrapping_add(self.register_x as u16))
            }
            AddressingMode::Indirect_Y => {
                let p_addr = self.mem_read_u16(self.program_counter);
                Some(self.mem_read_u16(p_addr).wrapping_add(self.register_y as u16))
            }
            AddressingMode::Accumulator => None,
            AddressingMode::NoAddressing => None 
        }
    }

    fn find_operand_parameter(&self, address_mode: &AddressingMode) -> Option<u8>
    {
        let parameter: Option<u8>;

        if let Some(addr) = self.find_operand_address(address_mode) {
           parameter = Some(self.mem_read(addr)) 
        } else {
            parameter = match address_mode {
                AddressingMode::Accumulator => {
                    Some(self.register_a)
                }
                _ => None
            }
        }
    
        parameter
    }

    fn adc(&mut self, mode: &AddressingMode) {
        // let addr = self.find_operand_address(&mode);
        // let param = self.mem_read(addr);

        // let overflow: bool;

        // self.flags.update_zero_negative_flags(param);
        // self.register_a += param + self.

        // (self.register_a, overflow) = self.register_a.overflowing_add(param);

        // self.flags.carry |= overflow;
    }

    fn lda(&mut self, mode: &AddressingMode) {
        if let Some(param) = self.find_operand_parameter(mode) {
            self.flags.update_zero_negative_flags(param);
            self.register_a = param;
        }
    }


    /// A logical AND is performed on the accumulator and the contents of a byte of memory.
    /// 
    /// A = A&M
    /// 
    /// Flags Effected: Z, N
    fn and(&mut self, mode: &AddressingMode)
    {
        if let Some(param) = self.find_operand_parameter(mode) {
            self.register_a = self.register_a & param;
            self.flags.update_zero_negative_flags(self.register_a);
        }
    }

    /// Shift all bits of the accumulator or a memory location by one bit left. Carry is set if the 
    /// result does not fit within 8-bits
    /// 
    /// A = A*2 or M=M*2
    /// 
    /// Flags Effected: Z, N, C
    fn asl(&mut self, mode: &AddressingMode)
    {
        if let Some(param) = self.find_operand_parameter(mode) {

            let param: u16 = (param as u16) << 1;

            self.flags.update_zero_negative_flags(param as u8);
            self.flags.update_carry_flag(param);

            if let Some(addr) = self.find_operand_address(mode) {
                self.mem_write(addr, param as u8);
            } else if matches!(mode, AddressingMode::Accumulator) {
                self.register_a = param as u8;
            }
        }
    }

    // Set Flag Opcodes

    fn sec(&mut self, _: &AddressingMode) {
        self.flags.carry = true;
    }

    fn sed(&mut self, _: &AddressingMode) {
        self.flags.decimal_mode= true;
    }

    fn sei(&mut self, _: &AddressingMode) {
        self.flags.interrupt_disable = true;
    }

}

#[cfg(test)]
mod test {
   use super::*;
 
   #[test]
   fn test_0xa9_lda_immediate_load_data() {
       let mut cpu = Cpu::new();
       cpu.load_program(vec![0xa9, 0x05, 0x00]);
       cpu.run();

       assert_eq!(cpu.register_a, 0x05);
       assert_eq!(cpu.flags.zero, false);
       assert_eq!(cpu.flags.negative, false);
   }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = Cpu::new();
        cpu.load_program(vec![0xa9, 0x00, 0x00]);
        cpu.run();
        assert_eq!(cpu.flags.zero, true);
    }

    #[test]
    fn test_0x69_adc_immediate() {
        let mut cpu = Cpu::new();
        cpu.load_program(vec![0xa9, 0x10, 0x69, 0x10, 0x00]);
        cpu.run();
        assert_eq!(cpu.register_a, 0x20);
        assert_eq!(cpu.flags.zero, false);
        assert_eq!(cpu.flags.negative, false);
    }

    #[test]
    fn test_0x69_adc_carry() {
        let mut cpu = Cpu::new();
        cpu.load_program(vec![0xa9, 0xFF, 0x69, 0x01, 0x00]);
        cpu.run();
        assert_eq!(cpu.register_a, 0x00);
        assert_eq!(cpu.flags.carry, true);
        assert_eq!(cpu.flags.negative, false);
    }

    #[test]
    fn test_0x69_adc_overflow_with_existing_carry() {
        let mut cpu = Cpu::new();
        cpu.load_program(vec![0x69, 0x01, 0x00]);

        cpu.flags.carry = true;

        cpu.run();
        assert_eq!(cpu.register_a, 0x01);
        assert_eq!(cpu.flags.carry, true);
        assert_eq!(cpu.flags.negative, false);
    }

    #[test]
    fn test_0x29_and_immediate() {
        let mut cpu = Cpu::new();
        cpu.load_program(vec![0xa9, 0x1, 0x29, 0x1, 0x00]);
        cpu.run();

        assert_eq!(cpu.register_a, 0x01);
    }

    #[test]
    fn test_0x0a_asl_accumulator() {
        let mut cpu = Cpu::new();
        cpu.load_program(vec![0xa9, 0x1, 0x0A, 0x00]);
        cpu.run();

        assert_eq!(cpu.register_a, 0x02);
    }

    #[test]
    fn test_0x38_sec() {
        let mut cpu = Cpu::new();
        cpu.load_program(vec![0x38, 0x00]);
        cpu.run();

        assert_eq!(cpu.flags.carry, true);
    }
    
    #[test]
    fn test_0xf8_sed() {
        let mut cpu = Cpu::new();
        cpu.load_program(vec![0xf8, 0x00]);
        cpu.run();

        assert_eq!(cpu.flags.decimal_mode, true);
    }

    #[test]
    fn test_0x78_sei() {
        let mut cpu = Cpu::new();
        cpu.load_program(vec![0x78, 0x00]);
        cpu.run();

        assert_eq!(cpu.flags.interrupt_disable, true);
    }
}
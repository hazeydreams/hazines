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
    NoAddressing,
}

#[derive(Debug)]
pub struct Flags {
    pub carry: bool,
    pub zero: bool,
    pub interrupt_disable: bool,
    pub decimal_mode: bool,
    pub overflow: bool,
    pub negative: bool,
}

impl Flags {
    fn new() -> Self {
        Self {
            carry: false,
            zero: false,
            interrupt_disable: false,
            decimal_mode: false,
            overflow: false,
            negative: false,
        }
    }

    fn update_zero_negative_flags(&mut self, data: u8) {
        self.zero = (data == 0);
        self.negative = (data & 0x80) != 0;
    }

    fn update_carry_flag(&mut self, data: u16) {
        self.carry = (data & 0x100) != 0;
    }
}


enum InstructionId {
    ADC, AND, ASL, BCC, BCS, BEQ, BIT, BMI,
    BNE, BPL, BRK, BVC, BVS, CLC, CLD, CLI,
    CLV, CMP, CPX, CPY, DEC, DEX, DEY, EOR,
    INC, INX, INY, JMP, JSR, LDA, LDX, LDY,
    LSR, NOP, ORA, PHA, PHP, PLA, PLP, ROL,
    ROR, RTI, RTS, SBC, SEC, SED, SEI, STA,
    STX, STY, TAX, TAY, TSX, TXA, TXS, TYA,
} 

struct Instruction {
    opcode: u8,
    id: InstructionId,
    tag: &'static str,
    width: u8,
    cycles: u8,
    addressing_mode: AddressingMode,
}

impl Instruction {
    const 

    fn new(
        opcode: u8,
        id: InstructionId,
        tag: &'static str,
        width: u8,
        cycles: u8,
        addressing_mode: AddressingMode,
    ) -> Self {
        Self {
            opcode,
            id,
            tag,
            width,
            cycles,
            addressing_mode,
        }
    }
}

lazy_static! {
    static ref CPU_INSTRUCTION_SET: Vec<Instruction> = vec![
        // ADC
        Instruction::new(0x69, InstructionId::ADC, "ADC", 2, 2, AddressingMode::Immediate),
        Instruction::new(0x65, InstructionId::ADC, "ADC", 2, 3, AddressingMode::ZeroPage),
        Instruction::new(0x75, InstructionId::ADC, "ADC", 2, 4, AddressingMode::ZeroPage_X),
        Instruction::new(0x6D, InstructionId::ADC, "ADC", 3, 4, AddressingMode::Absolute),
        Instruction::new(0x7D, InstructionId::ADC, "ADC", 3, 4, AddressingMode::Absolute_X),
        Instruction::new(0x79, InstructionId::ADC, "ADC", 3, 4, AddressingMode::Absolute_Y),
        Instruction::new(0x61, InstructionId::ADC, "ADC", 2, 6, AddressingMode::Indirect_X),
        Instruction::new(0x71, InstructionId::ADC, "ADC", 2, 5, AddressingMode::Indirect_Y),

        // AND
        Instruction::new(0x29, InstructionId::AND, "AND", 2, 2, AddressingMode::Immediate),
        Instruction::new(0x25, InstructionId::AND, "AND", 2, 3, AddressingMode::ZeroPage),
        Instruction::new(0x35, InstructionId::AND, "AND", 2, 4, AddressingMode::ZeroPage_X),
        Instruction::new(0x2D, InstructionId::AND, "AND", 3, 4, AddressingMode::Absolute),
        Instruction::new(0x3D, InstructionId::AND, "AND", 3, 4, AddressingMode::Absolute_X),
        Instruction::new(0x39, InstructionId::AND, "AND", 3, 4, AddressingMode::Absolute_Y),
        Instruction::new(0x21, InstructionId::AND, "AND", 2, 6, AddressingMode::Indirect_X),
        Instruction::new(0x31, InstructionId::AND, "AND", 2, 5, AddressingMode::Indirect_Y),

        // ASL
        Instruction::new(0x0A, InstructionId::ASL, "ASL", 1, 2, AddressingMode::Accumulator),
        Instruction::new(0x06, InstructionId::ASL, "ASL", 2, 5, AddressingMode::ZeroPage),
        Instruction::new(0x16, InstructionId::ASL, "ASL", 2, 6, AddressingMode::ZeroPage_X),
        Instruction::new(0x0E, InstructionId::ASL, "ASL", 3, 6, AddressingMode::Absolute),
        Instruction::new(0x1E, InstructionId::ASL, "ASL", 3, 7, AddressingMode::Absolute_X),

        // BCC
        Instruction::new(0x90, InstructionId::BCC, "BCC", 2, 2, AddressingMode::NoAddressing),

        // BCS
        Instruction::new(0xB0, InstructionId::BCS, "BCS", 2, 2, AddressingMode::NoAddressing),

        // BEQ
        Instruction::new(0xF0, InstructionId::BEQ, "BEQ", 2, 2, AddressingMode::NoAddressing),

        // BIT
        Instruction::new(0x24, InstructionId::BIT, "BIT", 2, 3, AddressingMode::ZeroPage),
        Instruction::new(0x2C, InstructionId::BIT, "BIT", 3, 4, AddressingMode::Absolute),

        // BMI
        Instruction::new(0x30, InstructionId::BMI, "BMI", 2, 2, AddressingMode::NoAddressing),

        // BNE
        Instruction::new(0xD0, InstructionId::BNE, "BNE", 2, 2, AddressingMode::NoAddressing),

        // BPL
        Instruction::new(0x10, InstructionId::BPL, "BPL", 2, 2, AddressingMode::NoAddressing),

        // BRK
        Instruction::new(0x00, InstructionId::BRK, "BRK", 1, 7, AddressingMode::NoAddressing),

        // BVC
        Instruction::new(0x50, InstructionId::BVC, "BVC", 2, 2, AddressingMode::NoAddressing),

        // BVS
        Instruction::new(0x70, InstructionId::BVS, "BVS", 2, 2, AddressingMode::NoAddressing),

        // CLC
        Instruction::new(0x18, InstructionId::CLC, "CLC", 1, 2, AddressingMode::NoAddressing),

        // CLD
        Instruction::new(0xD8, InstructionId::CLD, "CLD", 1, 2, AddressingMode::NoAddressing),

        // CLI
        Instruction::new(0x58, InstructionId::CLI, "CLI", 1, 2, AddressingMode::NoAddressing),

        // CLV
        Instruction::new(0xB8, InstructionId::CLV, "CLV", 1, 2, AddressingMode::NoAddressing),

        // CMP
        Instruction::new(0xC9, InstructionId::CMP, "CMP", 2, 2, AddressingMode::Immediate),
        Instruction::new(0xC5, InstructionId::CMP, "CMP", 2, 3, AddressingMode::ZeroPage),
        Instruction::new(0xD5, InstructionId::CMP, "CMP", 2, 4, AddressingMode::ZeroPage_X),
        Instruction::new(0xCD, InstructionId::CMP, "CMP", 3, 4, AddressingMode::Absolute),
        Instruction::new(0xDD, InstructionId::CMP, "CMP", 3, 4, AddressingMode::Absolute_X),
        Instruction::new(0xD9, InstructionId::CMP, "CMP", 3, 4, AddressingMode::Absolute_Y),
        Instruction::new(0xC1, InstructionId::CMP, "CMP", 2, 6, AddressingMode::Indirect_X),
        Instruction::new(0xD1, InstructionId::CMP, "CMP", 2, 5, AddressingMode::Indirect_Y),

        // CPX
        Instruction::new(0xE0, InstructionId::CPX, "CPX", 2, 2, AddressingMode::Immediate),
        Instruction::new(0xE4, InstructionId::CPX, "CPX", 2, 3, AddressingMode::ZeroPage),
        Instruction::new(0xEC, InstructionId::CPX, "CPX", 3, 4, AddressingMode::Absolute),

        // CPY
        Instruction::new(0xC0, InstructionId::CPY, "CPY", 2, 2, AddressingMode::Immediate),
        Instruction::new(0xC4, InstructionId::CPY, "CPY", 2, 3, AddressingMode::ZeroPage),
        Instruction::new(0xCC, InstructionId::CPY, "CPY", 3, 4, AddressingMode::Absolute),

        // DEC
        Instruction::new(0xC6, InstructionId::DEC, "DEC", 2, 5, AddressingMode::ZeroPage),
        Instruction::new(0xD6, InstructionId::DEC, "DEC", 2, 6, AddressingMode::ZeroPage_X),
        Instruction::new(0xCE, InstructionId::DEC, "DEC", 3, 6, AddressingMode::Absolute),
        Instruction::new(0xDE, InstructionId::DEC, "DEC", 3, 7, AddressingMode::Absolute_X),

        // DEX
        Instruction::new(0xCA, InstructionId::DEX, "DEX", 1, 2, AddressingMode::NoAddressing),

        // DEY
        Instruction::new(0x88, InstructionId::DEY, "DEY", 1, 2, AddressingMode::NoAddressing),

        // EOR
        Instruction::new(0x49, InstructionId::EOR, "EOR", 2, 2, AddressingMode::Immediate),
        Instruction::new(0x45, InstructionId::EOR, "EOR", 2, 3, AddressingMode::ZeroPage),
        Instruction::new(0x55, InstructionId::EOR, "EOR", 2, 4, AddressingMode::ZeroPage_X),
        Instruction::new(0x4D, InstructionId::EOR, "EOR", 3, 4, AddressingMode::Absolute),
        Instruction::new(0x5D, InstructionId::EOR, "EOR", 3, 4, AddressingMode::Absolute_X),
        Instruction::new(0x59, InstructionId::EOR, "EOR", 3, 4, AddressingMode::Absolute_Y),
        Instruction::new(0x41, InstructionId::EOR, "EOR", 2, 6, AddressingMode::Indirect_X),
        Instruction::new(0x51, InstructionId::EOR, "EOR", 2, 5, AddressingMode::Indirect_Y),

        // INC
        Instruction::new(0xE6, InstructionId::INC, "INC", 2, 5, AddressingMode::ZeroPage),
        Instruction::new(0xF6, InstructionId::INC, "INC", 2, 6, AddressingMode::ZeroPage_X),
        Instruction::new(0xEE, InstructionId::INC, "INC", 3, 6, AddressingMode::Absolute),
        Instruction::new(0xFE, InstructionId::INC, "INC", 3, 7, AddressingMode::Absolute_X),

        // INX
        Instruction::new(0xE8, InstructionId::INX, "INX", 1, 2, AddressingMode::NoAddressing),

        // INY
        Instruction::new(0xC8, InstructionId::INY, "INY", 1, 2, AddressingMode::NoAddressing),

        // JMP
        Instruction::new(0x4C, InstructionId::JMP, "JMP", 3, 3, AddressingMode::Absolute),
        Instruction::new(0x6C, InstructionId::JMP, "JMP", 3, 5, AddressingMode::Indirect),

        // JSR
        Instruction::new(0x20, InstructionId::JSR, "JSR", 3, 6, AddressingMode::Absolute),

        // LDA
        Instruction::new(0xA9, InstructionId::LDA, "LDA", 2, 2, AddressingMode::Immediate),
        Instruction::new(0xA5, InstructionId::LDA, "LDA", 2, 3, AddressingMode::ZeroPage),
        Instruction::new(0xB5, InstructionId::LDA, "LDA", 2, 4, AddressingMode::ZeroPage_X),
        Instruction::new(0xAD, InstructionId::LDA, "LDA", 3, 4, AddressingMode::Absolute),
        Instruction::new(0xBD, InstructionId::LDA, "LDA", 3, 4, AddressingMode::Absolute_X),
        Instruction::new(0xB9, InstructionId::LDA, "LDA", 3, 4, AddressingMode::Absolute_Y),
        Instruction::new(0xA1, InstructionId::LDA, "LDA", 2, 6, AddressingMode::Indirect_X),
        Instruction::new(0xB1, InstructionId::LDA, "LDA", 2, 5, AddressingMode::Indirect_Y),

        // LDX
        Instruction::new(0xA2, InstructionId::LDX, "LDX", 2, 2, AddressingMode::Immediate),
        Instruction::new(0xA6, InstructionId::LDX, "LDX", 2, 3, AddressingMode::ZeroPage),
        Instruction::new(0xB6, InstructionId::LDX, "LDX", 2, 4, AddressingMode::ZeroPage_Y),
        Instruction::new(0xAE, InstructionId::LDX, "LDX", 3, 4, AddressingMode::Absolute),
        Instruction::new(0xBE, InstructionId::LDX, "LDX", 3, 4, AddressingMode::Absolute_Y),

        // LDY
        Instruction::new(0xA0, InstructionId::LDY, "LDY", 2, 2, AddressingMode::Immediate),
        Instruction::new(0xA4, InstructionId::LDY, "LDY", 2, 3, AddressingMode::ZeroPage),
        Instruction::new(0xB4, InstructionId::LDY, "LDY", 2, 4, AddressingMode::ZeroPage_X),
        Instruction::new(0xAC, InstructionId::LDY, "LDY", 3, 4, AddressingMode::Absolute),
        Instruction::new(0xBC, InstructionId::LDY, "LDY", 3, 4, AddressingMode::Absolute_X),

        // LSR
        Instruction::new(0x4A, InstructionId::LSR, "LSR", 1, 2, AddressingMode::Accumulator),
        Instruction::new(0x46, InstructionId::LSR, "LSR", 2, 5, AddressingMode::ZeroPage),
        Instruction::new(0x56, InstructionId::LSR, "LSR", 2, 6, AddressingMode::ZeroPage_X),
        Instruction::new(0x4E, InstructionId::LSR, "LSR", 3, 6, AddressingMode::Absolute),
        Instruction::new(0x5E, InstructionId::LSR, "LSR", 3, 7, AddressingMode::Absolute_X),

        // NOP
        Instruction::new(0xEA, InstructionId::NOP, "NOP", 1, 2, AddressingMode::NoAddressing),

        // ORA
        Instruction::new(0x09, InstructionId::ORA, "ORA", 2, 2, AddressingMode::Immediate),
        Instruction::new(0x05, InstructionId::ORA, "ORA", 2, 3, AddressingMode::ZeroPage),
        Instruction::new(0x15, InstructionId::ORA, "ORA", 2, 4, AddressingMode::ZeroPage_X),
        Instruction::new(0x0D, InstructionId::ORA, "ORA", 3, 4, AddressingMode::Absolute),
        Instruction::new(0x1D, InstructionId::ORA, "ORA", 3, 4, AddressingMode::Absolute_X),
        Instruction::new(0x19, InstructionId::ORA, "ORA", 3, 4, AddressingMode::Absolute_Y),
        Instruction::new(0x01, InstructionId::ORA, "ORA", 2, 6, AddressingMode::Indirect_X),
        Instruction::new(0x11, InstructionId::ORA, "ORA", 2, 5, AddressingMode::Indirect_Y),

        // PHA
        Instruction::new(0x48, InstructionId::PHA, "PHA", 1, 3, AddressingMode::NoAddressing),

        // PHP
        Instruction::new(0x08, InstructionId::PHP, "PHP", 1, 3, AddressingMode::NoAddressing),

        // PLA
        Instruction::new(0x68, InstructionId::PLA, "PLA", 1, 4, AddressingMode::NoAddressing),

        // PLP
        Instruction::new(0x28, InstructionId::PLP, "PLP", 1, 4, AddressingMode::NoAddressing),

        // ROL
        Instruction::new(0x2A, InstructionId::ROL, "ROL", 1, 2, AddressingMode::Accumulator),
        Instruction::new(0x26, InstructionId::ROL, "ROL", 2, 5, AddressingMode::ZeroPage),
        Instruction::new(0x36, InstructionId::ROL, "ROL", 2, 6, AddressingMode::ZeroPage_X),
        Instruction::new(0x2E, InstructionId::ROL, "ROL", 3, 6, AddressingMode::Absolute),
        Instruction::new(0x3E, InstructionId::ROL, "ROL", 3, 7, AddressingMode::Absolute_X),

        // ROR
        Instruction::new(0x6A, InstructionId::ROR, "ROR", 1, 2, AddressingMode::Accumulator),
        Instruction::new(0x66, InstructionId::ROR, "ROR", 2, 5, AddressingMode::ZeroPage),
        Instruction::new(0x76, InstructionId::ROR, "ROR", 2, 6, AddressingMode::ZeroPage_X),
        Instruction::new(0x6E, InstructionId::ROR, "ROR", 3, 6, AddressingMode::Absolute),
        Instruction::new(0x7E, InstructionId::ROR, "ROR", 3, 7, AddressingMode::Absolute_X),

        // RTI
        Instruction::new(0x40, InstructionId::RTI, "RTI", 1, 6, AddressingMode::NoAddressing),

        // RTS
        Instruction::new(0x60, InstructionId::RTS, "RTS", 1, 6, AddressingMode::NoAddressing),

        // SBC
        Instruction::new(0xE9, InstructionId::SBC, "SBC", 2, 2, AddressingMode::Immediate),
        Instruction::new(0xE5, InstructionId::SBC, "SBC", 2, 3, AddressingMode::ZeroPage),
        Instruction::new(0xF5, InstructionId::SBC, "SBC", 2, 4, AddressingMode::ZeroPage_X),
        Instruction::new(0xED, InstructionId::SBC, "SBC", 3, 4, AddressingMode::Absolute),
        Instruction::new(0xFD, InstructionId::SBC, "SBC", 3, 4, AddressingMode::Absolute_X),
        Instruction::new(0xF9, InstructionId::SBC, "SBC", 3, 4, AddressingMode::Absolute_Y),
        Instruction::new(0xE1, InstructionId::SBC, "SBC", 2, 6, AddressingMode::Indirect_X),
        Instruction::new(0xF1, InstructionId::SBC, "SBC", 2, 5, AddressingMode::Indirect_Y),

        // SEC
        Instruction::new(0x38, InstructionId::SEC, "SEC", 1, 2, AddressingMode::NoAddressing),

        // SED
        Instruction::new(0xF8, InstructionId::SED, "SED", 1, 2, AddressingMode::NoAddressing),

        // SEI
        Instruction::new(0x78, InstructionId::SEI, "SEI", 1, 2, AddressingMode::NoAddressing),

        // STA
        Instruction::new(0x85, InstructionId::STA, "STA", 2, 3, AddressingMode::ZeroPage),
        Instruction::new(0x95, InstructionId::STA, "STA", 2, 4, AddressingMode::ZeroPage_X),
        Instruction::new(0x8D, InstructionId::STA, "STA", 3, 4, AddressingMode::Absolute),
        Instruction::new(0x9D, InstructionId::STA, "STA", 3, 5, AddressingMode::Absolute_X),
        Instruction::new(0x99, InstructionId::STA, "STA", 3, 5, AddressingMode::Absolute_Y),
        Instruction::new(0x81, InstructionId::STA, "STA", 2, 6, AddressingMode::Indirect_X),
        Instruction::new(0x91, InstructionId::STA, "STA", 2, 6, AddressingMode::Indirect_Y),

        // STX
        Instruction::new(0x86, InstructionId::STX, "STX", 2, 3, AddressingMode::ZeroPage),
        Instruction::new(0x96, InstructionId::STX, "STX", 2, 4, AddressingMode::ZeroPage_Y),
        Instruction::new(0x8E, InstructionId::STX, "STX", 3, 4, AddressingMode::Absolute),

        // STY
        Instruction::new(0x84, InstructionId::STY, "STY", 2, 3, AddressingMode::ZeroPage),
        Instruction::new(0x94, InstructionId::STY, "STY", 2, 4, AddressingMode::ZeroPage_X),
        Instruction::new(0x8C, InstructionId::STY, "STY", 3, 4, AddressingMode::Absolute),

        // TAX
        Instruction::new(0xAA, InstructionId::TAX, "TAX", 1, 2, AddressingMode::NoAddressing),

        // TAY
        Instruction::new(0xA8, InstructionId::TAY, "TAY", 1, 2, AddressingMode::NoAddressing),

        // TSX
        Instruction::new(0xBA, InstructionId::TSX, "TSX", 1, 2, AddressingMode::NoAddressing),

        // TXA
        Instruction::new(0x8A, InstructionId::TXA, "TXA", 1, 2, AddressingMode::NoAddressing),

        // TXS
        Instruction::new(0x9A, InstructionId::TXS, "TXS", 1, 2, AddressingMode::NoAddressing),

        // TYA
        Instruction::new(0x98, InstructionId::TYA, "TYA", 1, 2, AddressingMode::NoAddressing),
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
    memory: [u8; 0xFFFF],
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
            memory: [0; 0xFFFF],
        }
    }

    pub fn load_program(&mut self, program: Vec<u8>) {
        for i in 0..program.len() {
            self.mem_write(i as u16, program[i]);
        }
    }

    /// Run the program currently loaded into memory
    pub fn run(&mut self) {
        loop {
            let instruction = self.mem_read(self.program_counter);
            self.program_counter += 1;

            if let Some(opcode) = CPU_INSTRUCTION_SET
                .iter()
                .find(|opcode| opcode.opcode == instruction)
            {
                match opcode.id {
                    InstructionId::BRK => return,
                    InstructionId::ADC => self.adc(&opcode.addressing_mode),
                    InstructionId::AND => self.and(&opcode.addressing_mode),
                    InstructionId::ASL => self.asl(&opcode.addressing_mode),
                    InstructionId::BCC => self.bcc(&opcode.addressing_mode),
                    InstructionId::BCS => self.bcs(&opcode.addressing_mode),
                    InstructionId::BEQ => self.beq(&opcode.addressing_mode),
                    InstructionId::BIT => self.bit(&opcode.addressing_mode),
                    InstructionId::BMI => self.bmi(&opcode.addressing_mode),
                    InstructionId::BNE => self.bne(&opcode.addressing_mode),
                    InstructionId::BPL => self.bpl(&opcode.addressing_mode),
                    InstructionId::BVC => self.bvc(&opcode.addressing_mode),
                    InstructionId::BVS => self.bvs(&opcode.addressing_mode),
                    InstructionId::CLC => self.clc(&opcode.addressing_mode),
                    InstructionId::CLD => self.cld(&opcode.addressing_mode),
                    InstructionId::CLI => self.cli(&opcode.addressing_mode),
                    InstructionId::CLV => self.clv(&opcode.addressing_mode),
                    InstructionId::CMP => self.cmp(&opcode.addressing_mode),
                    InstructionId::CPX => self.cpx(&opcode.addressing_mode),
                    InstructionId::CPY => self.cpy(&opcode.addressing_mode),
                    InstructionId::DEC => self.dec(&opcode.addressing_mode),
                    InstructionId::DEX => self.dex(&opcode.addressing_mode),
                    InstructionId::DEY => self.dey(&opcode.addressing_mode),
                    InstructionId::EOR => self.eor(&opcode.addressing_mode),
                    InstructionId::INC => self.inc(&opcode.addressing_mode),
                    InstructionId::INX => self.inx(&opcode.addressing_mode),
                    InstructionId::INY => self.iny(&opcode.addressing_mode),
                    InstructionId::JMP => self.jmp(&opcode.addressing_mode),
                    InstructionId::JSR => self.jsr(&opcode.addressing_mode),
                    InstructionId::LDA => self.lda(&opcode.addressing_mode),
                    InstructionId::LDX => self.ldx(&opcode.addressing_mode),
                    InstructionId::LDY => self.ldy(&opcode.addressing_mode),
                    InstructionId::LSR => self.lsr(&opcode.addressing_mode),
                    InstructionId::NOP => self.nop(&opcode.addressing_mode),
                    InstructionId::ORA => self.ora(&opcode.addressing_mode),
                    InstructionId::PHA => self.pha(&opcode.addressing_mode),
                    InstructionId::PHP => self.php(&opcode.addressing_mode),
                    InstructionId::PLA => self.pla(&opcode.addressing_mode),
                    InstructionId::PLP => self.plp(&opcode.addressing_mode),
                    InstructionId::ROL => self.rol(&opcode.addressing_mode),
                    InstructionId::ROR => self.ror(&opcode.addressing_mode),
                    InstructionId::RTI => self.rti(&opcode.addressing_mode),
                    InstructionId::RTS => self.rts(&opcode.addressing_mode),
                    InstructionId::SBC => self.sbc(&opcode.addressing_mode),
                    InstructionId::SEC => self.sec(&opcode.addressing_mode),
                    InstructionId::SED => self.sed(&opcode.addressing_mode),
                    InstructionId::SEI => self.sei(&opcode.addressing_mode),
                    InstructionId::STA => self.sta(&opcode.addressing_mode),
                    InstructionId::STX => self.stx(&opcode.addressing_mode),
                    InstructionId::STY => self.sty(&opcode.addressing_mode),
                    InstructionId::TAX => self.tax(&opcode.addressing_mode),
                    InstructionId::TAY => self.tay(&opcode.addressing_mode),
                    InstructionId::TSX => self.tsx(&opcode.addressing_mode),
                    InstructionId::TXA => self.txa(&opcode.addressing_mode),
                    InstructionId::TXS => self.txs(&opcode.addressing_mode),
                    InstructionId::TYA => self.tya(&opcode.addressing_mode),
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

    fn find_operand_address(&self, address_mode: &AddressingMode) -> Option<u16> {
        match address_mode {
            AddressingMode::Immediate => Some(self.program_counter),
            AddressingMode::ZeroPage => Some(self.mem_read(self.program_counter) as u16),
            AddressingMode::Absolute => Some(self.mem_read_u16(self.program_counter)),
            AddressingMode::ZeroPage_X => Some(
                (self.mem_read(self.program_counter) as u16).wrapping_add(self.register_x as u16),
            ),
            AddressingMode::ZeroPage_Y => Some(
                (self.mem_read(self.program_counter) as u16).wrapping_add(self.register_y as u16),
            ),
            AddressingMode::Absolute_X => Some(
                (self.mem_read(self.program_counter) as u16).wrapping_add(self.register_x as u16),
            ),
            AddressingMode::Absolute_Y => Some(
                (self.mem_read(self.program_counter) as u16).wrapping_add(self.register_y as u16),
            ),
            AddressingMode::Indirect => {
                let p_addr = self.mem_read_u16(self.program_counter);
                Some(self.mem_read_u16(p_addr))
            }
            AddressingMode::Indirect_X => {
                let p_addr = self.mem_read_u16(self.program_counter);
                Some(
                    self.mem_read_u16(p_addr)
                        .wrapping_add(self.register_x as u16),
                )
            }
            AddressingMode::Indirect_Y => {
                let p_addr = self.mem_read_u16(self.program_counter);
                Some(
                    self.mem_read_u16(p_addr)
                        .wrapping_add(self.register_y as u16),
                )
            }
            AddressingMode::Accumulator => None,
            AddressingMode::NoAddressing => None,
        }
    }

    fn find_operand_parameter(&self, address_mode: &AddressingMode) -> Option<u8> {
        let parameter: Option<u8>;

        if let Some(addr) = self.find_operand_address(address_mode) {
            parameter = Some(self.mem_read(addr))
        } else {
            parameter = match address_mode {
                AddressingMode::Accumulator => Some(self.register_a),
                _ => None,
            }
        }

        parameter
    }

    // Instruction Implementations
    // -----------------------------------------------------------------------------------

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
    fn and(&mut self, mode: &AddressingMode) {
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
    fn asl(&mut self, mode: &AddressingMode) {
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

    fn sec(&mut self, _mode: &AddressingMode) {
        self.flags.carry = true;
    }

    fn sed(&mut self, _mode: &AddressingMode) {
        self.flags.decimal_mode = true;
    }

    fn sei(&mut self, _mode: &AddressingMode) {
        self.flags.interrupt_disable = true;
    }

    fn clc(&mut self, _mode: &AddressingMode) {
        self.flags.carry = false;
    }

    fn cld(&mut self, _mode: &AddressingMode) {
        self.flags.decimal_mode = false;
    }

    fn cli(&mut self, _mode: &AddressingMode) {
        self.flags.interrupt_disable = false;
    }

    fn clv(&mut self, _mode: &AddressingMode) {
        self.flags.overflow = false;
    }

    fn bcc(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn bcs(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn beq(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn bit(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn bmi(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn bne(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn bpl(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn bvc(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn bvs(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn cmp(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn cpx(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn cpy(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn dec(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn dex(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn dey(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn eor(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn inc(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn inx(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn iny(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn jmp(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn jsr(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn lsr(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn nop(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn ora(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn pha(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn php(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn pla(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn plp(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn rol(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn ror(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn rti(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn rts(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn sta(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn stx(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn sty(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn tax(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn tay(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn tsx(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn txa(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn txs(&mut self, mode: &AddressingMode) {
        todo!()
    }

    fn tya(&mut self, mode: &AddressingMode) {
        todo!()
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

    #[test]
    fn test_0x18_clc() {
        let mut cpu = Cpu::new();
        cpu.load_program(vec![0x18, 0x00]);

        cpu.flags.carry = true;

        cpu.run();

        assert_eq!(cpu.flags.carry, true);
    }

    #[test]
    fn test_0xd8_cld() {
        let mut cpu = Cpu::new();
        cpu.load_program(vec![0xd8, 0x00]);

        cpu.flags.decimal_mode = true;

        cpu.run();

        assert_eq!(cpu.flags.decimal_mode, false);
    }

    #[test]
    fn test_0x58_cli() {
        let mut cpu = Cpu::new();
        cpu.load_program(vec![0x58, 0x00]);

        cpu.flags.interrupt_disable = true;

        cpu.run();

        assert_eq!(cpu.flags.interrupt_disable, false);
    }
    #[test]
    fn test_0xb8_cli() {
        let mut cpu = Cpu::new();
        cpu.load_program(vec![0xb8, 0x00]);

        cpu.flags.overflow = true;

        cpu.run();

        assert_eq!(cpu.flags.overflow, false);
    }
}

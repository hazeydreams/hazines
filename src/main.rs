mod cpu;

use crate::cpu::Cpu;

fn main() {
    let mut cpu = Cpu::new();
    cpu.load_program(vec![0xb8, 0x00]);
    cpu.run();
}

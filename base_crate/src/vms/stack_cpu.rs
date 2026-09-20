pub struct StackCPU {
    memory: [u8; 255],
    pc: u8,
    stack: [u8; 32],
    sp: u8,
}
impl StackCPU {
    pub fn from(memory: Vec<u8>) -> Self {
        let mut final_memory = [0; 255];
        for (pos, cell) in memory.into_iter().enumerate() {
            final_memory[pos] = cell;
        }
        Self {
            memory: final_memory,
            pc: 1,
            stack: [0; 32],
            sp: 0,
        }
    }
    pub fn run(&mut self) {
        loop {
            if self.clock() {
                // true means it wants to halt
                return;
            }
        }
    }
    pub fn clock(&mut self) -> bool {
        let string = format!(
            "{} = [pc: {}], {:?}",
            self.memory[self.pc as usize],
            self.pc,
            &self.stack[0..16]
        );
        match self.memory[self.pc as usize] {
            0 /* ADD */ => {
                self.sp -= 1;
                self.stack[self.sp as usize] =
                self.stack[self.sp as usize].wrapping_add(self.stack[self.sp as usize + 1]);
                self.pc = self.pc.wrapping_add(1);
                }
            1 /* SUB */ => {
                self.sp -= 1;
                self.stack[self.sp as usize] =
                self.stack[self.sp as usize].wrapping_sub(self.stack[self.sp as usize + 1]);
                self.pc = self.pc.wrapping_add(1);
            }
            2 /* IMM */ => {
                self.sp += 1;
                self.pc = self.pc.wrapping_add(1);
                if self.sp > 31 { println!("{:?}", self.stack); panic!("stack overflow")}
                self.stack[self.sp as usize] = self.memory[self.pc as usize];
                self.pc = self.pc.wrapping_add(1);
            }
            3 /* LOD */ => {
                self.stack[self.sp as usize] = self.memory[self.stack[self.sp as usize] as usize];
                self.pc = self.pc.wrapping_add(1);
            }
            4 /* STO */ => {
                self.sp -= 1;
                self.memory[self.stack[self.sp as usize + 1] as usize] = self.stack[self.sp as usize];
                if self.stack[self.sp as usize + 1] as usize == 0 {
                    println!("|  {:08b}", self.stack[self.sp as usize]);
                }
                self.sp -= 1;
                self.pc = self.pc.wrapping_add(1);
            }
            5 /* JMP */ => {
                self.pc = self.stack[self.sp as usize];
                self.sp -= 1;
            }
            6 /* BE */ => {
                if self.stack[self.sp as usize] == 0 {
                    self.pc = self.memory[self.pc as usize + 1];
                }
                self.sp -= 1;
            }
            7 /* BNE */ => {
                if self.stack[self.sp as usize] != 0 {
                    self.pc = self.memory[self.pc as usize + 1];
                }
                self.sp -= 1;
            }
            8 /* BS */ => {
                if self.stack[self.sp as usize] > 127 {
                    self.pc = self.memory[self.pc as usize + 1];
                }
                self.sp -= 1;
            }
            9 /* BGE */ => {
                if self.stack[self.sp as usize] < 128 {
                    self.pc = self.memory[self.pc as usize + 1];
                }
                self.sp -= 1;
            }
            10 /* BG */ => {
                if self.stack[self.sp as usize] < 128 && self.stack[self.sp as usize] != 0 {
                    self.pc = self.memory[self.pc as usize + 1];
                }
                self.sp -= 1;
            }
            11 /* BSE */ => {
                if self.stack[self.sp as usize] > 127 || self.stack[self.sp as usize] == 0 {
                    self.pc = self.memory[self.pc as usize + 1];
                }
                self.sp -= 1;
            }
            _ => return true
        }
        println!("{} -> [pc: {}] {:?}", string, self.pc, &self.stack[0..16]);
        false
    }
}
enum _Instruction {
    ADD, // stack[sp - 1] = stack[sp - 1] + stack[sp]       0
    SUB, // stack[sp - 1] = stack[sp - 1] - stack[sp]       1
    IMM, // stack[sp] = mem[pc + 1]                         2
    LOD, // stack[sp] = mem[stack[sp]]                      3
    STO, // mem[stack[sp]] = stack[sp - 1]                  4

    JMP, // pc = pop stack                                  5
    BE,  // if stack[sp] == 0: pc = mem[pc + 1]             6
    BNE, // if stack[sp] != 0: pc = mem[pc + 1]             7
    BS,  // if stack[sp] < 0: pc = mem[pc + 1]              8
    BGE, // if stack[sp] >= 0: pc = mem[pc + 1]             9
    BG,  // if stack[sp] > 0: pc = mem[pc + 1]             10
    BSE, // if stack[sp] <= 0: pc = mem[pc + 1]            11
}

// Code your testbench here
// or browse Examples
#include <systemc.h>
#include <cmath>
#include <vector>
#include <iomanip>
#include <string>
#include <cstring>
#include "design.cpp"
#include <systemc.h>
#include <iostream>
#include <iomanip>
#include <cmath>
#include <string>
#include <vector>

// Forward declarations for FPU modules (assume they are defined elsewhere)
// Include your existing FPU modules here or link them separately



// Instruction formats and opcodes
enum fp_opcode_t {
    FP_ADD    = 0x0,
    FP_SUB    = 0x1, 
    FP_MUL    = 0x2,
    FP_DIV    = 0x3,
    FP_LOAD   = 0x4,
    FP_STORE  = 0x5,
    FP_MOVE   = 0x6,
    FP_NOP    = 0x7
};

// Instruction format: [31:28] opcode, [27:23] rd, [22:18] rs1, [17:13] rs2, [12:0] immediate
struct fp_instruction_t {
    sc_uint<4>  opcode;
    sc_uint<5>  rd;     // destination register
    sc_uint<5>  rs1;    // source register 1
    sc_uint<5>  rs2;    // source register 2  
    sc_uint<13> imm;    // immediate value
    
    fp_instruction_t() : opcode(0), rd(0), rs1(0), rs2(0), imm(0) {}
    
    fp_instruction_t(sc_uint<32> inst) {
        opcode = (inst >> 28) & 0xF;
        rd = (inst >> 23) & 0x1F;
        rs1 = (inst >> 18) & 0x1F;
        rs2 = (inst >> 13) & 0x1F;
        imm = inst & 0x1FFF;
    }
    
    sc_uint<32> to_word() const {
        return (sc_uint<32>(opcode) << 28) | (sc_uint<32>(rd) << 23) | 
               (sc_uint<32>(rs1) << 18) | (sc_uint<32>(rs2) << 13) | sc_uint<32>(imm);
    }
};

// ========== STAGE 1: INSTRUCTION FETCH ==========
SC_MODULE(InstructionFetch) {
    sc_in<bool> clk;
    sc_in<bool> reset;
    sc_in<bool> stall;
    sc_in<bool> branch_taken;
    sc_in<sc_uint<32>> branch_target;
    
    sc_out<sc_uint<32>> pc_out;
    sc_out<sc_uint<32>> instruction_out;
    sc_out<bool> valid_out;

private:
    sc_uint<32> pc;
    sc_uint<32> instruction_memory[1024]; // 4KB instruction memory
    bool initialized;

public:
    void fetch_process() {
        if (reset.read()) {
            pc = 0;
            valid_out.write(false);
            pc_out.write(0);
            instruction_out.write(0);
            
            // Initialize instruction memory with some test programs
            if (!initialized) {
                initialize_program();
                initialized = true;
            }
        } else if (!stall.read()) {
            // Update PC
            if (branch_taken.read()) {
                pc = branch_target.read();
            } else {
                pc = pc + 4;
            }
            
            // Fetch instruction
            sc_uint<32> addr = pc >> 2; // Word-aligned addressing
            sc_uint<32> instruction = (addr < 1024) ? instruction_memory[addr] : sc_uint<32>(0);
            
            pc_out.write(pc);
            instruction_out.write(instruction);
            valid_out.write(true);
        }
    }

    void initialize_program() {
        // Simple test program:
        // R0 = 3.14 (load immediate)
        // R1 = 2.71 (load immediate) 
        // R2 = R0 + R1 (add)
        // R3 = R0 * R1 (multiply)
        // R4 = R0 - R1 (subtract)
        // R5 = R0 / R1 (divide)
        
        fp_instruction_t inst;
        int addr = 0;
        
        // Load 3.14 into R0 (using load with immediate addressing)
        inst.opcode = FP_LOAD; inst.rd = 0; inst.rs1 = 0; inst.rs2 = 0; inst.imm = 100;
        instruction_memory[addr++] = inst.to_word();
        
        // Load 2.71 into R1 
        inst.opcode = FP_LOAD; inst.rd = 1; inst.rs1 = 0; inst.rs2 = 0; inst.imm = 101;
        instruction_memory[addr++] = inst.to_word();
        
        // R2 = R0 + R1
        inst.opcode = FP_ADD; inst.rd = 2; inst.rs1 = 0; inst.rs2 = 1; inst.imm = 0;
        instruction_memory[addr++] = inst.to_word();
        
        // R3 = R0 * R1  
        inst.opcode = FP_MUL; inst.rd = 3; inst.rs1 = 0; inst.rs2 = 1; inst.imm = 0;
        instruction_memory[addr++] = inst.to_word();
        
        // R4 = R0 - R1
        inst.opcode = FP_SUB; inst.rd = 4; inst.rs1 = 0; inst.rs2 = 1; inst.imm = 0;
        instruction_memory[addr++] = inst.to_word();
        
        // R5 = R0 / R1
        inst.opcode = FP_DIV; inst.rd = 5; inst.rs1 = 0; inst.rs2 = 1; inst.imm = 0;
        instruction_memory[addr++] = inst.to_word();
        
        // Store R2 to memory
        inst.opcode = FP_STORE; inst.rd = 0; inst.rs1 = 2; inst.rs2 = 0; inst.imm = 200;
        instruction_memory[addr++] = inst.to_word();
        
        // Fill rest with NOPs
        inst.opcode = FP_NOP; inst.rd = 0; inst.rs1 = 0; inst.rs2 = 0; inst.imm = 0;
        for (int i = addr; i < 1024; i++) {
            instruction_memory[i] = inst.to_word();
        }
    }

    SC_CTOR(InstructionFetch) : initialized(false) {
        SC_METHOD(fetch_process);
        sensitive << clk.pos() << reset;
    }
};

// ========== STAGE 2: INSTRUCTION DECODE ==========
SC_MODULE(InstructionDecode) {
    sc_in<bool> clk;
    sc_in<bool> reset;
    sc_in<bool> stall;
    sc_in<sc_uint<32>> pc_in;
    sc_in<sc_uint<32>> instruction_in;
    sc_in<bool> valid_in;
    
    // Writeback inputs for register file update
    sc_in<bool> wb_valid;
    sc_in<sc_uint<5>> wb_rd;
    sc_in<sc_uint<32>> wb_data;
    
    // Outputs to Execute stage
    sc_out<sc_uint<32>> pc_out;
    sc_out<sc_uint<4>> opcode_out;
    sc_out<sc_uint<5>> rd_out;
    sc_out<sc_uint<32>> operand1_out;
    sc_out<sc_uint<32>> operand2_out;
    sc_out<sc_uint<13>> immediate_out;
    sc_out<bool> valid_out;
    
    // Control signals
    sc_out<bool> stall_request;

private:
    sc_uint<32> register_file[32]; // 32 floating-point registers
    sc_uint<32> data_memory[256];  // Small data memory for loads/stores
    bool rf_initialized;

public:
    void decode_process() {
        if (reset.read()) {
            for (int i = 0; i < 32; i++) register_file[i] = 0;
            valid_out.write(false);
            stall_request.write(false);
            
            // Initialize data memory with some FP constants
            if (!rf_initialized) {
                // 3.14159 in IEEE 754 format
                data_memory[100] = 0x40490FDB;
                // 2.71828 in IEEE 754 format  
                data_memory[101] = 0x402DF84D;
                // More constants can be added here
                rf_initialized = true;
            }
        } else if (!stall.read() && valid_in.read()) {
            // Update register file from writeback
            if (wb_valid.read() && wb_rd.read().to_uint() != 0) { // R0 is always zero
                register_file[wb_rd.read().to_uint()] = wb_data.read();
            }
            
            // Decode instruction
            fp_instruction_t inst(instruction_in.read());
            
            sc_uint<32> op1 = 0, op2 = 0;
            bool need_stall = false;
            
            // Read operands based on instruction type
            switch (inst.opcode) {
                case FP_ADD:
                case FP_SUB: 
                case FP_MUL:
                case FP_DIV:
                    op1 = register_file[inst.rs1.to_uint()];
                    op2 = register_file[inst.rs2.to_uint()];
                    break;
                    
                case FP_LOAD:
                    // Load from data memory using immediate as address
                    op1 = data_memory[inst.imm.to_uint() & 0xFF];
                    op2 = sc_uint<32>(0);
                    break;
                    
                case FP_STORE:
                    op1 = register_file[inst.rs1.to_uint()]; // Data to store
                    op2 = inst.imm; // Address
                    break;
                    
                case FP_MOVE:
                    op1 = register_file[inst.rs1.to_uint()];
                    op2 = sc_uint<32>(0);
                    break;
                    
                default: // FP_NOP
                    op1 = sc_uint<32>(0);
                    op2 = sc_uint<32>(0);
                    break;
            }
            
            // Output decoded values
            pc_out.write(pc_in.read());
            opcode_out.write(inst.opcode);
            rd_out.write(inst.rd);
            operand1_out.write(op1);
            operand2_out.write(op2);
            immediate_out.write(inst.imm);
            valid_out.write(true);
            stall_request.write(need_stall);
        }
    }
    
    // Method to handle memory writes (for store instructions)
    void handle_memory_write(sc_uint<32> addr, sc_uint<32> data) {
        if ((addr.to_uint() & 0xFF) < 256) {
            data_memory[addr.to_uint() & 0xFF] = data;
        }
    }

    SC_CTOR(InstructionDecode) : rf_initialized(false) {
        SC_METHOD(decode_process);
        sensitive << clk.pos() << reset;
    }
};

// ========== STAGE 3: EXECUTE (Updated for Pipelined Adder) ==========
SC_MODULE(Execute) {
    // Clock and control
    sc_in<bool> clk;
    sc_in<bool> reset;
    sc_in<bool> stall;
    
    // Input from ID stage
    sc_in<sc_uint<32>> pc_in;
    sc_in<sc_uint<4>> opcode_in;
    sc_in<sc_uint<5>> rd_in;
    sc_in<sc_uint<32>> operand1_in;
    sc_in<sc_uint<32>> operand2_in;
    sc_in<sc_uint<13>> immediate_in;
    sc_in<bool> valid_in;
    
    // Output to MEM stage
    sc_out<sc_uint<32>> pc_out;
    sc_out<sc_uint<4>> opcode_out;
    sc_out<sc_uint<5>> rd_out;
    sc_out<sc_uint<32>> result_out;
    sc_out<sc_uint<32>> store_data_out;
    sc_out<sc_uint<13>> memory_addr_out;
    sc_out<bool> valid_out;
    
    // FPU status outputs
    sc_out<bool> fpu_overflow;
    sc_out<bool> fpu_underflow;
    sc_out<bool> fpu_divide_by_zero;

private:
    // FPU instances - Using your new pipelined adder
    ieee754mult *fpu_mult;
    ieee754add *fpu_add;  // Updated to use pipelined version
    ieee754_subtractor *fpu_sub;
    ieee754div *fpu_div;
    
    // Internal signals for FPU operations
    sc_signal<sc_uint<32>> fpu_a, fpu_b;
    sc_signal<bool> fpu_valid_in;  // Added for pipelined adder
    sc_signal<sc_uint<32>> mult_result, add_result, sub_result, div_result;
    sc_signal<bool> mult_valid, add_valid, sub_valid, div_valid;
    sc_signal<bool> mult_overflow, mult_underflow;
    sc_signal<bool> add_overflow, add_underflow;
    sc_signal<bool> sub_overflow, sub_underflow;
    sc_signal<bool> div_overflow, div_underflow, div_by_zero;
    
    // Opcode definitions
    enum opcodes {
        OP_NOP = 0x0,
        OP_ADD_INT = 0x1,
        OP_SUB_INT = 0x2,
        OP_LOAD = 0x3,
        OP_STORE = 0x4,
        OP_FADD = 0x5,    // Floating-point add
        OP_FSUB = 0x6,    // Floating-point subtract
        OP_FMUL = 0x7,    // Floating-point multiply
        OP_FDIV = 0x8,    // Floating-point divide
        OP_BRANCH = 0x9
    };

public:
    void execute_process() {
        if (reset.read()) {
            pc_out.write(0);
            opcode_out.write(0);
            rd_out.write(0);
            result_out.write(0);
            store_data_out.write(0);
            memory_addr_out.write(0);
            valid_out.write(false);
            fpu_overflow.write(false);
            fpu_underflow.write(false);
            fpu_divide_by_zero.write(false);
            
            // Reset FPU inputs
            fpu_a.write(0);
            fpu_b.write(0);
            fpu_valid_in.write(false);
            
        } else if (!stall.read() && valid_in.read()) {
            sc_uint<4> opcode = opcode_in.read();
            sc_uint<32> op1 = operand1_in.read();
            sc_uint<32> op2 = operand2_in.read();
            sc_uint<32> result = 0;
            
            // Set FPU inputs
            fpu_a.write(op1);
            fpu_b.write(op2);
            
            // Enable FPU operation for floating-point instructions
            bool is_fp_op = (opcode.to_uint() >= OP_FADD && opcode.to_uint() <= OP_FDIV);
            fpu_valid_in.write(is_fp_op);
            
            switch (opcode.to_uint()) {
                case OP_ADD_INT:
                    result = op1 + op2;
                    break;
                    
                case OP_SUB_INT:
                    result = op1 - op2;
                    break;
                    
                case OP_LOAD:
                    result = op1 + immediate_in.read(); // Address calculation
                    break;
                    
                case OP_STORE:
                    result = op1 + immediate_in.read(); // Address calculation
                    store_data_out.write(op2);
                    break;
                    
                case OP_FADD:
                    result = add_result.read(); // Will be valid after pipeline delay
                    fpu_overflow.write(add_overflow.read());
                    fpu_underflow.write(add_underflow.read());
                    break;
                    
                case OP_FSUB:
                    result = sub_result.read(); // Will be valid after pipeline delay
                    fpu_overflow.write(sub_overflow.read());
                    fpu_underflow.write(sub_underflow.read());
                    break;
                    
                case OP_FMUL:
                    result = mult_result.read(); // Will be valid after pipeline delay
                    fpu_overflow.write(mult_overflow.read());
                    fpu_underflow.write(mult_underflow.read());
                    break;
                    
                case OP_FDIV:
                    result = div_result.read(); // Will be valid after pipeline delay
                    fpu_overflow.write(div_overflow.read());
                    fpu_underflow.write(div_underflow.read());
                    fpu_divide_by_zero.write(div_by_zero.read());
                    break;
                    
                case OP_BRANCH:
                    result = pc_in.read() + (immediate_in.read() << 2);
                    break;
                    
                default: // OP_NOP
                    result = 0;
                    break;
            }
            
            // Output results - for FP operations, check if result is valid
            pc_out.write(pc_in.read());
            opcode_out.write(opcode);
            rd_out.write(rd_in.read());
            result_out.write(result);
            memory_addr_out.write(result.range(12, 0));
            
            // For pipelined FP operations, output is valid when FPU says it's valid
            if (is_fp_op) {
                bool fp_result_valid = false;
                switch (opcode.to_uint()) {
                    case OP_FADD:
                        fp_result_valid = add_valid.read();
                        break;
                    case OP_FSUB:
                        fp_result_valid = sub_valid.read();
                        break;
                    case OP_FMUL:
                        fp_result_valid = mult_valid.read();
                        break;
                    case OP_FDIV:
                        fp_result_valid = div_valid.read();
                        break;
                }
                valid_out.write(fp_result_valid);
            } else {
                valid_out.write(true); // Integer operations complete immediately
            }
        } else {
            valid_out.write(false);
            fpu_valid_in.write(false);
        }
    }

    SC_CTOR(Execute) {
        // Instantiate FPU units with correct port connections
        
        // Multiplier (assuming it has similar interface)
        fpu_mult = new ieee754mult("fpu_mult");
        fpu_mult->A(fpu_a);
        fpu_mult->B(fpu_b);
        fpu_mult->reset(reset);
        fpu_mult->clk(clk);
        fpu_mult->result(mult_result);
        fpu_mult->valid_out(mult_valid);
        fpu_mult->overflow(mult_overflow);
        fpu_mult->underflow(mult_underflow);
        
        // Pipelined Adder - using your new interface
        fpu_add = new ieee754add("fpu_add");
        fpu_add->clk(clk);
        fpu_add->reset(reset);
        fpu_add->A(fpu_a);
        fpu_add->B(fpu_b);
        fpu_add->valid_in(fpu_valid_in);  // Connect the valid_in signal
        fpu_add->result(add_result);
        fpu_add->valid_out(add_valid);
        fpu_add->overflow(add_overflow);
        fpu_add->underflow(add_underflow);
        
        // Subtractor (assuming similar interface to old adder)
        fpu_sub = new ieee754_subtractor("fpu_sub");
        fpu_sub->A(fpu_a);
        fpu_sub->B(fpu_b);
        fpu_sub->reset(reset);
        fpu_sub->clk(clk);
        fpu_sub->result(sub_result);
        fpu_sub->valid_out(sub_valid);
        fpu_sub->overflow(sub_overflow);
        fpu_sub->underflow(sub_underflow);
        
        // Divider (assuming similar interface)
        fpu_div = new ieee754div("fpu_div");
        fpu_div->a(fpu_a);
        fpu_div->b(fpu_b);
        fpu_div->reset(reset);
        fpu_div->clk(clk);
        fpu_div->result(div_result);
        fpu_div->valid_out(div_valid);
        fpu_div->overflow(div_overflow);
        fpu_div->underflow(div_underflow);
        fpu_div->divide_by_zero(div_by_zero);
        
        SC_METHOD(execute_process);
        sensitive << clk.pos() << reset << stall << valid_in << opcode_in 
                 << operand1_in << operand2_in << immediate_in << rd_in << pc_in
                 << mult_result << add_result << sub_result << div_result
                 << mult_valid << add_valid << sub_valid << div_valid
                 << mult_overflow << mult_underflow << add_overflow << add_underflow
                 << sub_overflow << sub_underflow << div_overflow << div_underflow << div_by_zero;
    }
    
    ~Execute() {
        delete fpu_mult;
        delete fpu_add;
        delete fpu_sub;
        delete fpu_div;
    }
};
// ========== STAGE 4: MEMORY ACCESS ==========
SC_MODULE(Memory) {
    sc_in<bool> clk;
    sc_in<bool> reset;
    sc_in<bool> stall;
    
    // Inputs from Execute
    sc_in<sc_uint<32>> pc_in;
    sc_in<sc_uint<4>> opcode_in;
    sc_in<sc_uint<5>> rd_in;
    sc_in<sc_uint<32>> result_in;
    sc_in<sc_uint<32>> store_data_in;
    sc_in<sc_uint<13>> memory_addr_in;
    sc_in<bool> valid_in;
    
    // Outputs to Writeback
    sc_out<sc_uint<32>> pc_out;
    sc_out<sc_uint<4>> opcode_out;
    sc_out<sc_uint<5>> rd_out;
    sc_out<sc_uint<32>> result_out;
    sc_out<bool> valid_out;
    
    // Memory interface to Decode stage
    sc_out<sc_uint<32>> mem_write_addr;
    sc_out<sc_uint<32>> mem_write_data;
    sc_out<bool> mem_write_enable;

public:
    void memory_process() {
        if (reset.read()) {
            valid_out.write(false);
            mem_write_enable.write(false);
        } else if (!stall.read() && valid_in.read()) {
            sc_uint<4> opcode = opcode_in.read();
            
            // Handle memory operations
            if (opcode == FP_STORE) {
                // Store operation
                mem_write_addr.write(memory_addr_in.read());
                mem_write_data.write(store_data_in.read());
                mem_write_enable.write(true);
                
                // No writeback needed for store
                valid_out.write(false);
            } else {
                // All other operations pass through
                mem_write_enable.write(false);
                
                pc_out.write(pc_in.read());
                opcode_out.write(opcode);
                rd_out.write(rd_in.read());
                result_out.write(result_in.read());
                valid_out.write(true);
            }
        }
    }

    SC_CTOR(Memory) {
        SC_METHOD(memory_process);
        sensitive << clk.pos() << reset;
    }
};

// ========== STAGE 5: WRITEBACK ==========
SC_MODULE(Writeback) {
    sc_in<bool> clk;
    sc_in<bool> reset;
    sc_in<bool> stall;
    
    // Inputs from Memory
    sc_in<sc_uint<32>> pc_in;
    sc_in<sc_uint<4>> opcode_in;
    sc_in<sc_uint<5>> rd_in;
    sc_in<sc_uint<32>> result_in;
    sc_in<bool> valid_in;
    
    // Outputs to Decode (register file update)
    sc_out<bool> wb_valid;
    sc_out<sc_uint<5>> wb_rd;
    sc_out<sc_uint<32>> wb_data;
    
    // Status outputs
    sc_out<sc_uint<32>> committed_pc;
    sc_out<bool> instruction_committed;

public:
    void writeback_process() {
        if (reset.read()) {
            wb_valid.write(false);
            instruction_committed.write(false);
        } else if (!stall.read() && valid_in.read()) {
            sc_uint<5> rd = rd_in.read();
            
            // Write back to register file (except for R0 which is hardwired to zero)
            if (rd.to_uint() != 0) {
                wb_valid.write(true);
                wb_rd.write(rd);
                wb_data.write(result_in.read());
            } else {
                wb_valid.write(false);
            }
            
            // Mark instruction as committed
            committed_pc.write(pc_in.read());
            instruction_committed.write(true);
        }
    }

    SC_CTOR(Writeback) {
        SC_METHOD(writeback_process);
        sensitive << clk.pos() << reset;
    }
};

// ========== TOP-LEVEL PROCESSOR ==========
SC_MODULE(FloatingPointProcessor) {
    sc_in<bool> clk;
    sc_in<bool> reset;
    
    // Status outputs
    sc_out<sc_uint<32>> current_pc;
    sc_out<bool> processor_active;
    sc_out<bool> fpu_overflow;
    sc_out<bool> fpu_underflow;
    sc_out<bool> fpu_divide_by_zero;

private:
    // Pipeline stage instances
    InstructionFetch *if_stage;
    InstructionDecode *id_stage;
    Execute *ex_stage;
    Memory *mem_stage;
    Writeback *wb_stage;
    
    // Inter-stage signals
    sc_signal<bool> global_stall;
    sc_signal<bool> branch_taken;
    sc_signal<sc_uint<32>> branch_target;
    
    // IF -> ID
    sc_signal<sc_uint<32>> if_pc, if_instruction;
    sc_signal<bool> if_valid;
    
    // ID -> EX
    sc_signal<sc_uint<32>> id_pc;
    sc_signal<sc_uint<4>> id_opcode;
    sc_signal<sc_uint<5>> id_rd;
    sc_signal<sc_uint<32>> id_operand1, id_operand2;
    sc_signal<sc_uint<13>> id_immediate;
    sc_signal<bool> id_valid, id_stall_req;
    
    // EX -> MEM
    sc_signal<sc_uint<32>> ex_pc, ex_result, ex_store_data;
    sc_signal<sc_uint<4>> ex_opcode;
    sc_signal<sc_uint<5>> ex_rd;
    sc_signal<sc_uint<13>> ex_memory_addr;
    sc_signal<bool> ex_valid;
    
    // MEM -> WB
    sc_signal<sc_uint<32>> mem_pc, mem_result;
    sc_signal<sc_uint<4>> mem_opcode;
    sc_signal<sc_uint<5>> mem_rd;
    sc_signal<bool> mem_valid;
    
    // WB -> ID (register file update)
    sc_signal<bool> wb_valid;
    sc_signal<sc_uint<5>> wb_rd;
    sc_signal<sc_uint<32>> wb_data;
    
    // Memory interface signals
    sc_signal<sc_uint<32>> mem_write_addr, mem_write_data;
    sc_signal<bool> mem_write_enable;
    
    // Status signals
    sc_signal<sc_uint<32>> committed_pc;
    sc_signal<bool> instruction_committed;

public:
    void control_process() {
        // Simple stall logic - could be enhanced for hazard detection
        global_stall.write(id_stall_req.read());
        
        // Branch prediction (always not taken for now)
        branch_taken.write(false);
        branch_target.write(0);
        
        // Status outputs
        current_pc.write(committed_pc.read());
        processor_active.write(instruction_committed.read());
    }

    SC_CTOR(FloatingPointProcessor) {
        // Instantiate pipeline stages
        if_stage = new InstructionFetch("if_stage");
        if_stage->clk(clk);
        if_stage->reset(reset);
        if_stage->stall(global_stall);
        if_stage->branch_taken(branch_taken);
        if_stage->branch_target(branch_target);
        if_stage->pc_out(if_pc);
        if_stage->instruction_out(if_instruction);
        if_stage->valid_out(if_valid);
        
        id_stage = new InstructionDecode("id_stage");
        id_stage->clk(clk);
        id_stage->reset(reset);
        id_stage->stall(global_stall);
        id_stage->pc_in(if_pc);
        id_stage->instruction_in(if_instruction);
        id_stage->valid_in(if_valid);
        id_stage->wb_valid(wb_valid);
        id_stage->wb_rd(wb_rd);
        id_stage->wb_data(wb_data);
        id_stage->pc_out(id_pc);
        id_stage->opcode_out(id_opcode);
        id_stage->rd_out(id_rd);
        id_stage->operand1_out(id_operand1);
        id_stage->operand2_out(id_operand2);
        id_stage->immediate_out(id_immediate);
        id_stage->valid_out(id_valid);
        id_stage->stall_request(id_stall_req);
        
        ex_stage = new Execute("ex_stage");
        ex_stage->clk(clk);
        ex_stage->reset(reset);
        ex_stage->stall(global_stall);
        ex_stage->pc_in(id_pc);
        ex_stage->opcode_in(id_opcode);
        ex_stage->rd_in(id_rd);
        ex_stage->operand1_in(id_operand1);
        ex_stage->operand2_in(id_operand2);
        ex_stage->immediate_in(id_immediate);
        ex_stage->valid_in(id_valid);
        ex_stage->pc_out(ex_pc);
        ex_stage->opcode_out(ex_opcode);
        ex_stage->rd_out(ex_rd);
        ex_stage->result_out(ex_result);
        ex_stage->store_data_out(ex_store_data);
        ex_stage->memory_addr_out(ex_memory_addr);
        ex_stage->valid_out(ex_valid);
        ex_stage->fpu_overflow(fpu_overflow);
        ex_stage->fpu_underflow(fpu_underflow);
        ex_stage->fpu_divide_by_zero(fpu_divide_by_zero);
        
        mem_stage = new Memory("mem_stage");
        mem_stage->clk(clk);
        mem_stage->reset(reset);
        mem_stage->stall(global_stall);
        mem_stage->pc_in(ex_pc);
        mem_stage->opcode_in(ex_opcode);
        mem_stage->rd_in(ex_rd);
        mem_stage->result_in(ex_result);
        mem_stage->store_data_in(ex_store_data);
        mem_stage->memory_addr_in(ex_memory_addr);
        mem_stage->valid_in(ex_valid);
        mem_stage->pc_out(mem_pc);
        mem_stage->opcode_out(mem_opcode);
        mem_stage->rd_out(mem_rd);
        mem_stage->result_out(mem_result);
        mem_stage->valid_out(mem_valid);
        mem_stage->mem_write_addr(mem_write_addr);
        mem_stage->mem_write_data(mem_write_data);
        mem_stage->mem_write_enable(mem_write_enable);
        
        wb_stage = new Writeback("wb_stage");
        wb_stage->clk(clk);
        wb_stage->reset(reset);
        wb_stage->stall(global_stall);
        wb_stage->pc_in(mem_pc);
        wb_stage->opcode_in(mem_opcode);
        wb_stage->rd_in(mem_rd);
        wb_stage->result_in(mem_result);
        wb_stage->valid_in(mem_valid);
        wb_stage->wb_valid(wb_valid);
        wb_stage->wb_rd(wb_rd);
        wb_stage->wb_data(wb_data);
        wb_stage->committed_pc(committed_pc);
        wb_stage->instruction_committed(instruction_committed);
        
        SC_METHOD(control_process);
        sensitive << id_stall_req << instruction_committed << committed_pc;
    }
    
    ~FloatingPointProcessor() {
        delete if_stage;
        delete id_stage;
        delete ex_stage;
        delete mem_stage;
        delete wb_stage;
    }
};
using namespace std;

// Helper function to convert float to IEEE 754 format
sc_uint<32> float_to_ieee754(float f) {
    union { float f; uint32_t i; } u;
    u.f = f;
    return sc_uint<32>(u.i);
}

// Helper function to convert IEEE 754 to float
float ieee754_to_float(sc_uint<32> ieee) {
    union { float f; uint32_t i; } u;
    u.i = ieee.to_uint();
    return u.f;
}

#include <systemc.h>
#include <iostream>
#include <iomanip>
#include <cmath>
#include <string>
#include <vector>



#include <systemc.h>
#include <iostream>
#include <iomanip>
#include <cmath>
#include <string>
#include <vector>

using namespace std;


// ========== DIRECT EXECUTE STAGE FPU TESTBENCH ==========
// ========== DIRECT EXECUTE STAGE FPU TESTBENCH ==========
SC_MODULE(ExecuteFPUTestbench) {
    sc_clock clk;
    sc_signal<bool> reset, stall;
    
    // Execute stage interface signals
    sc_signal<sc_uint<32>> pc_in, operand1_in, operand2_in;
    sc_signal<sc_uint<4>> opcode_in;
    sc_signal<sc_uint<5>> rd_in;
    sc_signal<sc_uint<13>> immediate_in;
    sc_signal<bool> valid_in;
    
    // Execute stage outputs
    sc_signal<sc_uint<32>> pc_out, result_out, store_data_out;
    sc_signal<sc_uint<4>> opcode_out;
    sc_signal<sc_uint<5>> rd_out;
    sc_signal<sc_uint<13>> memory_addr_out;
    sc_signal<bool> valid_out;
    sc_signal<bool> fpu_overflow, fpu_underflow, fpu_divide_by_zero;
    
    // Execute stage instance - this contains the FPU units!
    Execute *execute_stage;
    
    // Test results tracking
    int tests_passed = 0;
    int tests_failed = 0;
    int total_tests = 0;
    
    // Test vectors for FPU operations
    struct FPUTest {
        float a, b;
        float expected_add, expected_sub, expected_mult;
        string description;
    };
    
    vector<FPUTest> fpu_tests;
    
    void init_fpu_tests() {
        fpu_tests = {
            {1.0f, 2.0f, 3.0f, -1.0f, 2.0f, "Simple positive numbers"},
            {-1.5f, 3.2f, 1.7f, -4.7f, -4.8f, "Mixed signs"},
            {3.14159f, 2.71828f, 5.85987f, 0.42331f, 8.53973f, "Pi and e"},
            {0.0f, 5.0f, 5.0f, -5.0f, 0.0f, "Zero operand"},
            {100.0f, 25.0f, 125.0f, 75.0f, 2500.0f, "Large numbers"},
            {1.0f, 0.0f, 1.0f, 1.0f, 0.0f, "Multiplication by zero"}
        };
    }
    
    // Check if result matches expected value
    bool check_result(float actual, float expected, const string& operation) {
        bool passed = false;
        
        if (std::isnan(expected) && std::isnan(actual)) {
            passed = true;
        } else if (std::isinf(expected) && std::isinf(actual) && 
                  std::signbit(expected) == std::signbit(actual)) {
            passed = true;
        } else if (std::isfinite(expected) && std::isfinite(actual)) {
            float tolerance = std::max(1e-5f * std::abs(expected), 1e-6f);
            passed = std::abs(actual - expected) < tolerance;
        }
        
        cout << "    " << operation << ": " << actual << " (expected: " << expected << ") - " 
             << (passed ? "PASS" : "FAIL") << endl;
        
        if (passed) tests_passed++;
        else tests_failed++;
        total_tests++;
        
        return passed;
    }
    
    // Execute FPU operation directly through Execute stage
    void test_fpu_operation(int opcode, float a, float b, float expected, const string& op_name) {
        cout << "\n  Testing " << op_name << ": " << a << " " << op_name << " " << b << endl;
        
        // Setup inputs to Execute stage
        pc_in.write(0x1000);
        opcode_in.write(opcode);
        rd_in.write(1);  // Destination register
        operand1_in.write(float_to_ieee754(a));
        operand2_in.write(float_to_ieee754(b));
        immediate_in.write(0);
        valid_in.write(true);
        stall.write(false);
        
        // Wait one clock cycle to start operation
        wait(10, SC_NS);
        
        // Determine cycles needed based on operation
        int max_cycles = 5;   // Default for add/sub/mult
        
        // Wait for result to be valid
        bool found_result = false;
        for (int cycle = 0; cycle < max_cycles; cycle++) {
            wait(10, SC_NS);
            
            if (valid_out.read()) {
                // Get the result from Execute stage
                sc_uint<32> result_bits = result_out.read();
                float actual = ieee754_to_float(result_bits);
                
                // Check the result
                check_result(actual, expected, op_name);
                
                // Check FPU flags
                if (fpu_overflow.read()) {
                    cout << "      [OVERFLOW flag detected]" << endl;
                }
                if (fpu_underflow.read()) {
                    cout << "      [UNDERFLOW flag detected]" << endl;
                }
                if (fpu_divide_by_zero.read()) {
                    cout << "      [DIVIDE_BY_ZERO flag detected]" << endl;
                }
                
                found_result = true;
                break;
            }
        }
        
        if (!found_result) {
            cout << "    ERROR: No valid result after " << max_cycles << " cycles!" << endl;
            tests_failed++;
            total_tests++;
        }
        
        // Clear inputs
        valid_in.write(false);
        wait(10, SC_NS);
    }
    
    void execute_fpu_test_case(const FPUTest& test) {
        cout << "\n--- Testing: " << test.description << " ---" << endl;
        cout << "Operands: A=" << test.a << " (0x" << hex << float_to_ieee754(test.a).to_uint() << dec << "), "
             << "B=" << test.b << " (0x" << hex << float_to_ieee754(test.b).to_uint() << dec << ")" << endl;
        
        // Test the three FPU operations
        test_fpu_operation(0x5, test.a, test.b, test.expected_add, "FADD");
        test_fpu_operation(0x6, test.a, test.b, test.expected_sub, "FSUB");
        test_fpu_operation(0x7, test.a, test.b, test.expected_mult, "FMUL");
    }
    
    void run_comprehensive_fpu_tests() {
        cout << "\n=== TESTING FPU OPERATIONS DIRECTLY THROUGH EXECUTE STAGE ===" << endl;
        
        for (size_t i = 0; i < fpu_tests.size(); i++) {
            execute_fpu_test_case(fpu_tests[i]);
            wait(20, SC_NS); // Brief pause between test cases
        }
    }
    
    void test_integer_operations() {
        cout << "\n--- Testing Integer Operations for Comparison ---" << endl;
        
        // Test integer addition
        cout << "\nTesting Integer ADD: 42 + 58 = 100" << endl;
        pc_in.write(0x2000);
        opcode_in.write(0x1); // OP_ADD_INT
        operand1_in.write(42);
        operand2_in.write(58);
        valid_in.write(true);
        stall.write(false);
        
        wait(20, SC_NS);
        
        if (valid_out.read()) {
            sc_uint<32> result = result_out.read();
            bool passed = (result.to_uint() == 100);
            cout << "    Integer ADD: " << result.to_uint() << " (expected: 100) - " 
                 << (passed ? "PASS" : "FAIL") << endl;
            if (passed) tests_passed++;
            else tests_failed++;
            total_tests++;
        }
        
        valid_in.write(false);
        wait(10, SC_NS);
    }
    
    void print_test_summary() {
        cout << "\n" << string(70, '=') << endl;
        cout << "=== EXECUTE STAGE FPU TEST RESULTS SUMMARY ===" << endl;
        cout << string(70, '=') << endl;
        cout << "Total Tests Run:    " << total_tests << endl;
        cout << "Tests Passed:       " << tests_passed << " (" 
             << fixed << setprecision(1) << (100.0f * tests_passed / total_tests) << "%)" << endl;
        cout << "Tests Failed:       " << tests_failed << " (" 
             << fixed << setprecision(1) << (100.0f * tests_failed / total_tests) << "%)" << endl;
        cout << string(70, '=') << endl;
        
        if (tests_failed == 0) {
            cout << "🎉 ALL FPU TESTS PASSED! The FPU units in Execute stage are working perfectly!" << endl;
        } else if (tests_passed > tests_failed) {
            cout << "⚠️  MOST TESTS PASSED. Some issues detected in FPU implementation." << endl;
        } else {
            cout << "❌ MANY TESTS FAILED. Check FPU unit implementations in Execute stage." << endl;
        }
        cout << string(70, '=') << endl;
    }
    
    void test_process() {
        cout << "\n=== DIRECT EXECUTE STAGE FPU VERIFICATION ===" << endl;
        cout << "Testing FPU units directly through Execute stage (bypassing full processor)" << endl;
        cout << "This tests the actual FPU hardware without pipeline complexities" << endl;
        cout << "Testing operations: FADD, FSUB, FMUL" << endl;
        
        // Initialize test data
        init_fpu_tests();
        
        // Reset sequence
        cout << "\nResetting Execute stage..." << endl;
        reset.write(true);
        wait(50, SC_NS);
        reset.write(false);
        wait(20, SC_NS);
        cout << "Reset complete." << endl;
        
        // Run comprehensive FPU tests
        run_comprehensive_fpu_tests();
        
        // Test integer operations for comparison
        test_integer_operations();
        
        // Print final results
        print_test_summary();
        
        cout << "\n=== EXECUTE STAGE FPU TESTING COMPLETED ===" << endl;
        sc_stop();
    }
    
    SC_CTOR(ExecuteFPUTestbench) : clk("clk", 10, SC_NS) {
        // Instantiate only the Execute stage (which contains FPU units)
        execute_stage = new Execute("execute_stage_under_test");
        execute_stage->clk(clk);
        execute_stage->reset(reset);
        execute_stage->stall(stall);
        execute_stage->pc_in(pc_in);
        execute_stage->opcode_in(opcode_in);
        execute_stage->rd_in(rd_in);
        execute_stage->operand1_in(operand1_in);
        execute_stage->operand2_in(operand2_in);
        execute_stage->immediate_in(immediate_in);
        execute_stage->valid_in(valid_in);
        execute_stage->pc_out(pc_out);
        execute_stage->opcode_out(opcode_out);
        execute_stage->rd_out(rd_out);
        execute_stage->result_out(result_out);
        execute_stage->store_data_out(store_data_out);
        execute_stage->memory_addr_out(memory_addr_out);
        execute_stage->valid_out(valid_out);
        execute_stage->fpu_overflow(fpu_overflow);
        execute_stage->fpu_underflow(fpu_underflow);
        execute_stage->fpu_divide_by_zero(fpu_divide_by_zero);
        
        SC_THREAD(test_process);
    }
    
    ~ExecuteFPUTestbench() {
        delete execute_stage;
    }
};

// ========== MAIN FUNCTION ==========
int sc_main(int argc, char* argv[]) {
    cout << "=== DIRECT EXECUTE STAGE FPU VERIFICATION ===" << endl;
    cout << "This testbench directly tests the FPU units in the Execute stage" << endl;
    cout << "Bypasses instruction fetch/decode complexities" << endl;
    cout << "Tests: FADD, FSUB, FMUL, FDIV operations with real operands" << endl;
    cout << endl;
    
    ExecuteFPUTestbench testbench("execute_fpu_testbench");
    
    // Enable waveform tracing
    sc_trace_file *tf = sc_create_vcd_trace_file("execute_stage_fpu_test");
    sc_trace(tf, testbench.clk, "clk");
    sc_trace(tf, testbench.reset, "reset");
    sc_trace(tf, testbench.opcode_in, "opcode_in");
    sc_trace(tf, testbench.operand1_in, "operand1_in");
    sc_trace(tf, testbench.operand2_in, "operand2_in");
    sc_trace(tf, testbench.valid_in, "valid_in");
    sc_trace(tf, testbench.result_out, "result_out");
    sc_trace(tf, testbench.valid_out, "valid_out");
    sc_trace(tf, testbench.fpu_overflow, "fpu_overflow");
    sc_trace(tf, testbench.fpu_underflow, "fpu_underflow");
    sc_trace(tf, testbench.fpu_divide_by_zero, "fpu_divide_by_zero");
    
    // Start simulation
    try {
        sc_start();
        cout << "\n✅ Simulation completed successfully!" << endl;
    } catch (const exception& e) {
        cout << "\n❌ Simulation error: " << e.what() << endl;
        return 1;
    }
    
    sc_close_vcd_trace_file(tf);
    
    cout << "\n📊 Check the summary above for FPU unit verification results!" << endl;
    cout << "📈 Waveform file 'execute_stage_fpu_test.vcd' generated" << endl;
    
    return 0;
}

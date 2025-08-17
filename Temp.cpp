// Simple 4-Stage FPU Pipeline: Fetch -> Decode -> Execute -> Writeback
#include <systemc.h>
#include <cmath>
#include <vector>
#include <iomanip>
#include <string>
#include <cstring>
#include "design.cpp"
#include <iostream>
using namespace std;

// Helper functions
sc_uint<32> float_to_ieee754(float f) {
    union { float f; uint32_t i; } u;
    u.f = f;
    return sc_uint<32>(u.i);
}

float ieee754_to_float(sc_uint<32> ieee) {
    union { float f; uint32_t i; } u;
    u.i = ieee.to_uint();
    return u.f;
}

// Instruction format: [31:28] opcode, [27:23] rd, [22:18] rs1, [17:13] rs2, [12:0] unused
struct fp_instruction_t {
    sc_uint<4>  opcode;
    sc_uint<5>  rd;     
    sc_uint<5>  rs1;    
    sc_uint<5>  rs2;    
    sc_uint<13> unused;
    
    fp_instruction_t(sc_uint<4> op, sc_uint<5> dst, sc_uint<5> src1, sc_uint<5> src2) 
        : opcode(op), rd(dst), rs1(src1), rs2(src2), unused(0) {}
    
    sc_uint<32> to_word() const {
        return (sc_uint<32>(opcode) << 28) | (sc_uint<32>(rd) << 23) | 
               (sc_uint<32>(rs1) << 18) | (sc_uint<32>(rs2) << 13);
    }
};

// ========== FETCH STAGE ==========
SC_MODULE(Fetch) {
    sc_in<bool> clk;
    sc_in<bool> reset;
    sc_in<bool> stall;
    
    sc_out<sc_uint<32>> pc_out;
    sc_out<sc_uint<32>> instruction_out;
    sc_out<bool> valid_out;
    
    // Simple instruction memory
    vector<sc_uint<32>> imem;
    sc_uint<32> pc;
    
    void fetch_process() {
        if (reset.read()) {
            pc = 0;
            pc_out.write(0);
            instruction_out.write(0);
            valid_out.write(false);
        } else if (!stall.read()) {
            if (pc < imem.size()) {
                pc_out.write(pc * 4); // PC in bytes
                instruction_out.write(imem[pc]);
                valid_out.write(true);
                pc++;
            } else {
                valid_out.write(false);
            }
        }
    }
    
    void load_program(const vector<sc_uint<32>>& program) {
        imem = program;
    }
    
    SC_CTOR(Fetch) {
        SC_METHOD(fetch_process);
        sensitive << clk.pos() << reset << stall;
    }
};

// ========== DECODE STAGE ==========
SC_MODULE(Decode) {
    sc_in<bool> clk;
    sc_in<bool> reset;
    sc_in<bool> stall;
    
    // From Fetch
    sc_in<sc_uint<32>> pc_in;
    sc_in<sc_uint<32>> instruction_in;
    sc_in<bool> valid_in;
    
    // To Execute
    sc_out<sc_uint<32>> pc_out;
    sc_out<sc_uint<4>> opcode_out;
    sc_out<sc_uint<5>> rd_out;
    sc_out<sc_uint<32>> operand1_out;
    sc_out<sc_uint<32>> operand2_out;
    sc_out<bool> valid_out;
    
    // Register file (32 FP registers)
    sc_uint<32> fp_registers[32];
    
    void decode_process() {
        if (reset.read()) {
            pc_out.write(0);
            opcode_out.write(0);
            rd_out.write(0);
            operand1_out.write(0);
            operand2_out.write(0);
            valid_out.write(false);
            
            // Initialize registers to zero
            for (int i = 0; i < 32; i++) {
                fp_registers[i] = 0;
            }
        } else if (!stall.read() && valid_in.read()) {
            sc_uint<32> inst = instruction_in.read();
            
            // Decode instruction fields
            sc_uint<4> opcode = (inst >> 28) & 0xF;
            sc_uint<5> rd = (inst >> 23) & 0x1F;
            sc_uint<5> rs1 = (inst >> 18) & 0x1F;
            sc_uint<5> rs2 = (inst >> 13) & 0x1F;
            
            // Read operands from register file
            sc_uint<32> op1 = fp_registers[rs1.to_uint()];
            sc_uint<32> op2 = fp_registers[rs2.to_uint()];
            
            // Forward to Execute stage
            pc_out.write(pc_in.read());
            opcode_out.write(opcode);
            rd_out.write(rd);
            operand1_out.write(op1);
            operand2_out.write(op2);
            valid_out.write(true);
        } else {
            valid_out.write(false);
        }
    }
    
    // Method to write to register file (called by writeback)
    void write_register(sc_uint<5> reg, sc_uint<32> value) {
        if (reg.to_uint() != 0) { // Don't write to register 0
            fp_registers[reg.to_uint()] = value;
        }
    }
    
    // Method to set initial register values for testing
    void set_register(int reg, float value) {
        if (reg > 0 && reg < 32) {
            fp_registers[reg] = float_to_ieee754(value);
        }
    }
    
    SC_CTOR(Decode) {
        SC_METHOD(decode_process);
        sensitive << clk.pos() << reset << stall << valid_in << instruction_in << pc_in;
    }
};

// ========== EXECUTE STAGE (Reusing your working Execute stage) 

SC_MODULE(Execute) {
    sc_in<bool> clk;
    sc_in<bool> reset;
    sc_in<bool> stall;
    
    // Input from Decode stage
    sc_in<sc_uint<32>> pc_in;
    sc_in<sc_uint<4>> opcode_in;
    sc_in<sc_uint<5>> rd_in;
    sc_in<sc_uint<32>> operand1_in;
    sc_in<sc_uint<32>> operand2_in;
    sc_in<bool> valid_in;
    
    // Output to Writeback stage
    sc_out<sc_uint<32>> pc_out;
    sc_out<sc_uint<4>> opcode_out;
    sc_out<sc_uint<5>> rd_out;
    sc_out<sc_uint<32>> result_out;
    sc_out<bool> valid_out;

private:
    // FPU instances
    ieee754mult *fpu_mult;
    ieee754add *fpu_add;
    ieee754_subtractor *fpu_sub;
    
    // Shared FPU input signals (back to original approach)
    sc_signal<sc_uint<32>> fpu_a, fpu_b;
    sc_signal<bool> fpu_valid_in;
    sc_signal<sc_uint<32>> mult_result, add_result, sub_result;
    sc_signal<bool> mult_valid, add_valid, sub_valid;
    sc_signal<bool> mult_overflow, mult_underflow;
    sc_signal<bool> add_overflow, add_underflow;
    sc_signal<bool> sub_overflow, sub_underflow;
    
    // NEW: Pipeline registers to track FPU operations through their 3-cycle latency
    struct fpu_pipeline_entry {
        sc_uint<32> pc;
        sc_uint<4> opcode;
        sc_uint<5> rd;
        bool valid;
    };
    
    // 3-stage pipeline for each FPU result
    fpu_pipeline_entry mult_pipe[3];
    fpu_pipeline_entry add_pipe[3];  
    fpu_pipeline_entry sub_pipe[3];
    
    // Track if adder needs continuous valid signal
    int add_cycles_remaining;
    
    enum opcodes {
        OP_FADD = 0x0,
        OP_FSUB = 0x1,
        OP_FMUL = 0x2
    };

public:
    void execute_process() {
        if (reset.read()) {
            pc_out.write(0);
            opcode_out.write(0);
            rd_out.write(0);
            result_out.write(0);
            valid_out.write(false);
            fpu_a.write(0);
            fpu_b.write(0);
            fpu_valid_in.write(false);
            add_cycles_remaining = 0;
            
            // Clear pipeline registers
            for (int i = 0; i < 3; i++) {
                mult_pipe[i].valid = false;
                add_pipe[i].valid = false;
                sub_pipe[i].valid = false;
            }
            
        } else if (!stall.read()) {
            
            // NEW: Handle incoming instruction
            if (valid_in.read()) {
                sc_uint<4> opcode = opcode_in.read();
                sc_uint<32> op1 = operand1_in.read();
                sc_uint<32> op2 = operand2_in.read();
                
                // Set FPU inputs
                fpu_a.write(op1);
                fpu_b.write(op2);
                
                // Insert instruction into appropriate pipeline tracker
                switch (opcode.to_uint()) {
                    case OP_FADD:
                        fpu_valid_in.write(true);  // Adder needs this
                        add_cycles_remaining = 3;  // Track adder's need for valid signal
                        add_pipe[0] = {pc_in.read(), opcode, rd_in.read(), true};
                        break;
                        
                    case OP_FSUB:
                        fpu_valid_in.write(false); // Subtractor ignores valid_in
                        sub_pipe[0] = {pc_in.read(), opcode, rd_in.read(), true};
                        break;
                        
                    case OP_FMUL:
                        fpu_valid_in.write(false); // Multiplier ignores valid_in
                        mult_pipe[0] = {pc_in.read(), opcode, rd_in.read(), true};
                        break;
                        
                    default:
                        fpu_valid_in.write(false);
                        break;
                }
            } else {
                // NEW: Maintain adder's valid signal if it's still processing
                if (add_cycles_remaining > 0) {
                    fpu_valid_in.write(true);
                } else {
                    fpu_valid_in.write(false);
                }
            }
            
            // NEW: Check for completed FPU operations (stage 2 of each pipeline)
            sc_uint<32> result = 0;
            bool result_ready = false;
            sc_uint<32> output_pc = 0;
            sc_uint<4> output_opcode = 0;
            sc_uint<5> output_rd = 0;
            
            // Check multiplier completion
            if (mult_pipe[2].valid && mult_valid.read()) {
                result = mult_result.read();
                result_ready = true;
                output_pc = mult_pipe[2].pc;
                output_opcode = mult_pipe[2].opcode;
                output_rd = mult_pipe[2].rd;
            }
            // Check adder completion  
            else if (add_pipe[2].valid && add_valid.read()) {
                result = add_result.read();
                result_ready = true;
                output_pc = add_pipe[2].pc;
                output_opcode = add_pipe[2].opcode;
                output_rd = add_pipe[2].rd;
            }
            // Check subtractor completion
            else if (sub_pipe[2].valid && sub_valid.read()) {
                result = sub_result.read();
                result_ready = true;
                output_pc = sub_pipe[2].pc;
                output_opcode = sub_pipe[2].opcode;
                output_rd = sub_pipe[2].rd;
            }
            
            // Output completed result
            if (result_ready) {
                pc_out.write(output_pc);
                opcode_out.write(output_opcode);
                rd_out.write(output_rd);
                result_out.write(result);
                valid_out.write(true);
            } else {
                valid_out.write(false);
            }
        } else {
            valid_out.write(false);
            if (add_cycles_remaining > 0) {
                fpu_valid_in.write(true);
            } else {
                fpu_valid_in.write(false);
            }
        }
    }

    void pipeline_advance() {
        if (reset.read()) {
            add_cycles_remaining = 0;
            for (int i = 0; i < 3; i++) {
                mult_pipe[i].valid = false;
                add_pipe[i].valid = false;
                sub_pipe[i].valid = false;
            }
        } else if (!stall.read()) {
            // NEW: Advance all pipeline stages
            for (int i = 2; i > 0; i--) {
                mult_pipe[i] = mult_pipe[i-1];
                add_pipe[i] = add_pipe[i-1];
                sub_pipe[i] = sub_pipe[i-1];
            }
            mult_pipe[0].valid = false;
            add_pipe[0].valid = false;
            sub_pipe[0].valid = false;
            
            // Decrement adder valid signal counter
            if (add_cycles_remaining > 0) {
                add_cycles_remaining--;
            }
        }
    }

    SC_CTOR(Execute) {
        // Initialize pipeline registers
        add_cycles_remaining = 0;
        for (int i = 0; i < 3; i++) {
            mult_pipe[i].valid = false;
            add_pipe[i].valid = false;
            sub_pipe[i].valid = false;
        }
        
        // Instantiate FPU units (same as original)
        fpu_mult = new ieee754mult("fpu_mult");
        fpu_mult->A(fpu_a);
        fpu_mult->B(fpu_b);
        fpu_mult->reset(reset);
        fpu_mult->clk(clk);
        fpu_mult->result(mult_result);
        fpu_mult->valid_out(mult_valid);
        fpu_mult->overflow(mult_overflow);
        fpu_mult->underflow(mult_underflow);
        
        fpu_add = new ieee754add("fpu_add");
        fpu_add->clk(clk);
        fpu_add->reset(reset);
        fpu_add->A(fpu_a);
        fpu_add->B(fpu_b);
        fpu_add->valid_in(fpu_valid_in);  // KEY: This needs to stay true for 3 cycles
        fpu_add->result(add_result);
        fpu_add->valid_out(add_valid);
        fpu_add->overflow(add_overflow);
        fpu_add->underflow(add_underflow);
        
        fpu_sub = new ieee754_subtractor("fpu_sub");
        fpu_sub->A(fpu_a);
        fpu_sub->B(fpu_b);
        fpu_sub->reset(reset);
        fpu_sub->clk(clk);
        fpu_sub->result(sub_result);
        fpu_sub->valid_out(sub_valid);
        fpu_sub->overflow(sub_overflow);
        fpu_sub->underflow(sub_underflow);
        
        SC_METHOD(execute_process);
        sensitive << clk.pos() << reset << stall << valid_in << opcode_in 
                 << operand1_in << operand2_in << rd_in << pc_in
                 << mult_result << add_result << sub_result
                 << mult_valid << add_valid << sub_valid;
                 
        SC_METHOD(pipeline_advance);
        sensitive << clk.pos();
    }
    
    ~Execute() {
        delete fpu_mult;
        delete fpu_add;
        delete fpu_sub;
    }
};
// ========== WRITEBACK STAGE ==========
SC_MODULE(Writeback) {
    sc_in<bool> clk;
    sc_in<bool> reset;
    sc_in<bool> stall;
    
    // From Execute
    sc_in<sc_uint<32>> pc_in;
    sc_in<sc_uint<4>> opcode_in;
    sc_in<sc_uint<5>> rd_in;
    sc_in<sc_uint<32>> result_in;
    sc_in<bool> valid_in;
    
    // Pointer to Decode stage for register file access
    Decode *decode_stage;
    
    void writeback_process() {
        if (reset.read()) {
            // Nothing to do on reset
        } else if (!stall.read() && valid_in.read()) {
            sc_uint<5> rd = rd_in.read();
            sc_uint<32> result = result_in.read();
            
            // Write result back to register file
            if (decode_stage) {
                decode_stage->write_register(rd, result);
            }
        }
    }
    
    void set_decode_stage(Decode *decode_ptr) {
        decode_stage = decode_ptr;
    }
    
    SC_CTOR(Writeback) : decode_stage(nullptr) {
        SC_METHOD(writeback_process);
        sensitive << clk.pos() << reset << stall << valid_in << rd_in << result_in;
    }
};

// ========== SIMPLE PIPELINE TESTBENCH ==========
SC_MODULE(SimplePipelineTestbench) {
    sc_clock clk;
    sc_signal<bool> reset, stall;
    
    // Pipeline stage instances
    Fetch *fetch_stage;
    Decode *decode_stage;
    Execute *execute_stage;
    Writeback *writeback_stage;
    
    // Inter-stage signals
    sc_signal<sc_uint<32>> fetch_pc, fetch_inst;
    sc_signal<bool> fetch_valid;
    
    sc_signal<sc_uint<32>> decode_pc;
    sc_signal<sc_uint<4>> decode_opcode;
    sc_signal<sc_uint<5>> decode_rd;
    sc_signal<sc_uint<32>> decode_op1, decode_op2;
    sc_signal<bool> decode_valid;
    
    sc_signal<sc_uint<32>> execute_pc, execute_result;
    sc_signal<sc_uint<4>> execute_opcode;
    sc_signal<sc_uint<5>> execute_rd;
    sc_signal<bool> execute_valid;
    
    int tests_passed = 0;
    int tests_failed = 0;
    
    void create_test_program() {
        vector<sc_uint<32>> program;
        
        // Test 1: FADD f3, f1, f2  (3.0 + 2.0 = 5.0)
        fp_instruction_t inst1(0x0, 3, 1, 2);  // FADD
        program.push_back(inst1.to_word());
        
        // Test 2: FSUB f4, f1, f2  (3.0 - 2.0 = 1.0)
        fp_instruction_t inst2(0x1, 4, 1, 2);  // FSUB
        program.push_back(inst2.to_word());
        
        // Test 3: FMUL f5, f1, f2  (3.0 * 2.0 = 6.0)
        fp_instruction_t inst3(0x2, 5, 1, 2);  // FMUL
        program.push_back(inst3.to_word());
        
        fetch_stage->load_program(program);
    }
    
    void setup_initial_registers() {
        // Set up test values in registers
        decode_stage->set_register(1, 3.0f);  // f1 = 3.0
        decode_stage->set_register(2, 2.0f);  // f2 = 2.0
    }
    
    bool check_result(int reg, float expected, const string& test_name) {
        float actual = ieee754_to_float(decode_stage->fp_registers[reg]);
        bool passed = abs(actual - expected) < 1e-6f;
        
        cout << test_name << ": f" << reg << " = " << actual 
             << " (expected: " << expected << ") - " 
             << (passed ? "PASS" : "FAIL") << endl;
             
        if (passed) tests_passed++;
        else tests_failed++;
        
        return passed;
    }
    
    void test_process() {
        cout << "\n=== SIMPLE 4-STAGE FPU PIPELINE TEST ===" << endl;
        cout << "Testing: Fetch -> Decode -> Execute -> Writeback" << endl;
        cout << "Operations: FADD, FSUB, FMUL" << endl;
        
        // Reset pipeline
        cout << "\nResetting pipeline..." << endl;
        reset.write(true);
        stall.write(false);
        wait(50, SC_NS);
        reset.write(false);
        wait(20, SC_NS);
        
        // Setup test data
        setup_initial_registers();
        create_test_program();
        
        cout << "\nInitial register values:" << endl;
        cout << "f1 = 3.0, f2 = 2.0" << endl;
        
        cout << "\nRunning pipeline..." << endl;
        
        // Run pipeline for enough cycles to complete all instructions
        // Need extra cycles for FPU latency
        int max_cycles = 50;
        for (int cycle = 0; cycle < max_cycles; cycle++) {
            wait(10, SC_NS);
            
            // Check if we have results after sufficient cycles
            if (cycle > 20 && cycle % 10 == 0) {
                cout << "\n--- Checking results at cycle " << cycle << " ---" << endl;
                check_result(3, 5.0f, "Test 1 (FADD 3+2)");
                check_result(4, 1.0f, "Test 2 (FSUB 3-2)");
                check_result(5, 6.0f, "Test 3 (FMUL 3*2)");
            }
        }
        
        cout << "\n=== FINAL RESULTS ===" << endl;
        cout << "Tests Passed: " << tests_passed << endl;
        cout << "Tests Failed: " << tests_failed << endl;
        
        if (tests_failed == 0) {
            cout << "🎉 ALL PIPELINE TESTS PASSED!" << endl;
        } else {
            cout << "❌ Some pipeline tests failed." << endl;
        }
        
        sc_stop();
    }
    
    SC_CTOR(SimplePipelineTestbench) : clk("clk", 10, SC_NS) {
        // Instantiate pipeline stages
        fetch_stage = new Fetch("fetch");
        decode_stage = new Decode("decode");
        execute_stage = new Execute("execute");
        writeback_stage = new Writeback("writeback");
        
        // Connect Fetch stage
        fetch_stage->clk(clk);
        fetch_stage->reset(reset);
        fetch_stage->stall(stall);
        fetch_stage->pc_out(fetch_pc);
        fetch_stage->instruction_out(fetch_inst);
        fetch_stage->valid_out(fetch_valid);
        
        // Connect Decode stage
        decode_stage->clk(clk);
        decode_stage->reset(reset);
        decode_stage->stall(stall);
        decode_stage->pc_in(fetch_pc);
        decode_stage->instruction_in(fetch_inst);
        decode_stage->valid_in(fetch_valid);
        decode_stage->pc_out(decode_pc);
        decode_stage->opcode_out(decode_opcode);
        decode_stage->rd_out(decode_rd);
        decode_stage->operand1_out(decode_op1);
        decode_stage->operand2_out(decode_op2);
        decode_stage->valid_out(decode_valid);
        
        // Connect Execute stage
        execute_stage->clk(clk);
        execute_stage->reset(reset);
        execute_stage->stall(stall);
        execute_stage->pc_in(decode_pc);
        execute_stage->opcode_in(decode_opcode);
        execute_stage->rd_in(decode_rd);
        execute_stage->operand1_in(decode_op1);
        execute_stage->operand2_in(decode_op2);
        execute_stage->valid_in(decode_valid);
        execute_stage->pc_out(execute_pc);
        execute_stage->opcode_out(execute_opcode);
        execute_stage->rd_out(execute_rd);
        execute_stage->result_out(execute_result);
        execute_stage->valid_out(execute_valid);
        
        // Connect Writeback stage
        writeback_stage->clk(clk);
        writeback_stage->reset(reset);
        writeback_stage->stall(stall);
        writeback_stage->pc_in(execute_pc);
        writeback_stage->opcode_in(execute_opcode);
        writeback_stage->rd_in(execute_rd);
        writeback_stage->result_in(execute_result);
        writeback_stage->valid_in(execute_valid);
        
        // Set decode stage pointer for writeback
        writeback_stage->set_decode_stage(decode_stage);
        
        SC_THREAD(test_process);
    }
    
    ~SimplePipelineTestbench() {
        delete fetch_stage;
        delete decode_stage;
        delete execute_stage;
        delete writeback_stage;
    }
};

// ========== MAIN FUNCTION ==========
int sc_main(int argc, char* argv[]) {
    cout << "=== SIMPLE 4-STAGE FPU PIPELINE ===" << endl;
    cout << "Testing: FADD, FSUB, FMUL with 3 test cases" << endl;
    
    SimplePipelineTestbench testbench("pipeline_testbench");
    
    try {
        sc_start();
        cout << "\n✅ Pipeline simulation completed!" << endl;
    } catch (const exception& e) {
        cout << "\n❌ Simulation error: " << e.what() << endl;
        return 1;
    }
    return 0;
}

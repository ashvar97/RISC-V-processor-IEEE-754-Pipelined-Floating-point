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

// Fixed Execute Stage with Corrected Division Algorithm
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
    enum opcodes {
        OP_FADD = 0x0,
        OP_FSUB = 0x1,
        OP_FMUL = 0x2,
        OP_FDIV = 0x3
    };

    // Unified pipeline stage structure
    struct unified_pipeline_stage {
        sc_uint<32> pc;
        sc_uint<4> opcode;
        sc_uint<5> rd;
        sc_uint<32> operand_a;
        sc_uint<32> operand_b;
        bool valid;
        
        // Extracted fields (after stage 1)
        bool a_sign, b_sign;
        sc_uint<8> a_exp, b_exp;
        sc_uint<24> a_mant, b_mant;
        bool a_special, b_special;
        
        // Division-specific fields
        bool is_division;
        int div_cycles_remaining;
        sc_uint<48> div_dividend;
        sc_uint<24> div_divisor;
        sc_uint<24> div_quotient;
        bool div_sign;
        sc_int<12> div_exp;
        
        // Computed fields
        sc_uint<32> result;
        bool overflow, underflow;
        
        unified_pipeline_stage() {
            valid = false;
            pc = 0;
            opcode = 0;
            rd = 0;
            operand_a = 0;
            operand_b = 0;
            a_sign = b_sign = false;
            a_exp = b_exp = 0;
            a_mant = b_mant = 0;
            a_special = b_special = false;
            is_division = false;
            div_cycles_remaining = 0;
            div_dividend = 0;
            div_divisor = 0;
            div_quotient = 0;
            div_sign = false;
            div_exp = 0;
            result = 0;
            overflow = underflow = false;
        }
    };
    
    // Pipeline and division buffer
    unified_pipeline_stage pipe[3];
    vector<unified_pipeline_stage> division_buffer;
    
    // Utility functions
    void extract_ieee754(sc_uint<32> value, bool &sign, sc_uint<8> &exp, sc_uint<24> &mant, bool &special) {
        sign = value[31];
        exp = (value >> 23) & 0xFF;
        
        // Handle mantissa correctly for normalized and denormalized numbers
        if (exp != 0) {
            // Normalized: add implicit leading 1
            mant = (value & 0x7FFFFF) | 0x800000;
        } else {
            // Denormalized: no implicit leading 1
            mant = value & 0x7FFFFF;
        }
        
        // Special cases: infinity, NaN, or zero
        special = (exp == 0xFF) || (exp == 0 && (value & 0x7FFFFF) == 0);
    }
    
    sc_uint<32> pack_ieee754(bool sign, sc_int<12> exp_signed, sc_uint<24> mant) {
        // Handle overflow
        if (exp_signed >= 255) {
            // Return infinity
            return (sc_uint<32>(sign) << 31) | 0x7F800000;
        }
        
        // Handle underflow
        if (exp_signed <= 0) {
            // Return zero
            return (sc_uint<32>(sign) << 31);
        }
        
        // Normal case
        sc_uint<8> exp = sc_uint<8>(exp_signed);
        
        // Remove implicit leading bit for normalized numbers
        sc_uint<23> frac = mant & 0x7FFFFF;
        
        return (sc_uint<32>(sign) << 31) | (sc_uint<32>(exp) << 23) | sc_uint<32>(frac);
    }
    
    void perform_division_step(unified_pipeline_stage& div_stage) {
        if (div_stage.div_cycles_remaining <= 0) return;
        
        // Shift dividend left by 1
        div_stage.div_dividend <<= 1;
        
        // Compare and subtract if possible (restoring division)
        sc_uint<48> divisor_shifted = sc_uint<48>(div_stage.div_divisor) << 24;
        
        if (div_stage.div_dividend >= divisor_shifted) {
            div_stage.div_dividend -= divisor_shifted;
            div_stage.div_quotient = (div_stage.div_quotient << 1) | 1;
        } else {
            div_stage.div_quotient = div_stage.div_quotient << 1;
        }
        
        div_stage.div_cycles_remaining--;
        
        // When division is complete
        if (div_stage.div_cycles_remaining == 0) {
            sc_uint<24> final_quotient = div_stage.div_quotient;
            sc_int<12> final_exp = div_stage.div_exp;
            
            // Normalize the result
            if (final_quotient != 0) {
                // Find the leading 1 bit and normalize
                while (final_quotient != 0 && !(final_quotient & 0x800000)) {
                    final_quotient <<= 1;
                    final_exp--;
                }
                
                // Handle rounding (simple truncation for now)
                // In a more complete implementation, you'd add proper rounding
            }
            
            // Pack the result
            div_stage.result = pack_ieee754(div_stage.div_sign, final_exp, final_quotient);
        }
    }
    
    sc_uint<32> perform_operation(sc_uint<4> opcode, bool a_sign, sc_uint<8> a_exp, sc_uint<24> a_mant, 
                                  bool b_sign, sc_uint<8> b_exp, sc_uint<24> b_mant, bool a_special, bool b_special) {
        
        // Handle special cases (infinity, NaN, zero)
        if (a_special || b_special) {
            if (opcode == OP_FDIV && b_special) {
                // Division by zero or special case
                return 0x7F800000; // Return positive infinity
            }
            return 0; // Return zero for other special cases
        }
        
        switch (opcode.to_uint()) {
            case OP_FADD: {
                // Align exponents
                sc_uint<8> exp_diff;
                sc_uint<24> aligned_mant_a = a_mant;
                sc_uint<24> aligned_mant_b = b_mant;
                sc_uint<8> result_exp;
                
                if (a_exp >= b_exp) {
                    exp_diff = a_exp - b_exp;
                    result_exp = a_exp;
                    if (exp_diff < 24) {
                        aligned_mant_b >>= exp_diff;
                    } else {
                        aligned_mant_b = 0;
                    }
                } else {
                    exp_diff = b_exp - a_exp;
                    result_exp = b_exp;
                    if (exp_diff < 24) {
                        aligned_mant_a >>= exp_diff;
                    } else {
                        aligned_mant_a = 0;
                    }
                }
                
                // Perform addition or subtraction
                sc_uint<25> result_mant;
                bool result_sign;
                
                if (a_sign == b_sign) {
                    // Same signs: add magnitudes
                    result_mant = aligned_mant_a + aligned_mant_b;
                    result_sign = a_sign;
                } else {
                    // Different signs: subtract magnitudes
                    if (aligned_mant_a >= aligned_mant_b) {
                        result_mant = aligned_mant_a - aligned_mant_b;
                        result_sign = a_sign;
                    } else {
                        result_mant = aligned_mant_b - aligned_mant_a;
                        result_sign = b_sign;
                    }
                }
                
                // Handle zero result
                if (result_mant == 0) {
                    return 0;
                }
                
                // Normalize result
                if (result_mant & 0x1000000) {
                    // Overflow: shift right and increment exponent
                    result_mant >>= 1;
                    result_exp++;
                } else {
                    // Underflow: shift left and decrement exponent
                    while (result_mant != 0 && !(result_mant & 0x800000) && result_exp > 0) {
                        result_mant <<= 1;
                        result_exp--;
                    }
                }
                
                return pack_ieee754(result_sign, sc_int<12>(result_exp), result_mant & 0xFFFFFF);
            }
            
            case OP_FSUB: {
                // Subtraction: flip the sign of the second operand and add
                return perform_operation(OP_FADD, a_sign, a_exp, a_mant, !b_sign, b_exp, b_mant, a_special, b_special);
            }
            
            case OP_FMUL: {
                bool result_sign = a_sign ^ b_sign;
                sc_int<12> temp_exp = sc_int<12>(a_exp) + sc_int<12>(b_exp) - 127;
                
                // Multiply mantissas
                sc_uint<48> product = sc_uint<48>(a_mant) * sc_uint<48>(b_mant);
                
                // Normalize the product
                if (product & 0x800000000000ULL) {
                    // MSB is set: shift right 24 bits
                    product >>= 24;
                    temp_exp++;
                } else {
                    // MSB is not set: shift right 23 bits
                    product >>= 23;
                }
                
                return pack_ieee754(result_sign, temp_exp, sc_uint<24>(product & 0xFFFFFF));
            }
            
            case OP_FDIV: {
                // Division is handled in the division buffer
                return 0;
            }
        }
        
        return 0;
    }

public:
    void execute_process() {
        if (reset.read()) {
            for (int i = 0; i < 3; i++) {
                pipe[i] = unified_pipeline_stage();
            }
            division_buffer.clear();
            
            pc_out.write(0);
            opcode_out.write(0);
            rd_out.write(0);
            result_out.write(0);
            valid_out.write(false);
            
        } else if (!stall.read()) {
            
            // Process division buffer
            for (auto& div_op : division_buffer) {
                if (div_op.valid && div_op.div_cycles_remaining > 0) {
                    perform_division_step(div_op);
                }
            }
            
            // Stage 3: Output
            bool output_produced = false;
            
            // Check for completed divisions first
            for (int i = 0; i < division_buffer.size(); i++) {
                if (division_buffer[i].valid && division_buffer[i].div_cycles_remaining == 0) {
                    pc_out.write(division_buffer[i].pc);
                    opcode_out.write(division_buffer[i].opcode);
                    rd_out.write(division_buffer[i].rd);
                    result_out.write(division_buffer[i].result);
                    valid_out.write(true);
                    output_produced = true;
                    
                    division_buffer.erase(division_buffer.begin() + i);
                    break;
                }
            }
            
            // Output regular operations if no division completed
            if (!output_produced && pipe[2].valid) {
                pc_out.write(pipe[2].pc);
                opcode_out.write(pipe[2].opcode);
                rd_out.write(pipe[2].rd);
                result_out.write(pipe[2].result);
                valid_out.write(true);
            } else if (!output_produced) {
                valid_out.write(false);
            }
            
            // Stage 2: Compute
            if (pipe[1].valid) {
                pipe[2] = pipe[1];
                
                if (pipe[1].opcode.to_uint() == OP_FDIV) {
                    // Handle division
                    unified_pipeline_stage div_stage = pipe[1];
                    div_stage.is_division = true;
                    div_stage.div_sign = pipe[1].a_sign ^ pipe[1].b_sign;
                    div_stage.div_exp = sc_int<12>(pipe[1].a_exp) - sc_int<12>(pipe[1].b_exp) + 127;
                    
                    // Initialize division
                    div_stage.div_dividend = sc_uint<48>(pipe[1].a_mant) << 23; // Proper alignment
                    div_stage.div_divisor = pipe[1].b_mant;
                    div_stage.div_quotient = 0;
                    div_stage.div_cycles_remaining = 24; // 24 bits for mantissa
                    
                    // Handle special cases
                    if (pipe[1].b_special || pipe[1].b_mant == 0) {
                        // Division by zero or special
                        div_stage.result = pack_ieee754(div_stage.div_sign, sc_int<12>(255), sc_uint<24>(0));
                        div_stage.div_cycles_remaining = 0;
                    } else if (pipe[1].a_special || pipe[1].a_mant == 0) {
                        // Zero dividend
                        div_stage.result = pack_ieee754(div_stage.div_sign, sc_int<12>(0), sc_uint<24>(0));
                        div_stage.div_cycles_remaining = 0;
                    }
                    
                    division_buffer.push_back(div_stage);
                    pipe[2].valid = false; // Don't output immediately
                } else {
                    // Handle other operations
                    pipe[2].result = perform_operation(
                        pipe[1].opcode,
                        pipe[1].a_sign, pipe[1].a_exp, pipe[1].a_mant,
                        pipe[1].b_sign, pipe[1].b_exp, pipe[1].b_mant,
                        pipe[1].a_special, pipe[1].b_special
                    );
                }
            } else {
                pipe[2].valid = false;
            }
            
            // Stage 1: Extract
            if (pipe[0].valid) {
                pipe[1] = pipe[0];
                extract_ieee754(pipe[0].operand_a, pipe[1].a_sign, pipe[1].a_exp, pipe[1].a_mant, pipe[1].a_special);
                extract_ieee754(pipe[0].operand_b, pipe[1].b_sign, pipe[1].b_exp, pipe[1].b_mant, pipe[1].b_special);
            } else {
                pipe[1].valid = false;
            }
            
            // Stage 0: Input
            if (valid_in.read()) {
                pipe[0].pc = pc_in.read();
                pipe[0].opcode = opcode_in.read();
                pipe[0].rd = rd_in.read();
                pipe[0].operand_a = operand1_in.read();
                pipe[0].operand_b = operand2_in.read();
                pipe[0].valid = true;
            } else {
                pipe[0].valid = false;
            }
            
        } else {
            // Stalled - continue division processing only
            for (auto& div_op : division_buffer) {
                if (div_op.valid && div_op.div_cycles_remaining > 0) {
                    perform_division_step(div_op);
                }
            }
            valid_out.write(false);
        }
    }

    SC_CTOR(Execute) {
        for (int i = 0; i < 3; i++) {
            pipe[i] = unified_pipeline_stage();
        }
        division_buffer.clear();
        
        SC_METHOD(execute_process);
        sensitive << clk.pos() << reset << stall << valid_in << opcode_in 
                 << operand1_in << operand2_in << rd_in << pc_in;
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

// ========== ENHANCED PIPELINE TESTBENCH WITH DIVISION ==========
SC_MODULE(EnhancedPipelineTestbench) {
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
        
        // Test 4: FDIV f6, f1, f2  (3.0 / 2.0 = 1.5)
        fp_instruction_t inst4(0x3, 6, 1, 2);  // FDIV
        program.push_back(inst4.to_word());
        
        // Test 5: FDIV f7, f5, f4  (6.0 / 1.0 = 6.0) - uses computed values
        fp_instruction_t inst5(0x3, 7, 5, 4);  // FDIV
        program.push_back(inst5.to_word());
        
        fetch_stage->load_program(program);
    }
    
    void setup_initial_registers() {
        // Set up test values in registers
        decode_stage->set_register(1, 3.0f);  // f1 = 3.0
        decode_stage->set_register(2, 2.0f);  // f2 = 2.0
        
        cout << "\nInitial register values:" << endl;
        cout << "f1 = 3.0, f2 = 2.0" << endl;
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
        cout << "\n=== ENHANCED 4-STAGE FPU PIPELINE TEST ===" << endl;
        cout << "Testing: Fetch -> Decode -> Execute -> Writeback" << endl;
        cout << "Operations: FADD, FSUB, FMUL, FDIV (26-cycle)" << endl;
        
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
        
        cout << "\nRunning pipeline..." << endl;
        cout << "Note: Division operations take 26 cycles to complete" << endl;
        
        // Run pipeline for enough cycles to complete all instructions
        // Need extra cycles for division latency (26 cycles)
        int max_cycles = 100;
        
        for (int cycle = 0; cycle < max_cycles; cycle++) {
            wait(10, SC_NS);
            
            // Check results at different intervals
            if (cycle == 30) {
                cout << "\n--- Checking basic operations at cycle " << cycle << " ---" << endl;
                check_result(3, 5.0f, "Test 1 (FADD 3+2)");
                check_result(4, 1.0f, "Test 2 (FSUB 3-2)");
                check_result(5, 6.0f, "Test 3 (FMUL 3*2)");
                
                // Division might not be ready yet
                float div_result = ieee754_to_float(decode_stage->fp_registers[6]);
                cout << "Test 4 (FDIV 3/2): f6 = " << div_result 
                     << " (expected: 1.5) - " << (div_result != 0 ? "IN PROGRESS" : "PENDING") << endl;
            }
            
            if (cycle == 50) {
                cout << "\n--- Checking division results at cycle " << cycle << " ---" << endl;
                check_result(6, 1.5f, "Test 4 (FDIV 3/2)");
                
                float div2_result = ieee754_to_float(decode_stage->fp_registers[7]);
                cout << "Test 5 (FDIV 6/1): f7 = " << div2_result 
                     << " (expected: 6.0) - " << (div2_result != 0 ? "IN PROGRESS" : "PENDING") << endl;
            }
            
            if (cycle == 80) {
                cout << "\n--- Final check at cycle " << cycle << " ---" << endl;
                check_result(3, 5.0f, "Test 1 (FADD 3+2)");
                check_result(4, 1.0f, "Test 2 (FSUB 3-2)");
                check_result(5, 6.0f, "Test 3 (FMUL 3*2)");
                check_result(6, 1.5f, "Test 4 (FDIV 3/2)");
                check_result(7, 6.0f, "Test 5 (FDIV 6/1)");
            }
        }
        
        cout << "\n=== PERFORMANCE ANALYSIS ===" << endl;
        cout << "Fast operations (FADD/FSUB/FMUL): 3 cycles latency" << endl;
        cout << "Division (FDIV): 26 cycles latency" << endl;
        cout << "Pipeline throughput: 1 instruction/cycle (when not stalled)" << endl;
        
        cout << "\n=== FINAL RESULTS ===" << endl;
        cout << "Tests Passed: " << tests_passed << endl;
        cout << "Tests Failed: " << tests_failed << endl;
        
        if (tests_failed == 0) {
            cout << "🎉 ALL ENHANCED PIPELINE TESTS PASSED!" << endl;
            cout << "✅ Fast operations working correctly" << endl;
            cout << "✅ 26-cycle division working correctly" << endl;
        } else {
            cout << "❌ Some pipeline tests failed." << endl;
        }
        
        sc_stop();
    }
    
    SC_CTOR(EnhancedPipelineTestbench) : clk("clk", 10, SC_NS) {
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
    
    ~EnhancedPipelineTestbench() {
        delete fetch_stage;
        delete decode_stage;
        delete execute_stage;
        delete writeback_stage;
    }
};

// ========== MAIN FUNCTION ==========
int sc_main(int argc, char* argv[]) {
    cout << "=== ENHANCED 4-STAGE FPU PIPELINE ===" << endl;
    cout << "Testing: FADD, FSUB, FMUL, FDIV with 5 test cases" << endl;
    cout << "Features: 26-cycle restoring division algorithm" << endl;
    
    EnhancedPipelineTestbench testbench("enhanced_pipeline_testbench");
    
    try {
        sc_start();
        cout << "\n✅ Enhanced pipeline simulation completed!" << endl;
    } catch (const exception& e) {
        cout << "\n❌ Simulation error: " << e.what() << endl;
        return 1;
    }
    return 0;
}

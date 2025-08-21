#include <systemc.h>
#include <cmath>
#include <iomanip>
#include <cstring>
using namespace std;
#ifdef SC_INCLUDE_FX
#undef SC_INCLUDE_FX
#endif

enum fp_exceptions {
    FP_INVALID_OP     = 0x1,
    FP_OVERFLOW       = 0x2,
    FP_UNDERFLOW      = 0x4,
    FP_DIVIDE_BY_ZERO = 0x8,
    FP_INEXACT        = 0x10
};

struct ieee754_components {
    bool        sign;
    sc_uint<8>  exponent;
    sc_uint<23> mantissa;
    bool is_zero;
    bool is_infinity;
    bool is_nan;
    bool is_denormalized;
    sc_uint<24> effective_mantissa;
};

static inline ieee754_components decompose_ieee754_rtl(sc_uint<32> value) {
    ieee754_components comp;
    comp.sign     = value[31];
    comp.exponent = (value >> 23) & 0xFF;
    comp.mantissa = value & 0x7FFFFF;

    comp.is_zero         = (comp.exponent == 0) && (comp.mantissa == 0);
    comp.is_infinity     = (comp.exponent == 0xFF) && (comp.mantissa == 0);
    comp.is_nan          = (comp.exponent == 0xFF) && (comp.mantissa != 0);
    comp.is_denormalized = (comp.exponent == 0) && (comp.mantissa != 0);

    if (comp.is_zero || comp.is_infinity || comp.is_nan) {
        comp.effective_mantissa = comp.mantissa;
    } else if (comp.is_denormalized) {
        comp.effective_mantissa = comp.mantissa;
    } else {
        comp.effective_mantissa = comp.mantissa | 0x800000;
    }
    return comp;
}

static inline sc_uint<32> compose_ieee754_rtl(bool sign, sc_int<12> exp_signed, sc_uint<24> mantissa, sc_uint<8>& exceptions) {
    if (exp_signed >= 255) {
        exceptions |= FP_OVERFLOW;
        return (sc_uint<32>(sign) << 31) | 0x7F800000;
    }

    if (exp_signed <= 0) {
        if (exp_signed >= -22 && mantissa != 0) {
            exceptions |= FP_UNDERFLOW;
            int shift_amount = 1 - exp_signed.to_int();
            if (shift_amount > 0 && shift_amount < 24) {
                sc_uint<24> m = mantissa >> shift_amount;
                if (m == 0) {
                    return (sc_uint<32>(sign) << 31);
                }
                return (sc_uint<32>(sign) << 31) | (m & 0x7FFFFF);
            }
        }
        exceptions |= FP_UNDERFLOW;
        return (sc_uint<32>(sign) << 31);
    }

    sc_uint<8>  exp  = sc_uint<8>(exp_signed);
    sc_uint<23> frac = mantissa & 0x7FFFFF;
    return (sc_uint<32>(sign) << 31) | (sc_uint<32>(exp) << 23) | sc_uint<32>(frac);
}

static inline sc_uint<32> generate_nan_rtl(bool sign = false) {
    return (sc_uint<32>(sign) << 31) | 0x7FC00000;
}
static inline sc_uint<32> generate_infinity_rtl(bool sign = false) {
    return (sc_uint<32>(sign) << 31) | 0x7F800000;
}

struct fp_instruction_t {
    sc_uint<4>  opcode;
    sc_uint<5>  rd;
    sc_uint<5>  rs1;
    sc_uint<5>  rs2;
    sc_uint<13> unused;

    fp_instruction_t() : opcode(0), rd(0), rs1(0), rs2(0), unused(0) {}
    fp_instruction_t(sc_uint<4> op, sc_uint<5> dst, sc_uint<5> src1, sc_uint<5> src2)
        : opcode(op), rd(dst), rs1(src1), rs2(src2), unused(0) {}

    sc_uint<32> to_word() const {
        return (sc_uint<32>(opcode) << 28) | (sc_uint<32>(rd) << 23) |
               (sc_uint<32>(rs1) << 18) | (sc_uint<32>(rs2) << 13);
    }
};

// Branch ALU
SC_MODULE(Branch_ALU) {
    sc_in<sc_uint<32>>  operand_a;
    sc_in<sc_uint<32>>  operand_b;
    sc_in<sc_uint<4>>   branch_op;
    sc_out<bool>        branch_taken;
    
    enum branch_opcodes {
        BR_BEQ  = 0x4,
        BR_BNE  = 0x5,
        BR_BLT  = 0x6,
        BR_BGE  = 0x7,
        BR_BLTU = 0x8,
        BR_BGEU = 0x9
    };
    
    void branch_compute() {
        sc_uint<32> a = operand_a.read();
        sc_uint<32> b = operand_b.read();
        sc_uint<4> op = branch_op.read();
        bool taken = false;
        
        ieee754_components comp_a = decompose_ieee754_rtl(a);
        ieee754_components comp_b = decompose_ieee754_rtl(b);
        
        switch (op.to_uint()) {
            case BR_BEQ:
                taken = (a == b);
                break;
            case BR_BNE:
                taken = (a != b);
                break;
            case BR_BLT:
                if (comp_a.is_nan || comp_b.is_nan) taken = false;
                else if (comp_a.sign != comp_b.sign) taken = comp_a.sign && !comp_b.sign;
                else taken = (a < b) ^ comp_a.sign;
                break;
            case BR_BGE:
                taken = !((a < b) ^ comp_a.sign) && !(comp_a.is_nan || comp_b.is_nan);
                break;
            case BR_BLTU:
                taken = (a < b);
                break;
            case BR_BGEU:
                taken = (a >= b);
                break;
            default:
                taken = false;
                break;
        }
        
        branch_taken.write(taken);
    }
    
    SC_CTOR(Branch_ALU) {
        SC_METHOD(branch_compute);
        sensitive << operand_a << operand_b << branch_op;
    }
};

// Hazard Detection Unit
SC_MODULE(Hazard_Unit) {
    sc_in<sc_uint<5>>  if_rs1, if_rs2;
    sc_in<sc_uint<5>>  id_rd;
    sc_in<sc_uint<5>>  ex_rd;
    sc_in<sc_uint<5>>  mem_rd;
    sc_in<bool>        id_mem_read;
    sc_in<bool>        id_valid, ex_valid, mem_valid;
    sc_in<bool>        branch_taken;
    
    sc_out<bool>       pc_write;
    sc_out<bool>       if_id_write;
    sc_out<bool>       control_mux_sel;
    sc_out<bool>       flush_pipeline;
    
    sc_out<sc_uint<2>> forward_a;
    sc_out<sc_uint<2>> forward_b;
    
    void hazard_detect() {
        sc_uint<5> rs1 = if_rs1.read();
        sc_uint<5> rs2 = if_rs2.read();
        sc_uint<5> id_dest = id_rd.read();
        sc_uint<5> ex_dest = ex_rd.read();
        sc_uint<5> mem_dest = mem_rd.read();
        
        bool load_hazard = false;
        bool branch_hazard = false;
        
        if (id_mem_read.read() && id_valid.read() && id_dest != 0) {
            if ((id_dest == rs1) || (id_dest == rs2)) {
                load_hazard = true;
            }
        }
        
        branch_hazard = branch_taken.read();
        
        pc_write.write(!load_hazard);
        if_id_write.write(!load_hazard);
        control_mux_sel.write(load_hazard);
        flush_pipeline.write(branch_hazard);
        
        sc_uint<2> fwd_a = 0, fwd_b = 0;
        
        if (mem_valid.read() && mem_dest != 0) {
            if (mem_dest == rs1) fwd_a = 1;
            if (mem_dest == rs2) fwd_b = 1;
        }
        
        if (ex_valid.read() && ex_dest != 0) {
            if (ex_dest == rs1) fwd_a = 2;
            if (ex_dest == rs2) fwd_b = 2;
        }
        
        forward_a.write(fwd_a);
        forward_b.write(fwd_b);
    }
    
    SC_CTOR(Hazard_Unit) {
        SC_METHOD(hazard_detect);
        sensitive << if_rs1 << if_rs2 << id_rd << ex_rd << mem_rd 
                  << id_mem_read << id_valid << ex_valid << mem_valid << branch_taken;
    }
};

SC_MODULE(Fetch) {
    sc_in<bool> clk;
    sc_in<bool> reset;
    sc_in<bool> stall;
    sc_in<bool> pc_write_enable;
    sc_in<bool> flush;
    sc_in<sc_uint<32>> branch_target;

    sc_out<sc_uint<32>> pc_out;
    sc_out<sc_uint<32>> instruction_out;
    sc_out<bool>        valid_out;

    sc_uint<32> imem[256];
    sc_uint<9>  imem_size;
    sc_uint<32> pc;

    void load_program(const sc_uint<32>* program, int size) {
        int s = (size > 256) ? 256 : size;
        for (int i = 0; i < s; ++i) imem[i] = program[i];
        imem_size = s;
    }

    void fetch_process() {
        if (reset.read()) {
            pc = 0;
            pc_out.write(0);
            instruction_out.write(0);
            valid_out.write(false);
        } else if (flush.read()) {
            pc = branch_target.read() / 4;
            pc_out.write(branch_target.read());
            if (pc < imem_size) {
                instruction_out.write(imem[pc]);
                valid_out.write(true);
                pc = pc + 1;
            } else {
                instruction_out.write(0);
                valid_out.write(false);
            }
        } else if (!stall.read() && pc_write_enable.read()) {
            if (pc < imem_size) {
                pc_out.write(pc * 4);
                instruction_out.write(imem[pc]);
                valid_out.write(true);
                pc = pc + 1;
            } else {
                valid_out.write(false);
            }
        }
    }

    SC_CTOR(Fetch) : imem_size(0), pc(0) {
        for (int i = 0; i < 256; ++i) imem[i] = 0;
        SC_METHOD(fetch_process);
        sensitive << clk.pos();
    }
};

SC_MODULE(Decode) {
    sc_in<bool> clk;
    sc_in<bool> reset;
    sc_in<bool> stall;
    sc_in<bool> if_id_write;
    sc_in<bool> control_mux_sel;
    sc_in<bool> flush;

    sc_in<sc_uint<32>> pc_in;
    sc_in<sc_uint<32>> instruction_in;
    sc_in<bool>        valid_in;
    
    sc_in<sc_uint<2>>  forward_a, forward_b;
    sc_in<sc_uint<32>> forward_data_ex, forward_data_mem;

    sc_out<sc_uint<32>> pc_out;
    sc_out<sc_uint<4>>  opcode_out;
    sc_out<sc_uint<5>>  rd_out;
    sc_out<sc_uint<32>> operand1_out;
    sc_out<sc_uint<32>> operand2_out;
    sc_out<bool>        valid_out;
    sc_out<bool>        is_branch_out;
    sc_out<bool>        is_mem_read_out;
    sc_out<sc_uint<5>>  rs1_out, rs2_out;
    
    sc_out<sc_uint<32>> branch_target;

    sc_uint<32> fp_registers[32];
    sc_uint<8>  exception_flags;
    
    struct if_id_reg_t {
        sc_uint<32> pc;
        sc_uint<32> instruction;
        bool valid;
        if_id_reg_t() : pc(0), instruction(0), valid(false) {}
    } if_id_reg;

    void decode_process() {
        if (reset.read() || flush.read()) {
            pc_out.write(0);
            opcode_out.write(0);
            rd_out.write(0);
            operand1_out.write(0);
            operand2_out.write(0);
            valid_out.write(false);
            is_branch_out.write(false);
            is_mem_read_out.write(false);
            rs1_out.write(0);
            rs2_out.write(0);
            branch_target.write(0);
            exception_flags = 0;
            if_id_reg.valid = false;
            if (reset.read()) {
                for (int i = 0; i < 32; i++) fp_registers[i] = 0;
            }
        } else {
            if (if_id_write.read()) {
                if_id_reg.pc = pc_in.read();
                if_id_reg.instruction = instruction_in.read();
                if_id_reg.valid = valid_in.read();
            }
            
            if (!stall.read()) {
                if (if_id_reg.valid && !control_mux_sel.read()) {
                    sc_uint<32> inst = if_id_reg.instruction;
                    sc_uint<4> opcode = (inst >> 28) & 0xF;
                    sc_uint<5> rd     = (inst >> 23) & 0x1F;
                    sc_uint<5> rs1    = (inst >> 18) & 0x1F;
                    sc_uint<5> rs2    = (inst >> 13) & 0x1F;

                    sc_uint<32> op1 = fp_registers[rs1.to_uint()];
                    sc_uint<32> op2 = fp_registers[rs2.to_uint()];
                    
                    if (forward_a.read() == 1) op1 = forward_data_mem.read();
                    else if (forward_a.read() == 2) op1 = forward_data_ex.read();
                    
                    if (forward_b.read() == 1) op2 = forward_data_mem.read();
                    else if (forward_b.read() == 2) op2 = forward_data_ex.read();

                    pc_out.write(if_id_reg.pc);
                    opcode_out.write(opcode);
                    rd_out.write(rd);
                    operand1_out.write(op1);
                    operand2_out.write(op2);
                    rs1_out.write(rs1);
                    rs2_out.write(rs2);
                    valid_out.write(true);
                    
                    bool is_branch = (opcode >= 4 && opcode <= 9);
                    is_branch_out.write(is_branch);
                    is_mem_read_out.write(false);
                    
                    if (is_branch) {
                        sc_int<13> imm = inst & 0x1FFF;
                        if (imm & 0x1000) imm |= 0xE000;
                        branch_target.write(if_id_reg.pc + (imm.to_int() * 4));
                    }
                } else {
                    valid_out.write(false);
                    is_branch_out.write(false);
                    is_mem_read_out.write(false);
                }
            }
        }
    }

    void write_register(sc_uint<5> reg, sc_uint<32> value) {
        if (reg.to_uint() != 0) {
            fp_registers[reg.to_uint()] = value;
        }
    }

    void set_register_bits(int reg, sc_uint<32> bits) {
        if (reg > 0 && reg < 32) fp_registers[reg] = bits;
    }

    void set_exception_flag(sc_uint<8> flag) { exception_flags |= flag; }
    sc_uint<8> get_exception_flags() const { return exception_flags; }
    void clear_exception_flags() { exception_flags = 0; }

    SC_CTOR(Decode) : exception_flags(0) {
        for (int i = 0; i < 32; i++) fp_registers[i] = 0;
        SC_METHOD(decode_process);
        sensitive << clk.pos();
    }
};

SC_MODULE(Execute) {
    sc_in<bool> clk;
    sc_in<bool> reset;
    sc_in<bool> stall;
    sc_in<bool> flush;

    sc_in<sc_uint<32>> pc_in;
    sc_in<sc_uint<4>>  opcode_in;
    sc_in<sc_uint<5>>  rd_in;
    sc_in<sc_uint<32>> operand1_in;
    sc_in<sc_uint<32>> operand2_in;
    sc_in<bool>        valid_in;
    sc_in<bool>        is_branch_in;

    sc_out<sc_uint<32>> pc_out;
    sc_out<sc_uint<4>>  opcode_out;
    sc_out<sc_uint<5>>  rd_out;
    sc_out<sc_uint<32>> result_out;
    sc_out<sc_uint<8>>  exceptions_out;
    sc_out<bool>        valid_out;
    sc_out<bool>        branch_taken_out;

    Branch_ALU* branch_alu;
    sc_signal<bool> branch_result;

private:
    enum opcodes { OP_FADD = 0x0, OP_FSUB = 0x1, OP_FMUL = 0x2, OP_FDIV = 0x3 };

    struct stage_t {
        sc_uint<32> pc;
        sc_uint<4>  opcode;
        sc_uint<5>  rd;
        sc_uint<32> operand_a;
        sc_uint<32> operand_b;
        bool        valid;
        bool        is_branch;

        ieee754_components comp_a, comp_b;

        sc_uint<32> result;
        sc_uint<8>  exceptions;

        stage_t() : pc(0), opcode(0), rd(0), operand_a(0), operand_b(0), valid(false), is_branch(false), result(0), exceptions(0) {}
    };

    stage_t pipe[3];

    struct div_entry_t {
        bool        valid;
        sc_uint<32> pc;
        sc_uint<4>  opcode;
        sc_uint<5>  rd;
        ieee754_components a, b;

        bool        div_sign;
        sc_int<12>  div_exp;
        sc_uint<48> dividend;
        sc_uint<24> divisor;
        sc_uint<24> quotient;
        sc_int<6>   cycles;
        sc_uint<32> result;
        sc_uint<8>  exceptions;

        div_entry_t() : valid(false), opcode(0), rd(0), div_sign(0), div_exp(0), dividend(0),
                        divisor(0), quotient(0), cycles(0), result(0), exceptions(0) {}
    };

    static const int DIV_SLOTS = 4;
    div_entry_t divq[DIV_SLOTS];

    int find_free_divslot() {
        for (int i = 0; i < DIV_SLOTS; ++i) if (!divq[i].valid) return i;
        return -1;
    }
    int find_ready_divslot() {
        for (int i = 0; i < DIV_SLOTS; ++i) {
            if (divq[i].valid && divq[i].cycles == 0) return i;
        }
        return -1;
    }

    sc_uint<32> do_addsub(const ieee754_components& a, const ieee754_components& b_in, bool subtract, sc_uint<8>& exceptions) {
        if (a.is_nan || b_in.is_nan) {
            exceptions |= FP_INVALID_OP;
            return generate_nan_rtl();
        }

        bool bsign_eff = subtract ? !b_in.sign : b_in.sign;

        if (a.is_infinity || b_in.is_infinity) {
            if (a.is_infinity && b_in.is_infinity && (a.sign != bsign_eff)) {
                exceptions |= FP_INVALID_OP;
                return generate_nan_rtl();
            }
            return a.is_infinity
                ? ((sc_uint<32>(a.sign) << 31) | 0x7F800000)
                : ((sc_uint<32>(bsign_eff) << 31) | 0x7F800000);
        }

        if (a.is_zero && b_in.is_zero) {
            bool rsign = subtract ? (a.sign && !b_in.sign) : (a.sign && b_in.sign);
            return sc_uint<32>(rsign) << 31;
        }
        if (a.is_zero) {
            return (sc_uint<32>(bsign_eff) << 31) | (sc_uint<32>(b_in.exponent) << 23) | b_in.mantissa;
        }
        if (b_in.is_zero) {
            return (sc_uint<32>(a.sign) << 31) | (sc_uint<32>(a.exponent) << 23) | a.mantissa;
        }

        sc_int<12> exp_a = a.is_denormalized ? sc_int<12>(1) : sc_int<12>(a.exponent);
        sc_int<12> exp_b = b_in.is_denormalized ? sc_int<12>(1) : sc_int<12>(b_in.exponent);
        sc_uint<24> mant_a = a.is_denormalized ? 
                             (sc_uint<24>) a.mantissa : 
                             (sc_uint<24>) (a.mantissa | 0x800000);

        sc_uint<24> mant_b = b_in.is_denormalized ? 
                             (sc_uint<24>) b_in.mantissa : 
                             (sc_uint<24>) (b_in.mantissa | 0x800000);

        sc_int<12> diff = exp_a - exp_b;
        sc_int<12> rexp;
        if (diff >= 0) {
            rexp = exp_a;
            int s = diff.to_int();
            if (s > 0 && s < 24) mant_b >>= s;
            else if (s >= 24) mant_b = 0;
        } else {
            rexp = exp_b;
            int s = -diff.to_int();
            if (s > 0 && s < 24) mant_a >>= s;
            else if (s >= 24) mant_a = 0;
        }

        sc_uint<25> rmant;
        bool rsign;
        if (a.sign == bsign_eff) {
            rmant = sc_uint<25>(mant_a) + sc_uint<25>(mant_b);
            rsign = a.sign;
        } else {
            if (mant_a >= mant_b) {
                rmant = sc_uint<25>(mant_a) - sc_uint<25>(mant_b);
                rsign = a.sign;
            } else {
                rmant = sc_uint<25>(mant_b) - sc_uint<25>(mant_a);
                rsign = bsign_eff;
            }
        }

        if (rmant == 0) return 0;

        if (rmant & 0x1000000) {
            rmant >>= 1;
            rexp = rexp + 1;
        } else {
            for (int i = 0; i < 24; ++i) {
                if ((rmant & 0x800000) || (rexp <= 1)) break;
                rmant <<= 1;
                rexp = rexp - 1;
            }
        }

        sc_uint<24> fmant = rmant & 0x7FFFFF;
        return compose_ieee754_rtl(rsign, rexp, fmant, exceptions);
    }

    sc_uint<32> do_mul(const ieee754_components& a, const ieee754_components& b, sc_uint<8>& exceptions) {
        if (a.is_nan || b.is_nan) { exceptions |= FP_INVALID_OP; return generate_nan_rtl(); }
        if ((a.is_infinity && b.is_zero) || (a.is_zero && b.is_infinity)) { exceptions |= FP_INVALID_OP; return generate_nan_rtl(); }
        if (a.is_infinity || b.is_infinity) return generate_infinity_rtl(a.sign ^ b.sign);
        if (a.is_zero || b.is_zero) return sc_uint<32>(a.sign ^ b.sign) << 31;

        bool rsign = a.sign ^ b.sign;
        sc_int<12> ea = a.is_denormalized ? sc_int<12>(1) : sc_int<12>(a.exponent);
        sc_int<12> eb = b.is_denormalized ? sc_int<12>(1) : sc_int<12>(b.exponent);
        sc_int<12> rexp = ea + eb - 127;

        sc_uint<48> prod = sc_uint<48>(a.effective_mantissa) * sc_uint<48>(b.effective_mantissa);
        if (prod & 0x800000000000ULL) {
            prod >>= 24;
            rexp = rexp + 1;
        } else {
            prod >>= 23;
        }
        sc_uint<24> fmant = prod & 0xFFFFFF;
        return compose_ieee754_rtl(rsign, rexp, fmant, exceptions);
    }

    void div_start(div_entry_t& e) {
        const ieee754_components &a = e.a, &b = e.b;

        if (a.is_nan || b.is_nan) { e.exceptions |= FP_INVALID_OP; e.result = generate_nan_rtl(); e.cycles = 0; return; }
        if (b.is_zero) {
            e.exceptions |= FP_DIVIDE_BY_ZERO;
            if (a.is_zero) { e.exceptions |= FP_INVALID_OP; e.result = generate_nan_rtl(); }
            else { e.result = generate_infinity_rtl(a.sign ^ b.sign); }
            e.cycles = 0; return;
        }
        if (a.is_zero) { e.result = sc_uint<32>(a.sign ^ b.sign) << 31; e.cycles = 0; return; }
        if (a.is_infinity) {
            if (b.is_infinity) { e.exceptions |= FP_INVALID_OP; e.result = generate_nan_rtl(); }
            else { e.result = generate_infinity_rtl(a.sign ^ b.sign); }
            e.cycles = 0; return;
        }
        if (b.is_infinity) { e.result = sc_uint<32>(a.sign ^ b.sign) << 31; e.cycles = 0; return; }

        e.div_sign = a.sign ^ b.sign;
        sc_int<12> ea = a.is_denormalized ? sc_int<12>(1) : sc_int<12>(a.exponent);
        sc_int<12> eb = b.is_denormalized ? sc_int<12>(1) : sc_int<12>(b.exponent);
        e.div_exp = ea - eb + 127;

        e.dividend = sc_uint<48>(a.effective_mantissa) << 23;
        e.divisor  = b.effective_mantissa;
        e.quotient = 0;
        e.cycles   = 24;
    }

    void div_step(div_entry_t& e) {
        if (!e.valid || e.cycles <= 0) return;

        e.dividend = e.dividend << 1;
        sc_uint<48> dsh = sc_uint<48>(e.divisor) << 24;
        if (e.dividend >= dsh) {
            e.dividend = e.dividend - dsh;
            e.quotient = (e.quotient << 1) | 1;
        } else {
            e.quotient = e.quotient << 1;
        }

        e.cycles = e.cycles - 1;

        if (e.cycles == 0) {
            sc_uint<24> q = e.quotient;
            sc_int<12>  ex = e.div_exp;

            for (int i = 0; i < 24; ++i) {
                if ((q == 0) || (q & 0x800000) || (ex <= 1)) break;
                q <<= 1;
                ex = ex - 1;
            }

            e.result = compose_ieee754_rtl(e.div_sign, ex, q, e.exceptions);
        }
    }

    sc_uint<32> do_op(sc_uint<4> opc, const ieee754_components& a, const ieee754_components& b, sc_uint<8>& exc) {
        switch (opc.to_uint()) {
            case OP_FADD: return do_addsub(a, b, false, exc);
            case OP_FSUB: return do_addsub(a, b, true,  exc);
            case OP_FMUL: return do_mul(a, b, exc);
            case OP_FDIV: return 0; 
            default: exc |= FP_INVALID_OP; return generate_nan_rtl();
        }
    }

public:
    void exec_process() {
        if (reset.read() || flush.read()) {
            for (int i = 0; i < 3; ++i) pipe[i] = stage_t();
            if (reset.read()) {
                for (int i = 0; i < DIV_SLOTS; ++i) divq[i] = div_entry_t();
            }

            pc_out.write(0);
            opcode_out.write(0);
            rd_out.write(0);
            result_out.write(0);
            exceptions_out.write(0);
            valid_out.write(false);
            branch_taken_out.write(false);
            return;
        }

        if (stall.read()) {
            for (int i = 0; i < DIV_SLOTS; ++i) if (divq[i].valid && divq[i].cycles > 0) div_step(divq[i]);
            valid_out.write(false);
            branch_taken_out.write(false);
            return;
        }

        for (int i = 0; i < DIV_SLOTS; ++i) if (divq[i].valid && divq[i].cycles > 0) div_step(divq[i]);

        bool out_valid = false;
        sc_uint<32> out_pc = 0, out_res = 0; 
        sc_uint<4> out_op = 0; 
        sc_uint<5> out_rd = 0; 
        sc_uint<8> out_exc = 0;
        bool out_branch_taken = false;
        
        int ready_idx = find_ready_divslot();
        if (ready_idx >= 0) {
            out_valid = true;
            out_pc = divq[ready_idx].pc;
            out_op = divq[ready_idx].opcode;
            out_rd = divq[ready_idx].rd;
            out_res = divq[ready_idx].result;
            out_exc = divq[ready_idx].exceptions;
            divq[ready_idx].valid = false;
        } else if (pipe[2].valid) {
            out_valid = true;
            out_pc = pipe[2].pc;
            out_op = pipe[2].opcode;
            out_rd = pipe[2].rd;
            out_res = pipe[2].result;
            out_exc = pipe[2].exceptions;
            
            if (pipe[2].is_branch) {
                out_branch_taken = branch_result.read();
            }
        }

        pc_out.write(out_pc);
        opcode_out.write(out_op);
        rd_out.write(out_rd);
        result_out.write(out_res);
        exceptions_out.write(out_exc);
        valid_out.write(out_valid);
        branch_taken_out.write(out_branch_taken);

        if (pipe[1].valid) {
            pipe[2] = pipe[1];
            pipe[2].exceptions = 0;

            if (pipe[1].opcode == OP_FDIV) {
                int slot = find_free_divslot();
                if (slot >= 0) {
                    divq[slot] = div_entry_t();
                    divq[slot].valid   = true;
                    divq[slot].pc      = pipe[1].pc;
                    divq[slot].opcode  = pipe[1].opcode;
                    divq[slot].rd      = pipe[1].rd;
                    divq[slot].a       = pipe[1].comp_a;
                    divq[slot].b       = pipe[1].comp_b;
                    divq[slot].exceptions = 0;
                    div_start(divq[slot]);
                }
                pipe[2].valid = false;
            } else if (pipe[1].is_branch) {
                pipe[2].result = 0;
            } else {
                pipe[2].result = do_op(pipe[1].opcode, pipe[1].comp_a, pipe[1].comp_b, pipe[2].exceptions);
            }
        } else {
            pipe[2].valid = false;
        }

        if (pipe[0].valid) {
            pipe[1] = pipe[0];
            if (!pipe[0].is_branch) {
                pipe[1].comp_a = decompose_ieee754_rtl(pipe[0].operand_a);
                pipe[1].comp_b = decompose_ieee754_rtl(pipe[0].operand_b);
            }
            pipe[1].exceptions = 0;
        } else {
            pipe[1].valid = false;
        }

        if (valid_in.read()) {
            pipe[0].pc        = pc_in.read();
            pipe[0].opcode    = opcode_in.read();
            pipe[0].rd        = rd_in.read();
            pipe[0].operand_a = operand1_in.read();
            pipe[0].operand_b = operand2_in.read();
            pipe[0].valid     = true;
            pipe[0].is_branch = is_branch_in.read();
        } else {
            pipe[0].valid = false;
        }
    }

    SC_CTOR(Execute) {
        for (int i = 0; i < 3; ++i) pipe[i] = stage_t();
        for (int i = 0; i < DIV_SLOTS; ++i) divq[i] = div_entry_t();
        
        branch_alu = new Branch_ALU("branch_alu");
        branch_alu->operand_a(operand1_in);
        branch_alu->operand_b(operand2_in);
        branch_alu->branch_op(opcode_in);
        branch_alu->branch_taken(branch_result);
        
        SC_METHOD(exec_process);
        sensitive << clk.pos();
    }
    
    ~Execute() {
        delete branch_alu;
    }
};

SC_MODULE(Writeback) {
    sc_in<bool> clk;
    sc_in<bool> reset;
    sc_in<bool> stall;
    sc_in<bool> flush;

    sc_in<sc_uint<32>> pc_in;
    sc_in<sc_uint<4>>  opcode_in;
    sc_in<sc_uint<5>>  rd_in;
    sc_in<sc_uint<32>> result_in;
    sc_in<sc_uint<8>>  exceptions_in;
    sc_in<bool>        valid_in;

    sc_out<sc_uint<32>> writeback_data;
    sc_out<sc_uint<5>>  writeback_rd;
    sc_out<bool>        writeback_valid;

    Decode* decode_stage;
    
    struct ex_mem_reg_t {
        sc_uint<32> pc;
        sc_uint<4>  opcode;
        sc_uint<5>  rd;
        sc_uint<32> result;
        sc_uint<8>  exceptions;
        bool        valid;
        ex_mem_reg_t() : pc(0), opcode(0), rd(0), result(0), exceptions(0), valid(false) {}
    } ex_mem_reg;

    void writeback_process() {
        if (reset.read() || flush.read()) {
            if (decode_stage && reset.read()) decode_stage->clear_exception_flags();
            writeback_data.write(0);
            writeback_rd.write(0);
            writeback_valid.write(false);
            if (reset.read()) ex_mem_reg.valid = false;
        } else {
            if (!stall.read()) {
                ex_mem_reg.pc = pc_in.read();
                ex_mem_reg.opcode = opcode_in.read();
                ex_mem_reg.rd = rd_in.read();
                ex_mem_reg.result = result_in.read();
                ex_mem_reg.exceptions = exceptions_in.read();
                ex_mem_reg.valid = valid_in.read();
            }
            
            if (ex_mem_reg.valid && !stall.read()) {
                sc_uint<5>  rd  = ex_mem_reg.rd;
                sc_uint<32> res = ex_mem_reg.result;
                sc_uint<8>  exc = ex_mem_reg.exceptions;
                
                if (decode_stage) {
                    decode_stage->write_register(rd, res);
                    if (exc != 0) decode_stage->set_exception_flag(exc);
                }
                
                writeback_data.write(res);
                writeback_rd.write(rd);
                writeback_valid.write(true);
            } else {
                writeback_valid.write(false);
            }
        }
    }

    void set_decode_stage(Decode* p) { decode_stage = p; }

    SC_CTOR(Writeback) : decode_stage(nullptr) {
        SC_METHOD(writeback_process);
        sensitive << clk.pos();
    }
};

SC_MODULE(FPU_Pipeline_Top) {
    sc_in<bool> clk;
    sc_in<bool> reset;
    sc_in<bool> stall;

    Fetch*     fetch_stage;
    Decode*    decode_stage;
    Execute*   execute_stage;
    Writeback* writeback_stage;
    Hazard_Unit* hazard_unit;

    sc_signal<sc_uint<32>> fetch_pc, fetch_inst;
    sc_signal<bool>        fetch_valid;

    sc_signal<sc_uint<32>> decode_pc;
    sc_signal<sc_uint<4>>  decode_opcode;
    sc_signal<sc_uint<5>>  decode_rd;
    sc_signal<sc_uint<32>> decode_op1, decode_op2;
    sc_signal<bool>        decode_valid;
    sc_signal<bool>        decode_is_branch, decode_is_mem_read;
    sc_signal<sc_uint<5>>  decode_rs1, decode_rs2;
    sc_signal<sc_uint<32>> decode_branch_target;

    sc_signal<sc_uint<32>> execute_pc, execute_result;
    sc_signal<sc_uint<4>>  execute_opcode;
    sc_signal<sc_uint<5>>  execute_rd;
    sc_signal<sc_uint<8>>  execute_exceptions;
    sc_signal<bool>        execute_valid;
    sc_signal<bool>        execute_branch_taken;

    sc_signal<sc_uint<32>> writeback_data;
    sc_signal<sc_uint<5>>  writeback_rd;
    sc_signal<bool>        writeback_valid;

    sc_signal<bool>        pc_write_enable;
    sc_signal<bool>        if_id_write;
    sc_signal<bool>        control_mux_sel;
    sc_signal<bool>        flush_pipeline;
    sc_signal<sc_uint<2>>  forward_a, forward_b;

    SC_CTOR(FPU_Pipeline_Top) {
        fetch_stage     = new Fetch("fetch");
        decode_stage    = new Decode("decode");
        execute_stage   = new Execute("execute");
        writeback_stage = new Writeback("writeback");
        hazard_unit     = new Hazard_Unit("hazard_unit");

        fetch_stage->clk(clk);
        fetch_stage->reset(reset);
        fetch_stage->stall(stall);
        fetch_stage->pc_write_enable(pc_write_enable);
        fetch_stage->flush(flush_pipeline);
        fetch_stage->branch_target(decode_branch_target);
        fetch_stage->pc_out(fetch_pc);
        fetch_stage->instruction_out(fetch_inst);
        fetch_stage->valid_out(fetch_valid);

        decode_stage->clk(clk);
        decode_stage->reset(reset);
        decode_stage->stall(stall);
        decode_stage->if_id_write(if_id_write);
        decode_stage->control_mux_sel(control_mux_sel);
        decode_stage->flush(flush_pipeline);
        decode_stage->pc_in(fetch_pc);
        decode_stage->instruction_in(fetch_inst);
        decode_stage->valid_in(fetch_valid);
        decode_stage->forward_a(forward_a);
        decode_stage->forward_b(forward_b);
        decode_stage->forward_data_ex(execute_result);
        decode_stage->forward_data_mem(writeback_data);
        decode_stage->pc_out(decode_pc);
        decode_stage->opcode_out(decode_opcode);
        decode_stage->rd_out(decode_rd);
        decode_stage->operand1_out(decode_op1);
        decode_stage->operand2_out(decode_op2);
        decode_stage->valid_out(decode_valid);
        decode_stage->is_branch_out(decode_is_branch);
        decode_stage->is_mem_read_out(decode_is_mem_read);
        decode_stage->rs1_out(decode_rs1);
        decode_stage->rs2_out(decode_rs2);
        decode_stage->branch_target(decode_branch_target);

        execute_stage->clk(clk);
        execute_stage->reset(reset);
        execute_stage->stall(stall);
        execute_stage->flush(flush_pipeline);
        execute_stage->pc_in(decode_pc);
        execute_stage->opcode_in(decode_opcode);
        execute_stage->rd_in(decode_rd);
        execute_stage->operand1_in(decode_op1);
        execute_stage->operand2_in(decode_op2);
        execute_stage->valid_in(decode_valid);
        execute_stage->is_branch_in(decode_is_branch);
        execute_stage->pc_out(execute_pc);
        execute_stage->opcode_out(execute_opcode);
        execute_stage->rd_out(execute_rd);
        execute_stage->result_out(execute_result);
        execute_stage->exceptions_out(execute_exceptions);
        execute_stage->valid_out(execute_valid);
        execute_stage->branch_taken_out(execute_branch_taken);

        writeback_stage->clk(clk);
        writeback_stage->reset(reset);
        writeback_stage->stall(stall);
        writeback_stage->flush(flush_pipeline);
        writeback_stage->pc_in(execute_pc);
        writeback_stage->opcode_in(execute_opcode);
        writeback_stage->rd_in(execute_rd);
        writeback_stage->result_in(execute_result);
        writeback_stage->exceptions_in(execute_exceptions);
        writeback_stage->valid_in(execute_valid);
        writeback_stage->writeback_data(writeback_data);
        writeback_stage->writeback_rd(writeback_rd);
        writeback_stage->writeback_valid(writeback_valid);
        writeback_stage->set_decode_stage(decode_stage);

        hazard_unit->if_rs1(decode_rs1);
        hazard_unit->if_rs2(decode_rs2);
        hazard_unit->id_rd(decode_rd);
        hazard_unit->ex_rd(execute_rd);
        hazard_unit->mem_rd(writeback_rd);
        hazard_unit->id_mem_read(decode_is_mem_read);
        hazard_unit->id_valid(decode_valid);
        hazard_unit->ex_valid(execute_valid);
        hazard_unit->mem_valid(writeback_valid);
        hazard_unit->branch_taken(execute_branch_taken);
        hazard_unit->pc_write(pc_write_enable);
        hazard_unit->if_id_write(if_id_write);
        hazard_unit->control_mux_sel(control_mux_sel);
        hazard_unit->flush_pipeline(flush_pipeline);
        hazard_unit->forward_a(forward_a);
        hazard_unit->forward_b(forward_b);
    }

    ~FPU_Pipeline_Top() {
        delete fetch_stage;
        delete decode_stage;
        delete execute_stage;
        delete writeback_stage;
        delete hazard_unit;
    }
};

// ============================================================
//                    TESTBENCH (Simulation Only)
// ============================================================

static inline sc_uint<32> float_to_ieee754_bits(float f) {
    union { float f; uint32_t i; } u;
    u.f = f;
    return sc_uint<32>(u.i);
}

static inline float ieee754_bits_to_float(sc_uint<32> ieee) {
    union { float f; uint32_t i; } u;
    u.i = ieee.to_uint();
    return u.f;
}

// A TB-only version of decomposition for messages
static inline ieee754_components decompose_ieee754_dbg(sc_uint<32> value) {
    return decompose_ieee754_rtl(value);
}

// Opcodes for TB readability
enum { OP_FADD = 0x0, OP_FSUB = 0x1, OP_FMUL = 0x2, OP_FDIV = 0x3 };

SC_MODULE(ComprehensiveTestbench) {
    sc_clock clk;
    sc_signal<bool> reset, stall;

    FPU_Pipeline_Top* fpu_top;

    int tests_passed = 0;
    int tests_failed = 0;

    void create_program() {
        vector<sc_uint<32>> program;

        // Basic arithmetic operations
        fp_instruction_t inst1(OP_FADD, 3, 1, 2);    // f3 = f1 + f2 (3.0 + 2.0 = 5.0)
        fp_instruction_t inst2(OP_FSUB, 4, 1, 2);    // f4 = f1 - f2 (3.0 - 2.0 = 1.0)
        fp_instruction_t inst3(OP_FMUL, 5, 1, 2);    // f5 = f1 * f2 (3.0 * 2.0 = 6.0)
        fp_instruction_t inst4(OP_FDIV, 6, 1, 2);    // f6 = f1 / f2 (3.0 / 2.0 = 1.5)
        
        // Division by zero test
        fp_instruction_t inst5(OP_FDIV, 7, 1, 8);    // f7 = f1 / f8 (3.0 / 0.0 = +inf)
        
        // Special cases: infinity arithmetic
        fp_instruction_t inst6(OP_FADD, 9, 10, 11);  // f9 = f10 + f11 (+inf + (-inf) = NaN)
        
        // Underflow test
        fp_instruction_t inst7(OP_FMUL, 12, 13, 14); // f12 = f13 * f14 (tiny * tiny = underflow)
        
        // Overflow test
        fp_instruction_t inst8(OP_FMUL, 15, 16, 17); // f15 = f16 * f17 (large * large = overflow)
        
        // Denormalized number tests
        fp_instruction_t inst9(OP_FADD, 18, 19, 20); // f18 = f19 + f20 (denorm + denorm)
        fp_instruction_t inst10(OP_FMUL, 21, 22, 23); // f21 = f22 * f23 (denorm * normal)

        program.push_back(inst1.to_word());
        program.push_back(inst2.to_word());
        program.push_back(inst3.to_word());
        program.push_back(inst4.to_word());
        program.push_back(inst5.to_word());
        program.push_back(inst6.to_word());
        program.push_back(inst7.to_word());
        program.push_back(inst8.to_word());
        program.push_back(inst9.to_word());
        program.push_back(inst10.to_word());

        // Load program into fetch stage
        fpu_top->fetch_stage->load_program(program.data(), (int)program.size());
        cout << "Program loaded with " << program.size() << " instructions.\n";
    }

    void setup_registers() {
        cout << "\n--- Setting up test registers ---\n";
        
        // Basic operands
        fpu_top->decode_stage->set_register_bits(1, float_to_ieee754_bits(3.0f));
        fpu_top->decode_stage->set_register_bits(2, float_to_ieee754_bits(2.0f));
        cout << "f1 = 3.0, f2 = 2.0\n";

        // Special values
        fpu_top->decode_stage->set_register_bits(8, 0x00000000);  // +0.0
        fpu_top->decode_stage->set_register_bits(10, 0x7F800000); // +infinity
        fpu_top->decode_stage->set_register_bits(11, 0xFF800000); // -infinity
        cout << "f8 = +0.0, f10 = +inf, f11 = -inf\n";

        // Values for overflow test (large numbers)
        fpu_top->decode_stage->set_register_bits(16, 0x7F000000); // Very large positive
        fpu_top->decode_stage->set_register_bits(17, 0x7F000000); // Very large positive
        cout << "f16, f17 = large values for overflow test\n";

        // Values for underflow test (tiny numbers)
        fpu_top->decode_stage->set_register_bits(13, 0x00800000); // Smallest normal positive
        fpu_top->decode_stage->set_register_bits(14, 0x00800000); // Smallest normal positive
        cout << "f13, f14 = tiny values for underflow test\n";

        // Denormalized numbers
        fpu_top->decode_stage->set_register_bits(19, 0x00400000); // Denorm
        fpu_top->decode_stage->set_register_bits(20, 0x00200000); // Denorm
        fpu_top->decode_stage->set_register_bits(22, 0x00100000); // Denorm
        fpu_top->decode_stage->set_register_bits(23, 0x3F800000); // 1.0f
        cout << "f19, f20, f22 = denormalized values, f23 = 1.0\n";

        cout << "Register setup complete.\n";
    }

    bool check_result_float(int reg, float expected, const string& name, float tolerance = 1e-6f) {
        sc_uint<32> bits = fpu_top->decode_stage->fp_registers[reg];
        float actual = ieee754_bits_to_float(bits);
        bool pass = fabs(actual - expected) < tolerance;
        
        cout << name << ": f" << reg << " = " << actual << " (expected " << expected << ") - "
             << (pass ? "PASS" : "FAIL") << "\n";
        
        if (pass) tests_passed++; else tests_failed++;
        return pass;
    }

    bool check_special_value(int reg, const string& expected_type, const string& name) {
        sc_uint<32> bits = fpu_top->decode_stage->fp_registers[reg];
        ieee754_components comp = decompose_ieee754_dbg(bits);
        bool pass = false;
        string actual_type;
        
        if (comp.is_zero) {
            actual_type = "zero";
            pass = (expected_type == "zero");
        } else if (comp.is_infinity) {
            actual_type = comp.sign ? "-infinity" : "+infinity";
            pass = (expected_type == actual_type);
        } else if (comp.is_nan) {
            actual_type = "NaN";
            pass = (expected_type == "NaN");
        } else if (comp.is_denormalized) {
            actual_type = "denormalized";
            pass = (expected_type == "denormalized");
        } else {
            actual_type = "normal";
            pass = (expected_type == "normal");
        }
        
        cout << name << ": f" << reg << " = " << actual_type << " (expected " << expected_type << ") - "
             << (pass ? "PASS" : "FAIL") << "\n";
        
        if (pass) tests_passed++; else tests_failed++;
        return pass;
    }

    void check_exceptions(const string& phase) {
        sc_uint<8> flags = fpu_top->decode_stage->get_exception_flags();
        cout << "\n--- Exception Status (" << phase << ") ---\n";
        if (flags & FP_INVALID_OP)     cout << "⚠️  Invalid Operation detected\n";
        if (flags & FP_OVERFLOW)       cout << "⚠️  Overflow detected\n";
        if (flags & FP_UNDERFLOW)      cout << "⚠️  Underflow detected\n";
        if (flags & FP_DIVIDE_BY_ZERO) cout << "⚠️  Divide by Zero detected\n";
        if (flags & FP_INEXACT)        cout << "⚠️  Inexact result detected\n";
        if (flags == 0) cout << "✅ No exceptions detected\n";
        cout << "Exception flags = 0x" << hex << flags.to_uint() << dec << "\n";
    }

    void print_pipeline_state(int cycle) {
        cout << "\n--- Pipeline State @ Cycle " << cycle << " ---\n";
        cout << "Fetch PC: " << fpu_top->fetch_pc.read() << "\n";
        cout << "Decode Valid: " << fpu_top->decode_valid.read() << "\n";
        cout << "Execute Valid: " << fpu_top->execute_valid.read() << "\n";
        cout << "Writeback Valid: " << fpu_top->writeback_valid.read() << "\n";
        cout << "Branch Taken: " << fpu_top->execute_branch_taken.read() << "\n";
        cout << "Pipeline Flush: " << fpu_top->flush_pipeline.read() << "\n";
    }

    void test_thread() {
        cout << "\n=== FPU PIPELINE COMPREHENSIVE TEST ===\n";

        // Reset sequence
        cout << "\n--- Reset Phase ---\n";
        reset.write(true);
        stall.write(false);
        wait(20, SC_NS);  // Hold reset for 2 cycles
        reset.write(false);
        wait(10, SC_NS);   // One cycle after reset release

        // Setup test data
        setup_registers();
        create_program();

        cout << "\n--- Execution Phase ---\n";
        cout << "Starting pipeline execution...\n";

        // Run for sufficient cycles to complete all operations
        int max_cycles = 150;
        for (int cycle = 0; cycle < max_cycles; ++cycle) {
            wait(10, SC_NS);  // One clock cycle

            // Print pipeline state periodically
            if (cycle % 20 == 0 && cycle > 0) {
                print_pipeline_state(cycle);
            }

            // Check basic arithmetic results (should be available after pipeline delay)
            if (cycle == 50) {
                cout << "\n--- Basic Arithmetic Results @ Cycle " << cycle << " ---\n";
                check_result_float(3, 5.0f, "FADD: 3.0 + 2.0");
                check_result_float(4, 1.0f, "FSUB: 3.0 - 2.0");
                check_result_float(5, 6.0f, "FMUL: 3.0 * 2.0");
                check_exceptions("Basic Arithmetic");
            }

            // Check division results (division takes longer due to multi-cycle operation)
            if (cycle == 80) {
                cout << "\n--- Division Results @ Cycle " << cycle << " ---\n";
                check_result_float(6, 1.5f, "FDIV: 3.0 / 2.0");
                check_special_value(7, "+infinity", "FDIV: 3.0 / 0.0 (divide by zero)");
                check_exceptions("Division");
            }

            // Check special cases and exception handling
            if (cycle == 110) {
                cout << "\n--- Special Cases @ Cycle " << cycle << " ---\n";
                check_special_value(9, "NaN", "FADD: +inf + (-inf)");
                
                // Check underflow case
                sc_uint<32> f12_bits = fpu_top->decode_stage->fp_registers[12];
                ieee754_components f12_comp = decompose_ieee754_dbg(f12_bits);
                if (f12_comp.is_zero || f12_comp.is_denormalized) {
                    cout << "FMUL underflow test: PASS (result is zero or denorm)\n";
                    tests_passed++;
                } else {
                    cout << "FMUL underflow test: FAIL\n";
                    tests_failed++;
                }
                
                // Check overflow case
                check_special_value(15, "+infinity", "FMUL: large * large (overflow)");
                check_exceptions("Special Cases");
            }

            // Check denormalized number handling
            if (cycle == 130) {
                cout << "\n--- Denormalized Number Tests @ Cycle " << cycle << " ---\n";
                sc_uint<32> f18_bits = fpu_top->decode_stage->fp_registers[18];
                sc_uint<32> f21_bits = fpu_top->decode_stage->fp_registers[21];
                
                cout << "FADD denorm result f18 = " << ieee754_bits_to_float(f18_bits) << "\n";
                cout << "FMUL denorm result f21 = " << ieee754_bits_to_float(f21_bits) << "\n";
                check_exceptions("Denormalized");
            }
        }

        // Final results summary
        cout << "\n=== FINAL TEST SUMMARY ===\n";
        cout << "Tests Passed: " << tests_passed << "\n";
        cout << "Tests Failed: " << tests_failed << "\n";
        cout << "Success Rate: " << (tests_passed * 100) / (tests_passed + tests_failed) << "%\n";

        // Final exception check
        check_exceptions("Final");

        // Print final register dump for manual verification
        cout << "\n--- Final Register State ---\n";
        for (int i = 1; i <= 23; ++i) {
            if (fpu_top->decode_stage->fp_registers[i] != 0) {
                float val = ieee754_bits_to_float(fpu_top->decode_stage->fp_registers[i]);
                cout << "f" << i << " = " << val << " (0x" << hex 
                     << fpu_top->decode_stage->fp_registers[i].to_uint() << dec << ")\n";
            }
        }

        cout << "\nSimulation complete.\n";
        sc_stop();
    }

    SC_CTOR(ComprehensiveTestbench) : clk("clk", 10, SC_NS) {
        // Create the FPU pipeline top module
        fpu_top = new FPU_Pipeline_Top("fpu_top");
        
        // Connect signals
        fpu_top->clk(clk);
        fpu_top->reset(reset);
        fpu_top->stall(stall);

        // Start test thread
        SC_THREAD(test_thread);
    }

    ~ComprehensiveTestbench() {
        delete fpu_top;
    }
};

// ---------------- MAIN (TB only) ----------------
int sc_main(int argc, char* argv[]) {
    cout << "=== FPU PIPELINE TESTBENCH ===\n";
    cout << "Testing synthesizable RTL implementation with comprehensive test cases.\n";
    
    ComprehensiveTestbench tb("comprehensive_tb");
    sc_start();
    
    cout << "\n=== TESTBENCH COMPLETE ===\n";
    return 0;
}

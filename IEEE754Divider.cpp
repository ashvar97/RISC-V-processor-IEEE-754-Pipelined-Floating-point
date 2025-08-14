#include "systemc.h"

// Stage 1: Extraction and Preprocessing
SC_MODULE(DivisionExtractor) {
    sc_in<sc_uint<32>> a, b;
    sc_in<bool> reset;
    sc_out<sc_uint<32>> a_significand, b_significand;
    sc_out<bool> a_sign, b_sign;
    sc_out<sc_uint<8>> a_exp, b_exp;
    sc_out<bool> is_special;
    sc_out<sc_uint<32>> special_result;
    sc_out<bool> divide_by_zero;
    sc_out<bool> result_is_nan;
    sc_out<bool> result_is_inf;
    sc_out<bool> result_is_zero;

    void extract() {
        if (reset.read()) {
            a_significand.write(0);
            b_significand.write(0);
            a_sign.write(false);
            b_sign.write(false);
            a_exp.write(0);
            b_exp.write(0);
            is_special.write(false);
            special_result.write(0);
            divide_by_zero.write(false);
            result_is_nan.write(false);
            result_is_inf.write(false);
            result_is_zero.write(false);
        } else {
            sc_uint<32> a_val = a.read();
            sc_uint<32> b_val = b.read();
            
            // Extract components
            sc_uint<8> a_exp_val = (a_val >> 23) & 0xFF;
            sc_uint<8> b_exp_val = (b_val >> 23) & 0xFF;
            sc_uint<23> a_mant = a_val & 0x7FFFFF;
            sc_uint<23> b_mant = b_val & 0x7FFFFF;
            bool a_sign_val = (a_val >> 31) & 1;
            bool b_sign_val = (b_val >> 31) & 1;
            
            // Check for special cases
            bool a_is_zero = (a_exp_val == 0) && (a_mant == 0);
            bool b_is_zero = (b_exp_val == 0) && (b_mant == 0);
            bool a_is_inf = (a_exp_val == 0xFF) && (a_mant == 0);
            bool b_is_inf = (b_exp_val == 0xFF) && (b_mant == 0);
            bool a_is_nan = (a_exp_val == 0xFF) && (a_mant != 0);
            bool b_is_nan = (b_exp_val == 0xFF) && (b_mant != 0);
            
            bool special_case = a_is_nan || b_is_nan || a_is_zero || b_is_zero || a_is_inf || b_is_inf;
            
            a_sign.write(a_sign_val);
            b_sign.write(b_sign_val);
            a_exp.write(a_exp_val);
            b_exp.write(b_exp_val);
            
            if (special_case) {
                is_special.write(true);
                sc_uint<32> result = 0;
                bool result_sign = a_sign_val ^ b_sign_val;
                
                if (a_is_nan || b_is_nan || (a_is_zero && b_is_zero) || (a_is_inf && b_is_inf)) {
                    // NaN cases
                    result = 0x7FC00000; // Canonical NaN
                    result_is_nan.write(true);
                    result_is_inf.write(false);
                    result_is_zero.write(false);
                    divide_by_zero.write(false);
                } else if (b_is_zero && !a_is_zero) {
                    // Division by zero -> infinity
                    result = result_sign ? 0xFF800000 : 0x7F800000;
                    result_is_nan.write(false);
                    result_is_inf.write(true);
                    result_is_zero.write(false);
                    divide_by_zero.write(true);
                } else if (a_is_zero || b_is_inf) {
                    // Zero result
                    result = result_sign ? 0x80000000 : 0x00000000;
                    result_is_nan.write(false);
                    result_is_inf.write(false);
                    result_is_zero.write(true);
                    divide_by_zero.write(false);
                } else if (a_is_inf) {
                    // Infinity result
                    result = result_sign ? 0xFF800000 : 0x7F800000;
                    result_is_nan.write(false);
                    result_is_inf.write(true);
                    result_is_zero.write(false);
                    divide_by_zero.write(false);
                }
                
                special_result.write(result);
                a_significand.write(0);
                b_significand.write(0);
            } else {
                is_special.write(false);
                special_result.write(0);
                result_is_nan.write(false);
                result_is_inf.write(false);
                result_is_zero.write(false);
                divide_by_zero.write(false);
                
                // Add implicit leading 1 for normalized numbers
                // Fix: Cast to sc_uint<32> to ensure type consistency
                sc_uint<32> a_sig = (a_exp_val == 0) ? sc_uint<32>(a_mant) : sc_uint<32>(a_mant | 0x800000);
                sc_uint<32> b_sig = (b_exp_val == 0) ? sc_uint<32>(b_mant) : sc_uint<32>(b_mant | 0x800000);
                
                a_significand.write(a_sig);
                b_significand.write(b_sig);
            }
        }
    }

    SC_CTOR(DivisionExtractor) {
        SC_METHOD(extract);
        sensitive << a << b << reset;
    }
};

// Stage 2-26: Division Iterator (25 iterations)
SC_MODULE(DivisionIterator) {
    sc_in<bool> clk;
    sc_in<bool> reset;
    sc_in<bool> valid_in;
    sc_in<sc_uint<32>> dividend_in;
    sc_in<sc_uint<32>> divisor_in;
    sc_in<bool> result_sign_in;
    sc_in<sc_uint<8>> result_exp_in;
    sc_in<bool> is_special_in;
    sc_in<sc_uint<32>> special_result_in;
    
    sc_out<bool> valid_out;
    sc_out<sc_uint<32>> quotient_out;
    sc_out<sc_uint<32>> remainder_out;
    sc_out<bool> result_sign_out;
    sc_out<sc_uint<8>> result_exp_out;
    sc_out<bool> is_special_out;
    sc_out<sc_uint<32>> special_result_out;

    // Pipeline arrays for 25 stages
    sc_signal<bool> stage_valid[26];
    sc_signal<sc_uint<32>> stage_dividend[26];
    sc_signal<sc_uint<32>> stage_divisor[26];
    sc_signal<sc_uint<32>> stage_quotient[26];
    sc_signal<bool> stage_result_sign[26];
    sc_signal<sc_uint<8>> stage_result_exp[26];
    sc_signal<bool> stage_is_special[26];
    sc_signal<sc_uint<32>> stage_special_result[26];

    void iterate() {
        if (reset.read()) {
            for (int i = 0; i < 26; i++) {
                stage_valid[i].write(false);
                stage_dividend[i].write(0);
                stage_divisor[i].write(0);
                stage_quotient[i].write(0);
                stage_result_sign[i].write(false);
                stage_result_exp[i].write(0);
                stage_is_special[i].write(false);
                stage_special_result[i].write(0);
            }
            valid_out.write(false);
            quotient_out.write(0);
            remainder_out.write(0);
            result_sign_out.write(false);
            result_exp_out.write(0);
            is_special_out.write(false);
            special_result_out.write(0);
        } else {
            // Stage 0 (input)
            stage_valid[0].write(valid_in.read());
            stage_result_sign[0].write(result_sign_in.read());
            stage_result_exp[0].write(result_exp_in.read());
            stage_is_special[0].write(is_special_in.read());
            stage_special_result[0].write(special_result_in.read());
            
            if (is_special_in.read()) {
                // Pass through special cases
                stage_dividend[0].write(0);
                stage_divisor[0].write(0);
                stage_quotient[0].write(0);
            } else {
                // Initialize division
                sc_uint<32> dividend = dividend_in.read();
                sc_uint<32> divisor = divisor_in.read();
                
                // Normalize if dividend < divisor
                if (dividend < divisor) {
                    dividend = dividend << 1;
                    stage_result_exp[0].write(result_exp_in.read() - 1);
                }
                
                stage_dividend[0].write(dividend);
                stage_divisor[0].write(divisor);
                stage_quotient[0].write(0);
            }
            
            // Perform one division step per stage (stages 1-25)
            for (int i = 1; i < 26; i++) {
                // Pass through control signals
                stage_valid[i].write(stage_valid[i-1].read());
                stage_result_sign[i].write(stage_result_sign[i-1].read());
                stage_result_exp[i].write(stage_result_exp[i-1].read());
                stage_is_special[i].write(stage_is_special[i-1].read());
                stage_special_result[i].write(stage_special_result[i-1].read());
                
                if (stage_is_special[i-1].read()) {
                    // Pass through special cases
                    stage_dividend[i].write(0);
                    stage_divisor[i].write(0);
                    stage_quotient[i].write(0);
                } else {
                    // One division iteration
                    sc_uint<32> dividend = stage_dividend[i-1].read();
                    sc_uint<32> divisor = stage_divisor[i-1].read();
                    sc_uint<32> quotient = stage_quotient[i-1].read();
                    
                    quotient = quotient << 1;
                    
                    if (dividend >= divisor) {
                        dividend = dividend - divisor;
                        quotient = quotient | 1;
                    }
                    
                    dividend = dividend << 1;
                    
                    stage_dividend[i].write(dividend);
                    stage_divisor[i].write(divisor);
                    stage_quotient[i].write(quotient);
                }
            }
            
            // Output final stage
            valid_out.write(stage_valid[25].read());
            quotient_out.write(stage_quotient[25].read());
            remainder_out.write(stage_dividend[25].read() >> 1); // Adjust remainder
            result_sign_out.write(stage_result_sign[25].read());
            result_exp_out.write(stage_result_exp[25].read());
            is_special_out.write(stage_is_special[25].read());
            special_result_out.write(stage_special_result[25].read());
        }
    }

    SC_CTOR(DivisionIterator) {
        SC_METHOD(iterate);
        sensitive << clk.pos() << reset;
    }
};

// Stage 27: Normalization and Rounding
SC_MODULE(DivisionNormalizer) {
    sc_in<bool> reset;
    sc_in<sc_uint<32>> quotient;
    sc_in<sc_uint<32>> remainder;
    sc_in<bool> result_sign;
    sc_in<sc_uint<8>> result_exp;
    sc_in<bool> is_special;
    sc_in<sc_uint<32>> special_result;
    
    sc_out<sc_uint<32>> result;
    sc_out<bool> overflow;
    sc_out<bool> underflow;

    void normalize() {
        if (reset.read()) {
            result.write(0);
            overflow.write(false);
            underflow.write(false);
        } else {
            if (is_special.read()) {
                // Special case - pass through
                result.write(special_result.read());
                overflow.write(false);
                underflow.write(false);
            } else {
                sc_uint<32> q = quotient.read();
                sc_uint<32> r = remainder.read();
                sc_uint<8> exp = result_exp.read();
                bool sign = result_sign.read();
                
                // Check for sticky bit
                bool sticky = (r != 0);
                
                // Rounding logic
                bool rnd = (q & 0x1000000) != 0; // Bit 24 (guard bit)
                bool odd = (q & 0x2) != 0;       // Bit 1 (odd bit for tie-breaking)
                
                // Handle overflow/underflow cases
                if (exp >= 255) {
                    // Overflow to infinity
                    result.write(sign ? 0xFF800000 : 0x7F800000);
                    overflow.write(true);
                    underflow.write(false);
                } else if (exp == 0 || (exp == 1 && q < 0x1000000)) {
                    // Underflow case - denormalized or zero
                    sc_uint<8> shift = (exp == 0) ? 1 : (1 - exp);
                    if (shift > 25) shift = 25;
                    
                    // Update sticky bit with shifted-out bits
                    if (shift > 0) {
                        sc_uint<32> mask = (1 << shift) - 1;
                        sticky = sticky || ((q & mask) != 0);
                        q = q >> shift;
                        rnd = (q & 0x1000000) != 0;
                    }
                    
                    // Round
                    q = (q >> 1) + (rnd && (sticky || odd));
                    
                    // Create denormalized result
                    sc_uint<32> final_result = q & 0x7FFFFF;
                    final_result |= (sign ? 0x80000000 : 0);
                    
                    result.write(final_result);
                    overflow.write(false);
                    underflow.write(true);
                } else {
                    // Normal case
                    // Round the quotient
                    q = (q >> 1) + (rnd && (sticky || odd));
                    
                    // Check for carry out of mantissa
                    if (q >= 0x1000000) {
                        q = q >> 1;
                        exp = exp + 1;
                        
                        // Check for overflow after rounding
                        if (exp >= 255) {
                            result.write(sign ? 0xFF800000 : 0x7F800000);
                            overflow.write(true);
                            underflow.write(false);
                            return;
                        }
                    }
                    
                    // Construct final result
                    sc_uint<32> final_result = (q & 0x7FFFFF) | (exp << 23);
                    final_result |= (sign ? 0x80000000 : 0);
                    
                    result.write(final_result);
                    overflow.write(false);
                    underflow.write(false);
                }
            }
        }
    }

    SC_CTOR(DivisionNormalizer) {
        SC_METHOD(normalize);
        sensitive << quotient << remainder << result_sign << result_exp 
                 << is_special << special_result << reset;
    }
};

// Top-level pipelined divider
SC_MODULE(ieee754div) {
    sc_in<sc_uint<32>> a, b;
    sc_in<bool> reset;
    sc_in<bool> clk;
    sc_out<sc_uint<32>> result;
    sc_out<bool> valid_out;
    sc_out<bool> overflow;
    sc_out<bool> underflow;
    sc_out<bool> divide_by_zero;

    // Pipeline stage signals
    sc_signal<bool> stage1_valid, stage26_valid;
    
    // Stage 1 outputs
    sc_signal<sc_uint<32>> a_significand_s1, b_significand_s1;
    sc_signal<bool> a_sign_s1, b_sign_s1;
    sc_signal<sc_uint<8>> a_exp_s1, b_exp_s1;
    sc_signal<bool> is_special_s1;
    sc_signal<sc_uint<32>> special_result_s1;
    sc_signal<bool> divide_by_zero_s1;
    sc_signal<bool> result_is_nan_s1, result_is_inf_s1, result_is_zero_s1;
    
    // Stage 1 -> Stage 2 registered
    sc_signal<sc_uint<32>> dividend_s2, divisor_s2;
    sc_signal<bool> result_sign_s2;
    sc_signal<sc_uint<8>> result_exp_s2;
    sc_signal<bool> is_special_s2;
    sc_signal<sc_uint<32>> special_result_s2;
    
    // Stage 26 outputs 
    sc_signal<sc_uint<32>> quotient_s26, remainder_s26;
    sc_signal<bool> result_sign_s26;
    sc_signal<sc_uint<8>> result_exp_s26;
    sc_signal<bool> is_special_s26;
    sc_signal<sc_uint<32>> special_result_s26;

    // Submodules
    DivisionExtractor extract;
    DivisionIterator iterator;
    DivisionNormalizer normalizer;

    SC_CTOR(ieee754div) : 
        extract("extract"), iterator("iterator"), normalizer("normalizer")
    {
        // Stage 1: Extraction
        extract.a(a);
        extract.b(b);
        extract.reset(reset);
        extract.a_significand(a_significand_s1);
        extract.b_significand(b_significand_s1);
        extract.a_sign(a_sign_s1);
        extract.b_sign(b_sign_s1);
        extract.a_exp(a_exp_s1);
        extract.b_exp(b_exp_s1);
        extract.is_special(is_special_s1);
        extract.special_result(special_result_s1);
        extract.divide_by_zero(divide_by_zero_s1);
        extract.result_is_nan(result_is_nan_s1);
        extract.result_is_inf(result_is_inf_s1);
        extract.result_is_zero(result_is_zero_s1);

        // Stages 2-26: Division iteration
        iterator.clk(clk);
        iterator.reset(reset);
        iterator.valid_in(stage1_valid);
        iterator.dividend_in(dividend_s2);
        iterator.divisor_in(divisor_s2);
        iterator.result_sign_in(result_sign_s2);
        iterator.result_exp_in(result_exp_s2);
        iterator.is_special_in(is_special_s2);
        iterator.special_result_in(special_result_s2);
        iterator.valid_out(stage26_valid);
        iterator.quotient_out(quotient_s26);
        iterator.remainder_out(remainder_s26);
        iterator.result_sign_out(result_sign_s26);
        iterator.result_exp_out(result_exp_s26);
        iterator.is_special_out(is_special_s26);
        iterator.special_result_out(special_result_s26);

        // Stage 27: Normalization
        normalizer.reset(reset);
        normalizer.quotient(quotient_s26);
        normalizer.remainder(remainder_s26);
        normalizer.result_sign(result_sign_s26);
        normalizer.result_exp(result_exp_s26);
        normalizer.is_special(is_special_s26);
        normalizer.special_result(special_result_s26);
        normalizer.result(result);
        normalizer.overflow(overflow);
        normalizer.underflow(underflow);

        // Pipeline control
        SC_CTHREAD(pipeline_control, clk.pos());
        reset_signal_is(reset, true);
    }

    void pipeline_control() {
        // Reset pipeline
        stage1_valid = false;
        dividend_s2 = 0;
        divisor_s2 = 0;
        result_sign_s2 = false;
        result_exp_s2 = 0;
        is_special_s2 = false;
        special_result_s2 = 0;
        valid_out = false;
        divide_by_zero = false;
        
        wait();

        while (true) {
            // Stage 1 -> Stage 2 pipeline register
            stage1_valid = true; // Always valid input
            dividend_s2 = a_significand_s1.read();
            divisor_s2 = b_significand_s1.read();
            result_sign_s2 = a_sign_s1.read() ^ b_sign_s1.read();
            
            // Compute result exponent
            if (!is_special_s1.read()) {
                sc_int<10> exp_diff = sc_int<10>(a_exp_s1.read()) - sc_int<10>(b_exp_s1.read()) + 127;
                // Fix: Use sc_uint<8> for all operands in conditional
                result_exp_s2 = (exp_diff < 0) ? sc_uint<8>(0) : ((exp_diff > 255) ? sc_uint<8>(255) : sc_uint<8>(exp_diff));
            } else {
                result_exp_s2 = 0;
            }
            
            is_special_s2 = is_special_s1.read();
            special_result_s2 = special_result_s1.read();
            
            // Output control
            valid_out = stage26_valid.read();
            divide_by_zero = divide_by_zero_s1.read(); // Propagate with appropriate delay
            
            wait();
        }
    }
};

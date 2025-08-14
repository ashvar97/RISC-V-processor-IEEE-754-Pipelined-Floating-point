#include <systemc.h>


// FloatingPointExtractor  (Stage 1)
SC_MODULE(FloatingPointExtractor) {
    sc_in<sc_uint<32>> in;
    sc_in<bool> reset;
    sc_out<bool> sign;
    sc_out<sc_uint<8>> exponent;
    sc_out<sc_uint<24>> mantissa;
    sc_out<bool> is_nan;
    sc_out<bool> is_zero;
    sc_out<bool> is_inf;
    sc_out<bool> is_denorm;

    void extract() {
        if (reset.read()) {
            sign.write(false);
            exponent.write(0);
            mantissa.write(0);
            is_nan.write(false);
            is_zero.write(false);
            is_inf.write(false);
            is_denorm.write(false);
        } else {
            sc_uint<32> val = in.read();
            sc_uint<8> exp = val.range(30, 23);
            sc_uint<23> mant = val.range(22, 0);
            
            sign.write(val[31]);
            exponent.write(exp);
            
            // Handle mantissa based on whether number is normalized or denormalized
            if (exp == 0) {
                // Denormalized number - no implicit leading 1
                mantissa.write(sc_uint<24>(mant));
                is_denorm.write(mant != 0);
                is_zero.write(mant == 0);
            } else {
                // Normalized number - add implicit leading 1
                mantissa.write((sc_uint<24>(1) << 23) | mant);
                is_denorm.write(false);
                is_zero.write(false);
            }
            
            // Special case detection
            is_nan.write(exp == 0xFF && mant != 0);
            is_inf.write(exp == 0xFF && mant == 0);
        }
    }

    SC_CTOR(FloatingPointExtractor) {
        SC_METHOD(extract);
        sensitive << in << reset;
    }
};



// FloatingPointSubtractor Core - Stage 2
SC_MODULE(FloatingPointSubtractor) {
    sc_in<sc_uint<24>> A_Mantissa;
    sc_in<sc_uint<24>> B_Mantissa;
    sc_in<sc_uint<8>> A_Exponent;
    sc_in<sc_uint<8>> B_Exponent;
    sc_in<bool> A_sign;
    sc_in<bool> B_sign;
    sc_in<bool> A_is_nan;
    sc_in<bool> A_is_zero;
    sc_in<bool> A_is_inf;
    sc_in<bool> A_is_denorm;
    sc_in<bool> B_is_nan;
    sc_in<bool> B_is_zero;
    sc_in<bool> B_is_inf;
    sc_in<bool> B_is_denorm;
    sc_in<bool> reset;
    sc_out<sc_uint<25>> Result_Mantissa;
    sc_out<sc_uint<8>> Result_Exponent;
    sc_out<bool> Result_Sign;
    sc_out<bool> result_is_nan;
    sc_out<bool> result_is_inf;
    sc_out<bool> result_is_zero;

    void subtract() {
        if (reset.read()) {
            Result_Mantissa.write(0);
            Result_Exponent.write(0);
            Result_Sign.write(false);
            result_is_nan.write(false);
            result_is_inf.write(false);
            result_is_zero.write(false);
        } else {
            // Handle special cases first
            bool nan_result = A_is_nan.read() || B_is_nan.read() || 
                             (A_is_inf.read() && B_is_inf.read() && A_sign.read() == B_sign.read());
            
            if (nan_result) {
                result_is_nan.write(true);
                result_is_inf.write(false);
                result_is_zero.write(false);
                Result_Mantissa.write(0x800000); // NaN mantissa
                Result_Exponent.write(0xFF);
                Result_Sign.write(false);
                return;
            }
            
            // Handle infinity cases
            if (A_is_inf.read() || B_is_inf.read()) {
                result_is_nan.write(false);
                result_is_inf.write(true);
                result_is_zero.write(false);
                Result_Mantissa.write(0);
                Result_Exponent.write(0xFF);
                // For subtraction, if both are infinity with same sign, it's NaN (handled above)
                // Otherwise result is infinity with sign of the first operand
                Result_Sign.write(A_is_inf.read() ? A_sign.read() : !B_sign.read());
                return;
            }
            
            // Handle zero cases
            if (A_is_zero.read() && B_is_zero.read()) {
                result_is_nan.write(false);
                result_is_inf.write(false);
                result_is_zero.write(true);
                Result_Mantissa.write(0);
                Result_Exponent.write(0);
                // -0 - (-0) = +0, otherwise sign depends on signs
                Result_Sign.write(A_sign.read() && !B_sign.read());
                return;
            } else if (A_is_zero.read()) {
                result_is_nan.write(false);
                result_is_inf.write(false);
                result_is_zero.write(B_is_zero.read());
                Result_Mantissa.write(B_Mantissa.read());
                Result_Exponent.write(B_Exponent.read());
                // 0 - B = -B
                Result_Sign.write(!B_sign.read());
                return;
            } else if (B_is_zero.read()) {
                result_is_nan.write(false);
                result_is_inf.write(false);
                result_is_zero.write(A_is_zero.read());
                Result_Mantissa.write(A_Mantissa.read());
                Result_Exponent.write(A_Exponent.read());
                Result_Sign.write(A_sign.read());
                return;
            }
            
            // Normal subtraction case
            result_is_nan.write(false);
            result_is_inf.write(false);
            
            sc_uint<8> exp_a = A_Exponent.read();
            sc_uint<8> exp_b = B_Exponent.read();
            sc_uint<24> mant_a = A_Mantissa.read();
            sc_uint<24> mant_b = B_Mantissa.read();
            bool sign_a = A_sign.read();
            bool sign_b = !B_sign.read(); // Invert sign for subtraction
            
            // Handle denormalized numbers
            if (A_is_denorm.read()) exp_a = 1; // Treat as if exponent is 1
            if (B_is_denorm.read()) exp_b = 1; // Treat as if exponent is 1
            
            // Align mantissas by shifting the smaller exponent
            sc_uint<8> exp_diff;
            sc_uint<25> aligned_mant_a, aligned_mant_b;
            sc_uint<8> result_exp;
            
            if (exp_a >= exp_b) {
                exp_diff = exp_a - exp_b;
                result_exp = exp_a;
                aligned_mant_a = mant_a;
                if (exp_diff < 24) {
                    aligned_mant_b = mant_b >> exp_diff;
                } else {
                    aligned_mant_b = 0; // Complete underflow
                }
            } else {
                exp_diff = exp_b - exp_a;
                result_exp = exp_b;
                aligned_mant_b = mant_b;
                if (exp_diff < 24) {
                    aligned_mant_a = mant_a >> exp_diff;
                } else {
                    aligned_mant_a = 0; // Complete underflow
                }
            }
            
            // Perform subtraction or addition based on sign
            sc_uint<25> result_mant;
            bool result_sign;
            
            if (sign_a == sign_b) {
                // Same sign - addition
                result_mant = aligned_mant_a + aligned_mant_b;
                result_sign = sign_a;
            } else {
                // Different signs - subtraction
                if (aligned_mant_a >= aligned_mant_b) {
                    result_mant = aligned_mant_a - aligned_mant_b;
                    result_sign = sign_a;
                } else {
                    result_mant = aligned_mant_b - aligned_mant_a;
                    result_sign = sign_b;
                }
            }
            
            // Check for zero result
            if (result_mant == 0) {
                result_is_zero.write(true);
                Result_Mantissa.write(0);
                Result_Exponent.write(0);
                Result_Sign.write(false);
            } else {
                result_is_zero.write(false);
                Result_Mantissa.write(result_mant);
                Result_Exponent.write(result_exp);
                Result_Sign.write(result_sign);
            }
        }
    }

    SC_CTOR(FloatingPointSubtractor) {
        SC_METHOD(subtract);
        sensitive << A_Mantissa << B_Mantissa << A_Exponent << B_Exponent 
                 << A_sign << B_sign << A_is_nan << A_is_zero << A_is_inf << A_is_denorm
                 << B_is_nan << B_is_zero << B_is_inf << B_is_denorm << reset;
    }
};


// FloatingPointNormalizer - Stage 3 (Modified for addition)
SC_MODULE(FloatingPointNormalizer) {
    sc_in<sc_uint<25>> Result_Mantissa;
    sc_in<sc_uint<8>> Result_Exponent;
    sc_in<bool> Result_Sign;
    sc_in<bool> result_is_nan;
    sc_in<bool> result_is_inf;
    sc_in<bool> result_is_zero;
    sc_in<bool> reset;
    sc_out<sc_uint<32>> result;
    sc_out<bool> overflow;
    sc_out<bool> underflow;

    void normalize() {
        if (reset.read()) {
            result.write(0);
            overflow.write(false);
            underflow.write(false);
        } else {
            // Handle special cases first
            if (result_is_nan.read()) {
                result.write(0x7FC00000);
                overflow.write(false);
                underflow.write(false);
                return;
            }
            
            if (result_is_zero.read()) {
                sc_uint<32> zero_result = Result_Sign.read() ? 0x80000000 : 0x00000000;
                result.write(zero_result);
                overflow.write(false);
                underflow.write(false);
                return;
            }
            
            if (result_is_inf.read()) {
                sc_uint<32> inf_result = Result_Sign.read() ? 0xFF800000 : 0x7F800000;
                result.write(inf_result);
                overflow.write(true);
                underflow.write(false);
                return;
            }
            
            // Normal case normalization
            sc_uint<25> temp_mant = Result_Mantissa.read();
            sc_int<10> temp_exp = (sc_int<10>)Result_Exponent.read(); // Use signed for proper underflow handling
            
            // Handle zero result
            if (temp_mant == 0) {
                result.write(Result_Sign.read() ? 0x80000000 : 0x00000000);
                overflow.write(false);
                underflow.write(false);
                return;
            }
            
            // Normalize the mantissa
            sc_uint<25> normalized_mant = temp_mant;
            sc_int<10> adjusted_exp = temp_exp;
            
            // Check if we need to shift right (overflow in addition)
            if (normalized_mant[24] == 1) {
                // Shift right by 1, increment exponent
                normalized_mant = normalized_mant >> 1;
                adjusted_exp = adjusted_exp + 1;
            } else if (normalized_mant[23] == 0) {
                // Need to shift left to normalize
                sc_uint<8> shift_count = 0;
                for (int i = 22; i >= 0; i--) {
                    if (normalized_mant[i] == 1) {
                        shift_count = 23 - i;
                        break;
                    }
                }
                normalized_mant = normalized_mant << shift_count;
                adjusted_exp = adjusted_exp - shift_count;
            }
            
            // Extract the final mantissa (remove implicit leading 1)
            sc_uint<23> final_mant = normalized_mant.range(22, 0);
            
            // Handle overflow/underflow
            if (adjusted_exp >= 255) {
                // Overflow to infinity
                result.write(Result_Sign.read() ? 0xFF800000 : 0x7F800000);
                overflow.write(true);
                underflow.write(false);
            } else if (adjusted_exp <= 0) {
                if (adjusted_exp <= -23) {
                    // Complete underflow to zero
                    result.write(Result_Sign.read() ? 0x80000000 : 0x00000000);
                    underflow.write(true);
                    overflow.write(false);
                } else {
                    // Denormalized result
                    sc_uint<24> denorm_mant_24 = (sc_uint<24>(1) << 23) | final_mant;
                    sc_uint<23> denorm_mant = denorm_mant_24 >> (1 - adjusted_exp);
                    sc_uint<32> denorm_result = (sc_uint<32>(Result_Sign.read()) << 31) | denorm_mant;
                    result.write(denorm_result);
                    underflow.write(true);
                    overflow.write(false);
                }
            } else {
                // Normal result
                sc_uint<8> final_exp = sc_uint<8>(adjusted_exp);
                sc_uint<32> final_result = (sc_uint<32>(Result_Sign.read()) << 31) | 
                                         (sc_uint<32>(final_exp) << 23) | 
                                         sc_uint<32>(final_mant);
                result.write(final_result);
                overflow.write(false);
                underflow.write(false);
            }
        }
    }

    SC_CTOR(FloatingPointNormalizer) {
        SC_METHOD(normalize);
        sensitive << Result_Mantissa << Result_Exponent << Result_Sign 
                 << result_is_nan << result_is_inf << result_is_zero << reset;
    }
};
              
              
// Pipelined top-level subtractor
SC_MODULE(ieee754_subtractor) {
    sc_in<sc_uint<32>> A;
    sc_in<sc_uint<32>> B;
    sc_in<bool> reset;
    sc_in<bool> clk;
    sc_out<sc_uint<32>> result;
    sc_out<bool> valid_out;
    sc_out<bool> overflow;
    sc_out<bool> underflow;

    // Pipeline registers between stages
    sc_signal<bool> A_sign_reg, B_sign_reg;
    sc_signal<sc_uint<8>> A_Exponent_reg, B_Exponent_reg;
    sc_signal<sc_uint<24>> A_Mantissa_reg, B_Mantissa_reg;
    sc_signal<bool> A_is_nan_reg, A_is_zero_reg, A_is_inf_reg, A_is_denorm_reg;
    sc_signal<bool> B_is_nan_reg, B_is_zero_reg, B_is_inf_reg, B_is_denorm_reg;

    sc_signal<bool> Result_Sign_reg;
    sc_signal<sc_uint<8>> Result_Exponent_reg;
    sc_signal<sc_uint<25>> Result_Mantissa_reg;
    sc_signal<bool> result_is_nan_reg, result_is_inf_reg, result_is_zero_reg;

    // Stage outputs (combinational)
    sc_signal<bool> A_sign_comb, B_sign_comb;
    sc_signal<sc_uint<8>> A_Exponent_comb, B_Exponent_comb;
    sc_signal<sc_uint<24>> A_Mantissa_comb, B_Mantissa_comb;
    sc_signal<bool> A_is_nan_comb, A_is_zero_comb, A_is_inf_comb, A_is_denorm_comb;
    sc_signal<bool> B_is_nan_comb, B_is_zero_comb, B_is_inf_comb, B_is_denorm_comb;

    sc_signal<bool> Result_Sign_comb;
    sc_signal<sc_uint<8>> Result_Exponent_comb;
    sc_signal<sc_uint<25>> Result_Mantissa_comb;
    sc_signal<bool> result_is_nan_comb, result_is_inf_comb, result_is_zero_comb;

    // Valid pipeline tracking
    sc_signal<bool> valid_stage1, valid_stage2, valid_stage3;

    // Submodule instances
    FloatingPointExtractor extractA;
    FloatingPointExtractor extractB;
    FloatingPointSubtractor sub;
    FloatingPointNormalizer normalize;

    SC_CTOR(ieee754_subtractor) : 
        extractA("extractA"), extractB("extractB"), 
        sub("sub"), normalize("normalize") 
    {
        // Stage 1: Extraction (combinational outputs)
        extractA.in(A);
        extractA.reset(reset);
        extractA.sign(A_sign_comb);
        extractA.exponent(A_Exponent_comb);
        extractA.mantissa(A_Mantissa_comb);
        extractA.is_nan(A_is_nan_comb);
        extractA.is_zero(A_is_zero_comb);
        extractA.is_inf(A_is_inf_comb);
        extractA.is_denorm(A_is_denorm_comb);

        extractB.in(B);
        extractB.reset(reset);
        extractB.sign(B_sign_comb);
        extractB.exponent(B_Exponent_comb);
        extractB.mantissa(B_Mantissa_comb);
        extractB.is_nan(B_is_nan_comb);
        extractB.is_zero(B_is_zero_comb);
        extractB.is_inf(B_is_inf_comb);
        extractB.is_denorm(B_is_denorm_comb);

        // Stage 2: Subtraction (uses registered stage 1 outputs)
        sub.A_Mantissa(A_Mantissa_reg);
        sub.B_Mantissa(B_Mantissa_reg);
        sub.A_Exponent(A_Exponent_reg);
        sub.B_Exponent(B_Exponent_reg);
        sub.A_sign(A_sign_reg);
        sub.B_sign(B_sign_reg);
        sub.A_is_nan(A_is_nan_reg);
        sub.A_is_zero(A_is_zero_reg);
        sub.A_is_inf(A_is_inf_reg);
        sub.A_is_denorm(A_is_denorm_reg);
        sub.B_is_nan(B_is_nan_reg);
        sub.B_is_zero(B_is_zero_reg);
        sub.B_is_inf(B_is_inf_reg);
        sub.B_is_denorm(B_is_denorm_reg);
        sub.reset(reset);
        sub.Result_Mantissa(Result_Mantissa_comb);
        sub.Result_Exponent(Result_Exponent_comb);
        sub.Result_Sign(Result_Sign_comb);
        sub.result_is_nan(result_is_nan_comb);
        sub.result_is_inf(result_is_inf_comb);
        sub.result_is_zero(result_is_zero_comb);

        // Stage 3: Normalization (uses registered stage 2 outputs)
        normalize.Result_Mantissa(Result_Mantissa_reg);
        normalize.Result_Exponent(Result_Exponent_reg);
        normalize.Result_Sign(Result_Sign_reg);
        normalize.result_is_nan(result_is_nan_reg);
        normalize.result_is_inf(result_is_inf_reg);
        normalize.result_is_zero(result_is_zero_reg);
        normalize.reset(reset);
        normalize.result(result);
        normalize.overflow(overflow);
        normalize.underflow(underflow);

        // Pipeline control process
        SC_CTHREAD(pipeline_control, clk.pos());
        reset_signal_is(reset, true);
    }

    void pipeline_control() {
        // Reset all pipeline registers
        A_sign_reg = false;
        B_sign_reg = false;
        A_Exponent_reg = 0;
        B_Exponent_reg = 0;
        A_Mantissa_reg = 0;
        B_Mantissa_reg = 0;
        A_is_nan_reg = false;
        A_is_zero_reg = false;
        A_is_inf_reg = false;
        A_is_denorm_reg = false;
        B_is_nan_reg = false;
        B_is_zero_reg = false;
        B_is_inf_reg = false;
        B_is_denorm_reg = false;
        
        Result_Sign_reg = false;
        Result_Exponent_reg = 0;
        Result_Mantissa_reg = 0;
        result_is_nan_reg = false;
        result_is_inf_reg = false;
        result_is_zero_reg = false;
        
        valid_stage1 = false;
        valid_stage2 = false;
        valid_stage3 = false;
        valid_out = false;
        
        wait();

        while (true) {
            // Stage 1 -> Stage 2 pipeline register
            A_sign_reg = A_sign_comb.read();
            B_sign_reg = B_sign_comb.read();
            A_Exponent_reg = A_Exponent_comb.read();
            B_Exponent_reg = B_Exponent_comb.read();
            A_Mantissa_reg = A_Mantissa_comb.read();
            B_Mantissa_reg = B_Mantissa_comb.read();
            A_is_nan_reg = A_is_nan_comb.read();
            A_is_zero_reg = A_is_zero_comb.read();
            A_is_inf_reg = A_is_inf_comb.read();
            A_is_denorm_reg = A_is_denorm_comb.read();
            B_is_nan_reg = B_is_nan_comb.read();
            B_is_zero_reg = B_is_zero_comb.read();
            B_is_inf_reg = B_is_inf_comb.read();
            B_is_denorm_reg = B_is_denorm_comb.read();
            
            // Stage 2 -> Stage 3 pipeline register
            Result_Sign_reg = Result_Sign_comb.read();
            Result_Exponent_reg = Result_Exponent_comb.read();
            Result_Mantissa_reg = Result_Mantissa_comb.read();
            result_is_nan_reg = result_is_nan_comb.read();
            result_is_inf_reg = result_is_inf_comb.read();
            result_is_zero_reg = result_is_zero_comb.read();
            
            // Update valid signals (pipeline progression)
            valid_stage1 = true;
            valid_stage2 = valid_stage1;
            valid_stage3 = valid_stage2;
            valid_out = valid_stage3;  // Result is valid after 3 clock cycles
            
            wait();
        }
    }
};

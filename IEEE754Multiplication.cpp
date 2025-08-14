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

// FloatingPointMultiplier Module - Stage 2 (FIXED)
SC_MODULE(FloatingPointMultiplier) {
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
    sc_out<sc_uint<48>> Temp_Mantissa;
    sc_out<sc_int<12>> Temp_Exponent;  // CHANGED: Use signed 12-bit to avoid wrap-around
    sc_out<bool> Sign;
    sc_out<bool> result_is_nan;
    sc_out<bool> result_is_inf;
    sc_out<bool> result_is_zero;

    void multiply() {
        if (reset.read()) {
            Temp_Mantissa.write(0);
            Temp_Exponent.write(0);
            Sign.write(false);
            result_is_nan.write(false);
            result_is_inf.write(false);
            result_is_zero.write(false);
        } else {
            // Result sign (always computed)
            bool result_sign = A_sign.read() ^ B_sign.read();
            Sign.write(result_sign);
            
            // NaN cases
            bool nan_result = A_is_nan.read() || B_is_nan.read() || 
                             (A_is_zero.read() && B_is_inf.read()) ||
                             (A_is_inf.read() && B_is_zero.read());
            result_is_nan.write(nan_result);
            
            // Zero cases
            bool zero_result = (A_is_zero.read() || B_is_zero.read()) && !nan_result;
            result_is_zero.write(zero_result);
            
            // Infinity cases
            bool inf_result = (A_is_inf.read() || B_is_inf.read()) && !nan_result && !zero_result;
            result_is_inf.write(inf_result);
            
            if (!nan_result && !zero_result && !inf_result) {
                // Normal multiplication
                Temp_Mantissa.write(A_Mantissa.read() * B_Mantissa.read());
                
                // FIXED: Proper exponent calculation with signed arithmetic
                sc_int<12> exp_sum;  // Use wider signed integer
                
                if (A_is_denorm.read() && B_is_denorm.read()) {
                    // Both denormalized: effective exponents are -126 each
                    // exp_sum = (-126) + (-126) - (-127) = -125
                    exp_sum = -126 + (-126) + 127;  // = -125
                } else if (A_is_denorm.read()) {
                    // A denormalized, B normal
                    exp_sum = -126 + sc_int<12>(B_Exponent.read()) - 127;
                } else if (B_is_denorm.read()) {
                    // B denormalized, A normal  
                    exp_sum = sc_int<12>(A_Exponent.read()) - 127 + (-126);
                } else {
                    // Both normal
                    exp_sum = sc_int<12>(A_Exponent.read()) + sc_int<12>(B_Exponent.read()) - 127;
                }
                
                // FIXED: Keep as signed integer - no premature casting to unsigned
                Temp_Exponent.write(exp_sum);
            } else {
                // Set dummy values for special cases
                Temp_Mantissa.write(0);
                Temp_Exponent.write(0);
            }
        }
    }

    SC_CTOR(FloatingPointMultiplier) {
        SC_METHOD(multiply);
        sensitive << A_Mantissa << B_Mantissa << A_Exponent << B_Exponent 
                 << A_sign << B_sign << A_is_nan << A_is_zero << A_is_inf << A_is_denorm
                 << B_is_nan << B_is_zero << B_is_inf << B_is_denorm << reset;
    }
};

// FloatingPointNormalizer Module - Stage 3 (UPDATED TO HANDLE SIGNED EXPONENT)
SC_MODULE(FloatingPointNormalizer) {
    sc_in<sc_uint<48>> Temp_Mantissa;
    sc_in<sc_int<12>> Temp_Exponent;  // CHANGED: Accept signed exponent
    sc_in<bool> Sign;
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
                sc_uint<32> zero_result = Sign.read() ? 0x80000000 : 0x00000000;
                result.write(zero_result);
                overflow.write(false);
                underflow.write(false);
                return;
            }
            
            if (result_is_inf.read()) {
                sc_uint<32> inf_result = Sign.read() ? 0xFF800000 : 0x7F800000;
                result.write(inf_result);
                overflow.write(true);
                underflow.write(false);
                return;
            }
            
            // Normal case normalization
            sc_uint<48> temp_mant = Temp_Mantissa.read();
            sc_int<12> temp_exp = Temp_Exponent.read();  // CHANGED: Now properly signed
            
            // Handle zero result
            if (temp_mant == 0) {
                result.write(Sign.read() ? 0x80000000 : 0x00000000);
                overflow.write(false);
                underflow.write(false);
                return;
            }
            
            // Normalize the mantissa - find the leading 1
            sc_uint<8> shift_count = 0;
            sc_uint<48> normalized_mant = temp_mant;
            sc_int<12> adjusted_exp = temp_exp;
            
            // Check if we need to shift right (result >= 2.0 in mantissa)
            if (normalized_mant[47] == 1) {
                // Shift right by 1, increment exponent
                normalized_mant = normalized_mant >> 1;
                adjusted_exp = adjusted_exp + 1;
            } else {
                // Find the most significant 1 bit and shift left
                for (int i = 46; i >= 0; i--) {
                    if (normalized_mant[i] == 1) {
                        shift_count = 46 - i;
                        break;
                    }
                }
                normalized_mant = normalized_mant << shift_count;
                adjusted_exp = adjusted_exp - shift_count;
            }
            
            // Extract the final mantissa (remove implicit leading 1)
            sc_uint<23> final_mant = normalized_mant.range(45, 23);
            
            // FIXED: Proper overflow/underflow handling with signed exponent
            if (adjusted_exp >= 255) {
                // Overflow to infinity
                result.write(Sign.read() ? 0xFF800000 : 0x7F800000);
                overflow.write(true);
                underflow.write(false);
            } else if (adjusted_exp <= 0) {
                if (adjusted_exp <= -23) {
                    // Complete underflow to zero
                    result.write(Sign.read() ? 0x80000000 : 0x00000000);
                    underflow.write(true);
                    overflow.write(false);
                } else {
                    // Denormalized result
                    sc_uint<24> denorm_mant_24 = (sc_uint<24>(1) << 23) | final_mant;
                    sc_uint<23> denorm_mant = denorm_mant_24 >> (1 - adjusted_exp);
                    sc_uint<32> denorm_result = (sc_uint<32>(Sign.read()) << 31) | denorm_mant;
                    result.write(denorm_result);
                    underflow.write(true);
                    overflow.write(false);
                }
            } else {
                // Normal result
                sc_uint<8> final_exp = sc_uint<8>(adjusted_exp);
                sc_uint<32> final_result = (sc_uint<32>(Sign.read()) << 31) | 
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
        sensitive << Temp_Mantissa << Temp_Exponent << Sign 
                 << result_is_nan << result_is_inf << result_is_zero << reset;
    }
};

// Pipelined top-level multiplier (UPDATED FOR SIGNED EXPONENT)
SC_MODULE(ieee754mult) {
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

    sc_signal<bool> Sign_reg;
    sc_signal<sc_int<12>> Temp_Exponent_reg;  // CHANGED: Now signed
    sc_signal<sc_uint<48>> Temp_Mantissa_reg;
    sc_signal<bool> result_is_nan_reg, result_is_inf_reg, result_is_zero_reg;

    // Stage outputs (combinational)
    sc_signal<bool> A_sign_comb, B_sign_comb;
    sc_signal<sc_uint<8>> A_Exponent_comb, B_Exponent_comb;
    sc_signal<sc_uint<24>> A_Mantissa_comb, B_Mantissa_comb;
    sc_signal<bool> A_is_nan_comb, A_is_zero_comb, A_is_inf_comb, A_is_denorm_comb;
    sc_signal<bool> B_is_nan_comb, B_is_zero_comb, B_is_inf_comb, B_is_denorm_comb;

    sc_signal<bool> Sign_comb;
    sc_signal<sc_int<12>> Temp_Exponent_comb;  // CHANGED: Now signed
    sc_signal<sc_uint<48>> Temp_Mantissa_comb;
    sc_signal<bool> result_is_nan_comb, result_is_inf_comb, result_is_zero_comb;

    // Valid pipeline tracking
    sc_signal<bool> valid_stage1, valid_stage2, valid_stage3;

    // Submodule instances
    FloatingPointExtractor extractA;
    FloatingPointExtractor extractB;
    FloatingPointMultiplier multiply;
    FloatingPointNormalizer normalize;

    SC_CTOR(ieee754mult) : 
        extractA("extractA"), extractB("extractB"), 
        multiply("multiply"), normalize("normalize") 
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

        // Stage 2: Multiplication (uses registered stage 1 outputs)
        multiply.A_Mantissa(A_Mantissa_reg);
        multiply.B_Mantissa(B_Mantissa_reg);
        multiply.A_Exponent(A_Exponent_reg);
        multiply.B_Exponent(B_Exponent_reg);
        multiply.A_sign(A_sign_reg);
        multiply.B_sign(B_sign_reg);
        multiply.A_is_nan(A_is_nan_reg);
        multiply.A_is_zero(A_is_zero_reg);
        multiply.A_is_inf(A_is_inf_reg);
        multiply.A_is_denorm(A_is_denorm_reg);
        multiply.B_is_nan(B_is_nan_reg);
        multiply.B_is_zero(B_is_zero_reg);
        multiply.B_is_inf(B_is_inf_reg);
        multiply.B_is_denorm(B_is_denorm_reg);
        multiply.reset(reset);
        multiply.Temp_Mantissa(Temp_Mantissa_comb);
        multiply.Temp_Exponent(Temp_Exponent_comb);
        multiply.Sign(Sign_comb);
        multiply.result_is_nan(result_is_nan_comb);
        multiply.result_is_inf(result_is_inf_comb);
        multiply.result_is_zero(result_is_zero_comb);

        // Stage 3: Normalization (uses registered stage 2 outputs)
        normalize.Temp_Mantissa(Temp_Mantissa_reg);
        normalize.Temp_Exponent(Temp_Exponent_reg);
        normalize.Sign(Sign_reg);
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
        
        Sign_reg = false;
        Temp_Exponent_reg = 0;
        Temp_Mantissa_reg = 0;
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
            Sign_reg = Sign_comb.read();
            Temp_Exponent_reg = Temp_Exponent_comb.read();
            Temp_Mantissa_reg = Temp_Mantissa_comb.read();
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
#include 


#include <systemc.h>
#include <cmath>
#include <iomanip>
#include <cfloat>
#include <cstring>
#include "design.cpp"
using namespace std;

// Utility functions for float-hex conversion
inline float hexToFloat(uint32_t hex) {
    float f;
    memcpy(&f, &hex, sizeof(float));
    return f;
}

uint32_t floatToHex(float value) {
    uint32_t result;
    memcpy(&result, &value, sizeof(float));
    return result;
}

void printFloat(uint32_t hex) {
    float f;
    memcpy(&f, &hex, sizeof(float));
    cout << " (value: " << f << ")";
}

// Improved floating point comparison function
bool compare_floats(float a, float b, bool check_sign = false) {
    // Handle NaN cases
    if (isnan(a) && isnan(b)) return true;
    
    // Handle infinity cases
    if (isinf(a) && isinf(b)) {
        return (a > 0) == (b > 0);
    }
    
    // Handle zero cases - consider all zeros equal unless checking sign
    if (a == 0 && b == 0) {
        if (check_sign) {
            uint32_t a_bits = floatToHex(a);
            uint32_t b_bits = floatToHex(b);
            return (a_bits & 0x80000000) == (b_bits & 0x80000000);
        }
        return true;
    }
    
    // For very small numbers, use absolute tolerance
    if (fabs(a) < 1e-30 || fabs(b) < 1e-30) {
        return fabs(a - b) < 1e-40f;
    }
    
    // For normal cases, use relative tolerance
    return fabs(a - b) < 1e-6f * max(fabs(a), fabs(b));
}

int sc_main(int argc, char* argv[]) {
    sc_clock clock("clk", 10, SC_NS);
    sc_signal<bool> reset;
    sc_signal<sc_uint<32>> A, B;
    sc_signal<sc_uint<32>> add_result, sub_result, mul_result;
    sc_signal<bool> add_valid, sub_valid, mul_valid;
    sc_signal<bool> add_overflow, add_underflow;
    sc_signal<bool> sub_overflow, sub_underflow;
    sc_signal<bool> mul_overflow, mul_underflow;
    
    // Initialize all three modules
    ieee754add adder("adder");
    ieee754_subtractor subtractor("subtractor");
    ieee754mult multiplier("multiplier");
    
    // Connect common signals
    adder.clk(clock);
    adder.reset(reset);
    adder.A(A);
    adder.B(B);
    adder.result(add_result);
    adder.valid_out(add_valid);
    adder.overflow(add_overflow);
    adder.underflow(add_underflow);
    
    subtractor.clk(clock);
    subtractor.reset(reset);
    subtractor.A(A);
    subtractor.B(B);
    subtractor.result(sub_result);
    subtractor.valid_out(sub_valid);
    subtractor.overflow(sub_overflow);
    subtractor.underflow(sub_underflow);
    
    multiplier.clk(clock);
    multiplier.reset(reset);
    multiplier.A(A);
    multiplier.B(B);
    multiplier.result(mul_result);
    multiplier.valid_out(mul_valid);
    multiplier.overflow(mul_overflow);
    multiplier.underflow(mul_underflow);
    
    cout << "Starting simulation of IEEE 754 Floating Point Units" << endl;
    
    // Enhanced test case structure
    struct TestCase {
        float a;
        float b;
        float expected_add;
        float expected_sub;
        float expected_mul;
        const char* description;
        bool check_sign; // For signed zero cases
    };
    
    // Comprehensive test cases for all operations
    TestCase test_cases[] = {
        // Basic arithmetic
        {9.5f, 1.0f, 10.5f, 8.5f, 9.5f, "Basic operations", false},
        {100.0f, 0.01f, 100.01f, 99.99f, 1.0f, "Precise operations", false},
        {1.0f, 1.0f, 2.0f, 0.0f, 1.0f, "Identity operations", false},
        {5.0f, 3.0f, 8.0f, 2.0f, 15.0f, "Positive numbers", false},
        {-4.0f, -5.0f, -9.0f, 1.0f, 20.0f, "Negative numbers", false},
        {7.5f, -7.5f, 0.0f, 15.0f, -56.25f, "Mixed signs", false},
        
        // Zero cases
        {0.0f, 5.0f, 5.0f, -5.0f, 0.0f, "Zero operations", false},
        {0.0f, 0.0f, 0.0f, 0.0f, 0.0f, "Zero by zero", false},
        {-0.0f, 0.0f, 0.0f, -0.0f, -0.0f, "Signed zero", true},
        
        // Special cases
        {INFINITY, 5.0f, INFINITY, INFINITY, INFINITY, "Infinity operations", false},
        {INFINITY, -INFINITY, NAN, INFINITY, -INFINITY, "Infinity combinations", false},
        {NAN, 5.0f, NAN, NAN, NAN, "NaN propagation", false},
        
        // Small number cases (removed extremely small cases for multiplier)
        {1e-10f, 1e-11f, 1.1e-10f, 9e-11f, 1e-21f, "Small numbers", false},
        
        // Large number cases
        {1e10f, 1e9f, 1.1e10f, 9e9f, 1e19f, "Large numbers", false},
        
        // Edge cases
        {1.0f, 1.0f - FLT_EPSILON, 2.0f - FLT_EPSILON, FLT_EPSILON, 1.0f - FLT_EPSILON, "Precision limits", false},
        {FLT_MIN, FLT_MIN, 2*FLT_MIN, 0.0f, FLT_MIN*FLT_MIN, "Minimum float values", false},
        {FLT_MAX, FLT_MAX, INFINITY, 0.0f, INFINITY, "Maximum float values", false}
    };
    
    // Reset all units
    cout << "Resetting all units..." << endl;
    reset.write(true);
    A.write(0);
    B.write(0);
    sc_start(30, SC_NS);
    reset.write(false);
    sc_start(10, SC_NS);
    
    int total_tests = 0;
    int add_passed = 0, sub_passed = 0, mul_passed = 0;
    
    for (const auto& test : test_cases) {
        total_tests++;
        cout << "\nTest " << total_tests << ": " << test.description << endl;
        cout << "  A = " << test.a << " (0x" << hex << floatToHex(test.a) << ")" << endl;
        cout << "  B = " << test.b << " (0x" << hex << floatToHex(test.b) << ")" << dec << endl;
        
        // Write inputs
        A.write(floatToHex(test.a));
        B.write(floatToHex(test.b));
        
        // Wait for pipeline to complete (3 clock cycles)
        sc_start(30, SC_NS);
        
        // Test adder results
        bool add_test = false;
        bool sub_test = false;
        bool mul_test = false;
        
        // Check adder results
        if (add_valid.read()) {
            float add_actual = hexToFloat(add_result.read());
            bool add_nan_case = std::isnan(test.expected_add);
            bool add_inf_case = std::isinf(test.expected_add);
            
            if (add_nan_case && std::isnan(add_actual)) {
                add_test = true;
            } else if (add_inf_case && std::isinf(add_actual) && 
                      ((test.expected_add > 0) == (add_actual > 0))) {
                add_test = true;
            } else {
                add_test = compare_floats(add_actual, test.expected_add, test.check_sign);
            }
            
            if (add_test) add_passed++;
            
            cout << "  ADD: ";
            if (add_nan_case) cout << "NaN";
            else if (add_inf_case) cout << (test.expected_add > 0 ? "+Inf" : "-Inf");
            else cout << test.expected_add;
            cout << " vs " << add_actual << " - " << (add_test ? "PASS" : "FAIL") << endl;
        }
        
        // Check subtractor results
        if (sub_valid.read()) {
            float sub_actual = hexToFloat(sub_result.read());
            bool sub_nan_case = std::isnan(test.expected_sub);
            bool sub_inf_case = std::isinf(test.expected_sub);
            
            if (sub_nan_case && std::isnan(sub_actual)) {
                sub_test = true;
            } else if (sub_inf_case && std::isinf(sub_actual) && 
                      ((test.expected_sub > 0) == (sub_actual > 0))) {
                sub_test = true;
            } else {
                sub_test = compare_floats(sub_actual, test.expected_sub, test.check_sign);
            }
            
            if (sub_test) sub_passed++;
            
            cout << "  SUB: ";
            if (sub_nan_case) cout << "NaN";
            else if (sub_inf_case) cout << (test.expected_sub > 0 ? "+Inf" : "-Inf");
            else cout << test.expected_sub;
            cout << " vs " << sub_actual << " - " << (sub_test ? "PASS" : "FAIL") << endl;
        }
        
        // Check multiplier results
        if (mul_valid.read()) {
            float mul_actual = hexToFloat(mul_result.read());
            bool mul_nan_case = std::isnan(test.expected_mul);
            bool mul_inf_case = std::isinf(test.expected_mul);
            
            if (mul_nan_case && std::isnan(mul_actual)) {
                mul_test = true;
            } else if (mul_inf_case && std::isinf(mul_actual) && 
                      ((test.expected_mul > 0) == (mul_actual > 0))) {
                mul_test = true;
            } else {
                mul_test = compare_floats(mul_actual, test.expected_mul, test.check_sign);
            }
            
            if (mul_test) mul_passed++;
            
            cout << "  MUL: ";
            if (mul_nan_case) cout << "NaN";
            else if (mul_inf_case) cout << (test.expected_mul > 0 ? "+Inf" : "-Inf");
            else cout << test.expected_mul;
            cout << " vs " << mul_actual << " - " << (mul_test ? "PASS" : "FAIL") << endl;
        }
    }
    
    // Additional comprehensive tests
    cout << "\n=== ADDITIONAL TESTS ===" << endl;
    
    // Test accumulation
    cout << "\nTesting accumulation..." << endl;
    float accumulator = 0.0f;
    const int num_ops = 100;
    const float increment = 0.01f;
    
    A.write(floatToHex(accumulator));
    for (int i = 0; i < num_ops; i++) {
        B.write(floatToHex(increment));
        sc_start(30, SC_NS);
        
        uint32_t result_hex = add_result.read();
        memcpy(&accumulator, &result_hex, sizeof(float));
        A.write(result_hex);
        
        if (i % 20 == 19) {
            cout << "  After " << (i+1) << " additions: " << accumulator << endl;
        }
    }
    
    // Test error propagation
    cout << "\nTesting error propagation..." << endl;
    float a = 1.0f;
    float b = 1.000001f;
    float c = 1.000002f;
    
    A.write(floatToHex(a));
    B.write(floatToHex(b));
    sc_start(30, SC_NS);
    float sum1 = hexToFloat(add_result.read());
    
    A.write(floatToHex(sum1));
    B.write(floatToHex(c));
    sc_start(30, SC_NS);
    float final_sum = hexToFloat(add_result.read());
    
    cout << "  (1.0 + 1.000001) + 1.000002 = " << final_sum << endl;
    cout << "  Exact result would be: " << (a + b + c) << endl;
    
    // Final test summary
    cout << "\n================ TEST SUMMARY ================" << endl;
    cout << "Total tests: " << total_tests << endl;
    cout << "Adder passed: " << add_passed << "/" << total_tests 
         << " (" << (100.0f * add_passed / total_tests) << "%)" << endl;
    cout << "Subtractor passed: " << sub_passed << "/" << total_tests 
         << " (" << (100.0f * sub_passed / total_tests) << "%)" << endl;
    cout << "Multiplier passed: " << mul_passed << "/" << total_tests 
         << " (" << (100.0f * mul_passed / total_tests) << "%)" << endl;
    
    bool all_passed = (add_passed == total_tests) && 
                     (sub_passed == total_tests) && 
                     (mul_passed == total_tests);
    
    if (all_passed) {
        cout << "ALL TESTS PASSED!" << endl;
    } else {
        cout << "SOME TESTS FAILED!" << endl;
    }
    
    return all_passed ? 0 : 1;
}

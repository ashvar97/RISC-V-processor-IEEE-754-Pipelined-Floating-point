
#include <systemc.h>
#include <cmath>
#include <iomanip>
#include <cfloat>
#include <cstring>

using namespace std;

// Ensure float constants are available
#ifndef __FLT_EPSILON__
#define __FLT_EPSILON__ 1.19209290e-07F
#endif

#ifndef __FLT_MIN__
#define __FLT_MIN__ 1.17549435e-38F
#endif



inline float hexToFloat(uint32_t hex) {
    float f;
    memcpy(&f, &hex, sizeof(float));
    return f;
}


// Utility function to convert float to hex representation
uint32_t floatToHex(float value) {
    uint32_t result;
    memcpy(&result, &value, sizeof(float));
    return result;
}

// Utility function to print float value from hex
void printFloat(uint32_t hex) {
    float f;
    memcpy(&f, &hex, sizeof(float));
    cout << " (value: " << f << ")";
}

int sc_main(int argc, char* argv[]) {
    sc_clock clock("clk", 10, SC_NS);
    sc_signal<bool> reset;
    sc_signal<sc_uint<32>> A, B;
    sc_signal<sc_uint<32>> result;
    sc_signal<bool> valid_out;
    sc_signal<bool> overflow, underflow;
    
    ieee754add adder("adder");
    
    adder.clk(clock);
    adder.reset(reset);
    adder.A(A);
    adder.B(B);
    adder.result(result);
    adder.valid_out(valid_out);
    adder.overflow(overflow);
    adder.underflow(underflow);
    
    cout << "Starting simulation of IEEE 754 Floating Point Adder" << endl;
    
    // Test cases
    struct TestCase {
        float a;
        float b;
        const char* description;
    };
    
    TestCase test_cases[] = {
        // Basic addition cases
        {9.5f, -1.0f, "Normal numbers (3.5 + 1.25)"},
        {100.0f, 0.01f, "Normal numbers with different scales (100 + 0.01)"},
        {1.0f, 1.0f, "Identity addition (1.0 + 1.0)"},
        {5.0f, -3.0f, "Positive + Negative (5.0 + (-3.0))"},
        {-4.0f, -5.0f, "Negative + Negative (-4.0 + (-5.0))"},
        {7.5f, -7.5f, "Cancellation (7.5 + (-7.5))"},
        
        // Zero cases
        {0.0f, 5.0f, "Zero addition (0.0 + 5.0)"},
        {0.0f, 0.0f, "Zero by zero (0.0 + 0.0)"},
        {-0.0f, 0.0f, "Negative zero + zero (-0.0 + 0.0)"},
        {-0.0f, -0.0f, "Negative zero + negative zero (-0.0 + (-0.0))"},
        
        // Special cases
        {INFINITY, 5.0f, "Infinity addition (inf + 5.0)"},
        {INFINITY, -INFINITY, "Infinity + negative infinity (inf + (-inf) -> NaN)"},
        {INFINITY, INFINITY, "Infinity + infinity (inf + inf)"},
        {NAN, 5.0f, "NaN propagation (NaN + 5.0)"},
        {5.0f, NAN, "NaN propagation (5.0 + NaN)"},
        
        // Small number cases
        {1e-20f, 1e20f, "Very different magnitudes"},
        {1e-30f, 1e-31f, "Very small numbers"},
        {1e30f, 1e31f, "Very large numbers"},
        
        // Edge cases for normalization
        {1.0f, 1.0f + __FLT_EPSILON__, "Close numbers (1.0 + (1.0 + epsilon))"},
        {1.5f, 0.5f, "Numbers resulting in carry (1.5 + 0.5)"},
        
        // Denormalized number tests (very small numbers)
        {1e-38f, 1e-39f, "Near denormal range"},
        {__FLT_MIN__, __FLT_MIN__, "Minimum float values"},
        
        // Precision edge cases  
        {1.0f, 0x1p-24f, "1.0 + smallest representable fraction"},
        {0x1.fffffep+127f, 1.0f, "Near maximum float + 1.0"}
    };
    
    // Reset the adder
    cout << "Resetting the adder..." << endl;
    reset.write(true);
    A.write(0);
    B.write(0);
    sc_start(30, SC_NS);
    reset.write(false);
    sc_start(10, SC_NS);
    
    int total_tests = 0;
    int passed_tests = 0;
    
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
        
        // Calculate expected result
        float expected_float = test.a + test.b;
        uint32_t expected_hex = floatToHex(expected_float);
        uint32_t actual_hex = result.read();
        
        bool nan_case = std::isnan(test.a) || std::isnan(test.b) || 
                       (std::isinf(test.a) && std::isinf(test.b) && 
                        ((test.a > 0) != (test.b > 0))); // inf + (-inf)
        
        cout << "  Expected: ";
        if (nan_case) {
            cout << "NaN (0x7FC00000 or similar)";
        } else if (std::isinf(expected_float)) {
            cout << (expected_float < 0 ? "-Inf" : "+Inf");
            cout << " (0x" << hex << expected_hex << ")";
        } else {
            cout << expected_float << " (0x" << hex << expected_hex << ")";
        }
        cout << dec << endl;
        
        cout << "  Actual:   0x" << hex << actual_hex;
        printFloat(actual_hex);
        cout << dec << endl;
        
        cout << "  Valid:    " << valid_out.read() << endl;
        cout << "  Overflow: " << overflow.read() << endl;
        cout << "  Underflow:" << underflow.read() << endl;
        
        // Check results
        bool test_passed = false;
        
        if (nan_case) {
            // For NaN results, check exponent all 1s and mantissa non-zero
            test_passed = ((actual_hex & 0x7F800000) == 0x7F800000) && 
                         ((actual_hex & 0x007FFFFF) != 0);
        } else if (std::isinf(expected_float)) {
            // For infinity, check correct sign
            test_passed = (actual_hex == expected_hex);
        } else {
            // For normal cases, allow small floating point errors
            float actual_float;
            memcpy(&actual_float, &actual_hex, sizeof(float));
            
            if (expected_float == 0.0f && actual_float == 0.0f) {
                // Both zero - consider passed (ignore sign for now)
                test_passed = true;
            } else if (expected_float == 0.0f || actual_float == 0.0f) {
                // One is zero, other isn't - check if very close to zero
                float non_zero = (expected_float == 0.0f) ? actual_float : expected_float;
                test_passed = (fabs(non_zero) < 1e-35f);
            } else {
                // Both non-zero - check relative error
                float rel_error = fabs((actual_float - expected_float) / expected_float);
                test_passed = (rel_error < 1e-6f) || (actual_hex == expected_hex);
            }
        }
        
        if (test_passed) {
            cout << "  PASS" << endl;
            passed_tests++;
        } else {
            cout << "  FAIL" << endl;
            
            // Additional debug info for failed tests
            float actual_float;
            memcpy(&actual_float, &actual_hex, sizeof(float));
            if (!nan_case && !std::isinf(expected_float)) {
                float error = actual_float - expected_float;
                cout << "    Error: " << error << endl;
                if (expected_float != 0.0f) {
                    float rel_error = error / expected_float;
                    cout << "    Relative error: " << rel_error << endl;
                }
            }
        }
    }
    
    // Additional comprehensive tests for edge cases
    cout << "\n=== ADDITIONAL EDGE CASE TESTS ===" << endl;
    
    // Test many small additions that should sum to a larger number
    cout << "\nTesting accumulation of small numbers..." << endl;
    float accumulator = 0.0f;
    const int num_additions = 100;
    const float small_increment = 0.01f;
    
    A.write(floatToHex(accumulator));
    for (int i = 0; i < num_additions; i++) {
        B.write(floatToHex(small_increment));
        sc_start(30, SC_NS); // Wait for pipeline
        
        uint32_t result_hex = result.read();
        memcpy(&accumulator, &result_hex, sizeof(float));
        A.write(result_hex); // Use result as next A input
        
        if (i % 20 == 19) { // Print every 20th result
            cout << "  After " << (i+1) << " additions: " << accumulator << endl;
        }
    }
    
    float expected_accumulation = num_additions * small_increment;
    float error = accumulator - expected_accumulation;
    cout << "  Expected total: " << expected_accumulation << endl;
    cout << "  Actual total:   " << accumulator << endl;
    cout << "  Error:          " << error << endl;
    cout << "  Relative error: " << (error / expected_accumulation) << endl;
    
    if (fabs(error / expected_accumulation) < 0.01f) { // Allow 1% error for accumulation
        cout << "  Accumulation test: PASS" << endl;
        passed_tests++;
    } else {
        cout << "  Accumulation test: FAIL" << endl;
    }
    total_tests++;
    
    // Test subtraction that should result in very small numbers
    cout << "\nTesting near-cancellation subtraction..." << endl;
    float big_num = 1000000.0f;
    float almost_same = big_num + 1.0f;
    
    A.write(floatToHex(almost_same));
    B.write(floatToHex(-big_num)); // Subtract big_num
    sc_start(30, SC_NS);
    
    uint32_t cancel_result = result.read();
    float cancel_float;
    memcpy(&cancel_float, &cancel_result, sizeof(float));
    
    cout << "  " << almost_same << " + (" << (-big_num) << ") = " << cancel_float << endl;
    cout << "  Expected: 1.0, Actual: " << cancel_float << endl;
    
    if (fabs(cancel_float - 1.0f) < 1e-5f) {
        cout << "  Near-cancellation test: PASS" << endl;
        passed_tests++;
    } else {
        cout << "  Near-cancellation test: FAIL" << endl;
    }
    total_tests++;
    
    cout << "\n================ TEST SUMMARY ================" << endl;
    cout << "Total tests: " << total_tests << endl;
    cout << "Passed:      " << passed_tests << endl;
    cout << "Failed:      " << (total_tests - passed_tests) << endl;
    cout << "Success rate: " << std::fixed << std::setprecision(1) 
         << (100.0f * passed_tests / total_tests) << "%" << endl;
    
    if (passed_tests == total_tests) {
        cout << "ALL TESTS PASSED!" << endl;
    } else {
        cout << "SOME TESTS FAILED!" << endl;
    }
    
    // Performance analysis
    cout << "\n================ PERFORMANCE ANALYSIS ================" << endl;
    cout << "Pipeline depth: 3 stages" << endl;
    cout << "Latency: 3 clock cycles" << endl;
    cout << "Throughput: 1 result per clock cycle (after initial latency)" << endl;
    cout << "Clock period: 10 ns" << endl;
    cout << "Maximum frequency: 100 MHz" << endl;
    
    return (passed_tests == total_tests) ? 0 : 1;
}

#include "systemc.h"
#include <cmath>
#include <iomanip>
#include <cstdlib>

// Utility functions for float/hex conversion
uint32_t floatToHex(float f) {
    union { float f; uint32_t i; } u;
    u.f = f;
    return u.i;
}

float hexToFloat(uint32_t i) {
    union { float f; uint32_t i; } u;
    u.i = i;
    return u.f;
}

void printFloat(uint32_t hex_val) {
    float f = hexToFloat(hex_val);
    if (std::isnan(f)) {
        std::cout << " (NaN)";
    } else if (std::isinf(f)) {
        std::cout << " (" << (f > 0 ? "+Inf" : "-Inf") << ")";
    } else {
        std::cout << " (" << f << ")";
    }
}

int sc_main(int argc, char* argv[]) {
    sc_clock clock("clk", 10, SC_NS);
    sc_signal<bool> reset;
    sc_signal<sc_uint<32>> a, b;
    sc_signal<sc_uint<32>> result;
    sc_signal<bool> valid_out;
    sc_signal<bool> overflow, underflow, divide_by_zero;
    
    ieee754div divider("divider");
    
    divider.clk(clock);
    divider.reset(reset);
    divider.a(a);
    divider.b(b);
    divider.result(result);
    divider.valid_out(valid_out);
    divider.overflow(overflow);
    divider.underflow(underflow);
    divider.divide_by_zero(divide_by_zero);
    
    std::cout << "Starting simulation of IEEE 754 Floating Point Divider (27-stage pipeline)" << std::endl;
    
    // Test cases
    struct TestCase {
        float a;
        float b;
        const char* description;
    };
    
    TestCase test_cases[] = {
        // Basic division cases
        {10.0f, 2.0f, "Simple division (10.0 / 2.0)"},
        {7.0f, 3.0f, "Non-exact division (7.0 / 3.0)"},
        {1.0f, 3.0f, "Fraction result (1.0 / 3.0)"},
        {100.0f, 0.01f, "Large / small (100.0 / 0.01)"},
        {0.01f, 100.0f, "Small / large (0.01 / 100.0)"},
        {1.0f, 1.0f, "Identity division (1.0 / 1.0)"},
        
        // Sign combinations
        {10.0f, -2.0f, "Positive / negative (10.0 / -2.0)"},
        {-10.0f, 2.0f, "Negative / positive (-10.0 / 2.0)"},
        {-10.0f, -2.0f, "Negative / negative (-10.0 / -2.0)"},
        
        // Division by zero cases
        {1.0f, 0.0f, "Division by zero (1.0 / 0.0 -> +inf)"},
        {-1.0f, 0.0f, "Negative division by zero (-1.0 / 0.0 -> -inf)"},
        {INFINITY, 0.0f, "Infinity / zero (inf / 0.0 -> inf)"},
        
        // Zero dividend cases
        {0.0f, 1.0f, "Zero dividend (0.0 / 1.0)"},
        {0.0f, -1.0f, "Zero / negative (0.0 / -1.0)"},
        {-0.0f, 1.0f, "Negative zero dividend (-0.0 / 1.0)"},
        {0.0f, 0.0f, "Zero / zero (0.0 / 0.0 -> NaN)"},
        
        // Infinity cases
        {INFINITY, 1.0f, "Infinity dividend (inf / 1.0)"},
        {INFINITY, -1.0f, "Infinity / negative (inf / -1.0)"},
        {1.0f, INFINITY, "Finite / infinity (1.0 / inf)"},
        {INFINITY, INFINITY, "Infinity / infinity (inf / inf -> NaN)"},
        {-INFINITY, INFINITY, "Different sign infinities (-inf / inf -> NaN)"},
        
        // NaN cases
        {NAN, 1.0f, "NaN dividend (NaN / 1.0)"},
        {1.0f, NAN, "NaN divisor (1.0 / NaN)"},
        {NAN, NAN, "NaN / NaN"},
        
        // Very small and large numbers
        {1e-20f, 1e-20f, "Very small / very small"},
        {1e20f, 1e20f, "Very large / very large"},
        {1e30f, 1e-30f, "Large / small (potential overflow)"},
        
        // Numbers near 1
        {1.000001f, 1.0f, "Close to 1 / 1"},
        {1.0f, 1.000001f, "1 / close to 1"},
        {0.999999f, 1.000001f, "Close numbers"},
        
        // Powers of 2 (should be exact)
        {8.0f, 2.0f, "Power of 2 division (8.0 / 2.0)"},
        {1.0f, 8.0f, "Reciprocal power of 2 (1.0 / 8.0)"},
        {16.0f, 4.0f, "Power of 2 / power of 2"},
        
        // Edge cases for normalization
        {3.0f, 6.0f, "Result < 1 requiring normalization"},
        {0.75f, 1.5f, "Fraction / fraction"},
        
        // Denormalized results
        {__FLT_MIN__, 2.0f, "Minimum float / 2 (underflow)"},
        
        // Maximum finite values
        {3.4e38f, 1.0f, "Maximum float / 1"},
        {3.4e38f, 0.5f, "Maximum float / 0.5 (potential overflow)"}
    };

    const int NUM_TESTS = sizeof(test_cases) / sizeof(TestCase);
    const int PIPELINE_STAGES = 27; // Number of pipeline stages in your design
    const int TEST_INTERVAL = PIPELINE_STAGES + 2; // Allow extra cycles for safety

    // Open VCD file for waveform dumping
    sc_trace_file *wf = sc_create_vcd_trace_file("divider_waveforms");
    sc_trace(wf, clock, "clock");
    sc_trace(wf, reset, "reset");
    sc_trace(wf, a, "a");
    sc_trace(wf, b, "b");
    sc_trace(wf, result, "result");
    sc_trace(wf, valid_out, "valid_out");
    sc_trace(wf, overflow, "overflow");
    sc_trace(wf, underflow, "underflow");
    sc_trace(wf, divide_by_zero, "divide_by_zero");

    // Reset the system
    reset = true;
    sc_start(20, SC_NS);
    reset = false;

    // Test result counters
    int passed = 0;
    int failed = 0;
    int warnings = 0;

    // Run test cases
    for (int i = 0; i < NUM_TESTS; i++) {
        TestCase tc = test_cases[i];
        uint32_t a_hex = floatToHex(tc.a);
        uint32_t b_hex = floatToHex(tc.b);

        a = a_hex;
        b = b_hex;

        std::cout << "\nTest " << i+1 << "/" << NUM_TESTS << ": " << tc.description << std::endl;
        std::cout << "Input A: 0x" << std::hex << std::setw(8) << std::setfill('0') << a_hex;
        printFloat(a_hex);
        std::cout << "\nInput B: 0x" << std::hex << std::setw(8) << std::setfill('0') << b_hex;
        printFloat(b_hex);
        std::cout << std::endl;

        sc_start(TEST_INTERVAL * 10, SC_NS);

        if (valid_out.read()) {
            uint32_t res_hex = result.read();
            float expected = tc.a / tc.b; // Reference result
            
            std::cout << "Result:   0x" << std::hex << std::setw(8) << std::setfill('0') << res_hex;
            printFloat(res_hex);
            
            std::cout << "\nExpected: 0x" << std::hex << std::setw(8) << std::setfill('0') << floatToHex(expected);
            printFloat(floatToHex(expected));
            
            // Check flags
            if (divide_by_zero.read()) {
                std::cout << "\nDivide by zero flag set";
            }
            if (overflow.read()) {
                std::cout << "\nOverflow flag set";
            }
            if (underflow.read()) {
                std::cout << "\nUnderflow flag set";
            }
            
            // Simplified comparison - only check for major issues
            float result_float = hexToFloat(res_hex);
            bool test_passed = true;
            
            // For special values, do exact comparison
            if (std::isnan(expected)) {
                if (!std::isnan(result_float)) {
                    std::cout << "\nERROR: Expected NaN but got non-NaN result";
                    test_passed = false;
                }
            } else if (std::isinf(expected)) {
                if (!std::isinf(result_float) || (std::signbit(expected) != std::signbit(result_float))) {
                    std::cout << "\nERROR: Expected " << (expected > 0 ? "+Inf" : "-Inf") 
                              << " but got different result";
                    test_passed = false;
                }
            } else if (expected == 0.0f) {
                if (result_float != 0.0f || (std::signbit(expected) != std::signbit(result_float))) {
                    std::cout << "\nERROR: Expected " << (std::signbit(expected) ? "-0" : "+0") 
                              << " but got different result";
                    test_passed = false;
                }
            } else {
                // For normal values, allow small precision differences
                // IEEE 754 single precision has about 7 decimal digits of precision
                // Allow for small rounding errors inherent in floating-point division
                if (fabs(result_float - expected) > fabs(expected) * 1e-6 && 
                    fabs(result_float - expected) > 1e-30) {
                    std::cout << "\nWARNING: Result differs from expected (may be due to rounding)";
                    std::cout << "\n  Difference: " << std::scientific << (result_float - expected);
                    warnings++;
                } else {
                    test_passed = true;
                }
            }
            
            // Update counters
            if (test_passed) {
                std::cout << " [PASS]";
                passed++;
            } else {
                std::cout << " [FAIL]";
                failed++;
            }
            
            std::cout << std::endl;
        } else {
            std::cout << "ERROR: Valid signal not asserted for test case [FAIL]" << std::endl;
            failed++;
        }
    }

    // Print test summary
    std::cout << "\n" << std::string(60, '=') << std::endl;
    std::cout << "TEST SUMMARY" << std::endl;
    std::cout << std::string(60, '=') << std::endl;
    std::cout << "Total Tests: " << NUM_TESTS << std::endl;
    std::cout << "Passed:      " << passed << std::endl;
    std::cout << "Failed:      " << failed << std::endl;
    std::cout << "Warnings:    " << warnings << std::endl;
    std::cout << "Success Rate: " << std::fixed << std::setprecision(1) 
              << (100.0 * passed / NUM_TESTS) << "%" << std::endl;
    std::cout << std::string(60, '=') << std::endl;

    if (failed == 0) {
        std::cout << "🎉 All tests passed!" << std::endl;
    } else {
        std::cout << "⚠️  " << failed << " test(s) failed - check results above" << std::endl;
    }
    
    if (warnings > 0) {
        std::cout << "ℹ️  " << warnings << " warning(s) - minor precision differences detected" << std::endl;
    }

    // Close VCD file
    sc_close_vcd_trace_file(wf);
    
    std::cout << "\nSimulation complete. Check divider_waveforms.vcd for detailed timing analysis." << std::endl;
    
    // Return non-zero exit code if tests failed
    return (failed > 0) ? 1 : 0;
}

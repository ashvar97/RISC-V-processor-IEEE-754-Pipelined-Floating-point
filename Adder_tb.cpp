// Test Bench
int sc_main(int argc, char* argv[]) {
    // Create clock and reset signals
    sc_clock clk("clk", 10, SC_NS);  // 10ns period = 100MHz
    sc_signal<bool> reset;

    // Adder signals
    sc_signal<sc_uint<32>> A, B;
    sc_signal<bool> valid_in;
    sc_signal<sc_uint<32>> result;
    sc_signal<bool> valid_out;
    sc_signal<bool> overflow, underflow;
    
    // Instantiate the pipelined adder
    ieee754_adder_pipelined adder("adder");
    adder.clk(clk);
    adder.reset(reset);
    adder.A(A);
    adder.B(B);
    adder.valid_in(valid_in);
    adder.result(result);
    adder.valid_out(valid_out);
    adder.overflow(overflow);
    adder.underflow(underflow);

    // Trace setup for viewing in waveform viewer
    sc_trace_file *tf = sc_create_vcd_trace_file("pipelined_adder_trace");
    sc_trace(tf, clk, "clk");
    sc_trace(tf, reset, "reset");
    sc_trace(tf, A, "A");
    sc_trace(tf, B, "B");
    sc_trace(tf, valid_in, "valid_in");
    sc_trace(tf, result, "result");
    sc_trace(tf, valid_out, "valid_out");
    sc_trace(tf, overflow, "overflow");
    sc_trace(tf, underflow, "underflow");
    
    // Trace internal pipeline registers
    sc_trace(tf, adder.A_sign_reg, "A_sign_reg");
    sc_trace(tf, adder.A_Exponent_reg, "A_Exponent_reg");
    sc_trace(tf, adder.A_Mantissa_reg, "A_Mantissa_reg");
    sc_trace(tf, adder.valid_stage1, "valid_stage1");
    sc_trace(tf, adder.valid_stage2, "valid_stage2");
    sc_trace(tf, adder.valid_stage3, "valid_stage3");

    // Test cases
    struct TestCase {
        float a;
        float b;
        float expected;
        const char* description;
    };

    TestCase test_cases[] = {
        {1.5f, 2.5f, 4.0f, "Basic addition: 1.5 + 2.5"},
        {-3.25f, 5.75f, 2.5f, "Mixed signs: -3.25 + 5.75"},
        {0.0f, 42.0f, 42.0f, "Zero + number: 0.0 + 42.0"},
        {-0.0f, 0.0f, 0.0f, "Negative zero + positive zero"},
        {std::numeric_limits<float>::infinity(), 1.0f, std::numeric_limits<float>::infinity(), "Infinity + number"},
        {-std::numeric_limits<float>::infinity(), std::numeric_limits<float>::infinity(), std::numeric_limits<float>::quiet_NaN(), "Negative infinity + positive infinity (NaN)"},
        {1e-38f, 1e-38f, 2e-38f, "Very small numbers"},
        {3.14159f, 2.71828f, 5.85987f, "Pi + e"},
        {1000000.0f, 0.001f, 1000000.001f, "Large + small numbers"},
        {-7.5f, 7.5f, 0.0f, "Cancellation: -7.5 + 7.5"}
    };

    int num_tests = sizeof(test_cases) / sizeof(test_cases[0]);

    cout << "\n=== Pipelined IEEE 754 Adder Test ===\n";
    cout << "Pipeline has 3 stages: Extract -> Add -> Normalize\n";
    cout << "Result will be available after 3 clock cycles\n\n";

    // Initialize
    reset = true;
    A = 0;
    B = 0;
    valid_in = false;
    
    cout << "Applying reset...\n";
    sc_start(20, SC_NS);  // 2 clock cycles reset

    reset = false;
    valid_in = true;
    cout << "Reset released, starting tests...\n\n";

    // Run test cases
    for (int i = 0; i < num_tests; i++) {
        TestCase &tc = test_cases[i];
        
        // Convert floats to raw bits for input
        uint32_t a_bits = *reinterpret_cast<uint32_t*>(&tc.a);
        uint32_t b_bits = *reinterpret_cast<uint32_t*>(&tc.b);
        
        // Set inputs
        A = a_bits;
        B = b_bits;
        
        cout << "Test " << (i+1) << ": " << tc.description << endl;
        cout << "  Input A = " << tc.a << " (0x" << hex << a_bits << ")" << endl;
        cout << "  Input B = " << tc.b << " (0x" << hex << b_bits << ")" << endl;
        cout << "  Expected = " << tc.expected << endl;
        
        cout << "\n  Pipeline progress:" << endl;
        
        // Monitor pipeline for 4 clock cycles to see the progression
        for (int cycle = 0; cycle < 4; cycle++) {
            sc_start(10, SC_NS);  // One clock cycle
            
            cout << "    Cycle " << (cycle + 1) << ": ";
            cout << "valid_in=" << (valid_in.read() ? "1" : "0");
            cout << ", stage1=" << (adder.valid_stage1.read() ? "1" : "0");
            cout << ", stage2=" << (adder.valid_stage2.read() ? "1" : "0");
            cout << ", stage3=" << (adder.valid_stage3.read() ? "1" : "0");
            cout << ", valid_out=" << (valid_out.read() ? "1" : "0");
            
            if (valid_out.read()) {
                uint32_t result_bits = result.read();
                float result_float = *reinterpret_cast<float*>(&result_bits);
                cout << " -> RESULT: " << result_float << " (0x" << hex << result_bits << ")";
                
                // Validation
                bool is_expected_nan = std::isnan(tc.expected);
                bool is_result_nan = std::isnan(result_float);
                
                if (is_expected_nan && is_result_nan) {
                    cout << " ✓ PASS (both NaN)";
                } else if (std::isinf(tc.expected) && std::isinf(result_float) && 
                          (std::signbit(tc.expected) == std::signbit(result_float))) {
                    cout << " ✓ PASS (both infinity with same sign)";
                } else if (abs(result_float - tc.expected) < 0.001f) {
                    cout << " ✓ PASS";
                } else {
                    cout << " ✗ FAIL";
                }
                
                if (overflow.read()) cout << " [OVERFLOW]";
                if (underflow.read()) cout << " [UNDERFLOW]";
            }
            cout << endl;
        }
        
        // Clear valid_in for next test to show pipeline behavior
        if (i < num_tests - 1) {
            valid_in = false;
            sc_start(10, SC_NS);  // One cycle with no new input
            valid_in = true;
        }
        
        cout << endl;
    }

    // Test pipeline throughput with consecutive inputs
    cout << "\n=== Pipeline Throughput Test ===\n";
    cout << "Testing pipeline with consecutive inputs every cycle\n\n";
    
    // Prepare consecutive test inputs
    float consecutive_inputs[][2] = {
        {1.0f, 2.0f},
        {3.0f, 4.0f},
        {5.0f, 6.0f},
        {7.0f, 8.0f}
    };
    
    valid_in = true;
    
    // Feed inputs every cycle
    for (int i = 0; i < 4; i++) {
        uint32_t a_bits = *reinterpret_cast<uint32_t*>(&consecutive_inputs[i][0]);
        uint32_t b_bits = *reinterpret_cast<uint32_t*>(&consecutive_inputs[i][1]);
        
        A = a_bits;
        B = b_bits;
        
        cout << "Cycle " << (i+1) << ": Input " << consecutive_inputs[i][0] 
             << " + " << consecutive_inputs[i][1] << endl;
        
        sc_start(10, SC_NS);
    }
    
    // Continue for a few more cycles to see all outputs
    valid_in = false;  // No more new inputs
    A = 0;
    B = 0;
    
    for (int i = 0; i < 4; i++) {
        sc_start(10, SC_NS);
        
        cout << "Cycle " << (i+5) << ": ";
        if (valid_out.read()) {
            uint32_t result_bits = result.read();
            float result_float = *reinterpret_cast<float*>(&result_bits);
            cout << "Output = " << result_float;
        } else {
            cout << "No output";
        }
        cout << endl;
    }
    
    // Test special cases
    cout << "\n=== Special Cases Test ===\n";
    
    struct SpecialCase {
        uint32_t a_bits;
        uint32_t b_bits;
        const char* description;
    };
    
    SpecialCase special_cases[] = {
        {0x7F800000, 0x3F800000, "Positive infinity + 1.0"},
        {0xFF800000, 0x7F800000, "Negative infinity + positive infinity"},
        {0x7FC00000, 0x3F800000, "NaN + 1.0"},
        {0x00000000, 0x80000000, "Positive zero + negative zero"},
        {0x00800000, 0x00400000, "Smallest normal + denormalized"}
    };
    
    int num_special = sizeof(special_cases) / sizeof(special_cases[0]);
    
    valid_in = true;
    
    for (int i = 0; i < num_special; i++) {
        SpecialCase &sc = special_cases[i];
        
        A = sc.a_bits;
        B = sc.b_bits;
        
        cout << "Special Test " << (i+1) << ": " << sc.description << endl;
        cout << "  A = 0x" << hex << sc.a_bits << endl;
        cout << "  B = 0x" << hex << sc.b_bits << endl;
        
        // Wait for pipeline to complete
        for (int cycle = 0; cycle < 4; cycle++) {
            sc_start(10, SC_NS);
        }
        
        if (valid_out.read()) {
            uint32_t result_bits = result.read();
            float result_float = *reinterpret_cast<float*>(&result_bits);
            
            cout << "  Result = " << result_float << " (0x" << hex << result_bits << ")";
            if (overflow.read()) cout << " [OVERFLOW]";
            if (underflow.read()) cout << " [UNDERFLOW]";
            cout << endl;
        }
        cout << endl;
    }
    
    // Performance analysis
    cout << "\n=== Performance Analysis ===\n";
    cout << "Pipeline Characteristics:\n";
    cout << "  - Latency: 3 clock cycles\n";
    cout << "  - Throughput: 1 result per clock cycle (after initial latency)\n";
    cout << "  - Clock frequency: 100 MHz (10ns period)\n";
    cout << "  - Maximum throughput: 100 MFLOPS\n";
    cout << "  - Area: 3 pipeline stages with registers\n\n";
    
    cout << "Advantages of pipelined design:\n";
    cout << "  + High throughput for streaming data\n";
    cout << "  + Better resource utilization\n";
    cout << "  + Scalable to higher frequencies\n\n";
    
    cout << "Trade-offs:\n";
    cout << "  - Increased latency (3 cycles vs 1)\n";
    cout << "  - Additional register overhead\n";
    cout << "  - More complex control logic\n\n";
    
    // Cleanup
    sc_close_vcd_trace_file(tf);
    
    cout << "Simulation completed. VCD trace saved as 'pipelined_adder_trace.vcd'\n";
    cout << "Use a waveform viewer like GTKWave to analyze the timing diagrams.\n";
    
    return 0;
}

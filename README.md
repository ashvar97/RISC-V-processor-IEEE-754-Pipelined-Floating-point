# IEEE 754 Floating-Point Pipeline Processor

A comprehensive SystemC implementation of a 5-stage pipelined floating-point unit (FPU) supporting IEEE 754 single-precision operations with hazard detection, branch prediction, and exception handling.

## 🚀 Features

### Core Operations
- **FADD**: Floating-point addition
- **FSUB**: Floating-point subtraction  
- **FMUL**: Floating-point multiplication
- **FDIV**: Floating-point division with multi-cycle execution

### Architecture Highlights
- **5-Stage Pipeline**: Fetch → Decode → Execute → Memory → Writeback
- **IEEE 754 Compliance**: Full support for special values (NaN, infinity, zero, denormalized numbers)
- **Exception Handling**: Invalid operation, overflow, underflow, divide-by-zero, inexact result
- **Hazard Detection**: Data forwarding and pipeline stalling
- **Branch Support**: Conditional branches with flush capability
- **Multi-cycle Division**: Non-blocking division with dedicated execution slots

### Special Value Support
- ✅ **NaN (Not a Number)**: Proper propagation and generation
- ✅ **Infinity**: Positive and negative infinity handling
- ✅ **Zero**: Signed zero support (+0.0, -0.0)
- ✅ **Denormalized Numbers**: Subnormal value processing
- ✅ **Exception Flags**: IEEE 754 compliant exception reporting


## 🏗️ Architecture Overview

### Pipeline Stages

1. **Fetch Stage**
   - Instruction memory interface
   - Program counter management
   - Branch target handling

2. **Decode Stage**  
   - Instruction parsing
   - Register file access (32 FP registers)
   - Operand forwarding
   - Exception flag management

3. **Execute Stage**
   - Multi-stage arithmetic pipeline
   - IEEE 754 decomposition/composition
   - Branch ALU for comparisons
   - Multi-cycle division queue

4. **Memory Stage** (Writeback)
   - Result forwarding
   - Register file updates
   - Exception flag accumulation

5. **Hazard Detection Unit**
   - Data dependency resolution
   - Pipeline stall generation
   - Branch prediction and flush

### Key Components

#### IEEE 754 Processing Engine
```cpp
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
```

#### Multi-Cycle Division
- **4 parallel division slots** for non-blocking execution
- **24-cycle restoring division** algorithm
- **Queue-based scheduling** for optimal throughput

#### Exception System
```cpp
enum fp_exceptions {
    FP_INVALID_OP     = 0x1,   // Invalid operation (0/0, inf-inf)
    FP_OVERFLOW       = 0x2,   // Result too large
    FP_UNDERFLOW      = 0x4,   // Result too small
    FP_DIVIDE_BY_ZERO = 0x8,   // Division by zero
    FP_INEXACT        = 0x10   // Inexact result
};
```

## 🧪 Testing

The project includes a comprehensive testbench covering:

### Test Categories

1. **Basic Arithmetic**
   - Addition: `3.0 + 2.0 = 5.0`
   - Subtraction: `3.0 - 2.0 = 1.0`
   - Multiplication: `3.0 × 2.0 = 6.0`
   - Division: `3.0 ÷ 2.0 = 1.5`

2. **Special Values**
   - Division by zero: `3.0 ÷ 0.0 = +∞`
   - Invalid operations: `+∞ + (-∞) = NaN`
   - Infinity arithmetic
   - Signed zero handling

3. **Edge Cases**
   - Overflow conditions: `large × large = +∞`
   - Underflow conditions: `tiny × tiny → 0`
   - Denormalized number operations
   - NaN propagation

4. **Pipeline Verification**
   - Hazard detection and resolution
   - Branch prediction accuracy
   - Multi-cycle operation handling
   - Exception flag accumulation

### Running Tests

```bash
# Compile with SystemC
g++ -I$SYSTEMC_HOME/include -L$SYSTEMC_HOME/lib-linux64 \
    -o fpu_pipeline fpu_pipeline.cpp -lsystemc -lm

# Run comprehensive tests
./fpu_pipeline

# Expected output: Test results with pass/fail status
```

## 📊 Performance Metrics

- **Pipeline Depth**: 5 stages
- **Throughput**: 1 operation per cycle (except division)
- **Division Latency**: 24-28 cycles
- **Register File**: 32 × 32-bit IEEE 754 registers
- **Instruction Memory**: 256 × 32-bit instructions
- **Exception Latency**: 1 cycle detection + propagation

## 🔧 Configuration Options

### Compile-Time Parameters
- `DIV_SLOTS`: Number of parallel division units (default: 4)
- `IMEM_SIZE`: Instruction memory size (default: 256)
- `REG_COUNT`: Number of FP registers (default: 32)

### Runtime Controls
- **Stall Signal**: External pipeline stall
- **Reset**: Synchronous reset with state clearing
- **Clock**: Configurable clock period (default: 10ns)

## 🚦 Usage Example

```cpp
// Create testbench
ComprehensiveTestbench tb("fpu_test");

// Load test program
vector<sc_uint<32>> program = {
    fp_instruction_t(OP_FADD, 3, 1, 2).to_word(),  // f3 = f1 + f2
    fp_instruction_t(OP_FMUL, 4, 3, 1).to_word(),  // f4 = f3 * f1
    fp_instruction_t(OP_FDIV, 5, 4, 2).to_word()   // f5 = f4 / f2
};

// Initialize registers
tb.fpu_top->decode_stage->set_register_bits(1, float_to_ieee754_bits(3.14f));
tb.fpu_top->decode_stage->set_register_bits(2, float_to_ieee754_bits(2.71f));

// Run simulation
sc_start();
```

## 🐛 Known Issues & Limitations

- **Division throughput**: Limited by 24-cycle latency
- **Branch prediction**: Simple static prediction
- **Memory interface**: Simple instruction-only memory
- **Rounding modes**: Only round-to-nearest supported



---

**Status**: ✅ Functional | 🧪 Tested | 📋 Documented | 🔧 Configurable

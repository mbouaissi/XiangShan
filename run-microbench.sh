#!/bin/bash
# Script to run microbenchmarks and collect results

cd /raid1/menzo/xs-env/XiangShan
source ../env.sh

TIMESTAMP=$(date +%Y%m%d_%H%M%S)
RESULTS_DIR="benchmark-results"
mkdir -p "$RESULTS_DIR"

echo "======================================"
echo "Running XiangShan Microbenchmarks"
echo "Timestamp: $TIMESTAMP"
echo "======================================"

# Function to run a benchmark
run_benchmark() {
    local name=$1
    local binary=$2
    local use_difftest=$3
    
    echo ""
    echo "Running: $name"
    echo "--------------------------------------"
    
    local output_file="$RESULTS_DIR/${name}_${TIMESTAMP}.log"
    local stats_file="$RESULTS_DIR/${name}_${TIMESTAMP}_stats.txt"
    
    if [ "$use_difftest" = "true" ]; then
        timeout 600 ./build/emu -i "$binary" \
            --diff ready-to-run/riscv64-nemu-interpreter-so \
            2>&1 | tee "$output_file"
    else
        timeout 600 ./build/emu -i "$binary" \
            2>&1 | tee "$output_file"
    fi
    
    local exit_code=$?
    
    # Extract statistics
    echo "=== Benchmark Results for $name ===" > "$stats_file"
    echo "Exit code: $exit_code" >> "$stats_file"
    echo "" >> "$stats_file"
    
    # Look for test results
    grep -E "Passed|FAILED|Total|time|cycles|IPC|Instructions" "$output_file" >> "$stats_file" 2>/dev/null
    
    # Get final statistics from emu
    tail -50 "$output_file" | grep -E "host time|guest cycles|guest instructions|IPC" >> "$stats_file" 2>/dev/null
    
    echo "Results saved to: $output_file"
    echo "Statistics saved to: $stats_file"
    
    if [ $exit_code -eq 124 ]; then
        echo "WARNING: Benchmark timed out after 600 seconds"
    fi
}

# Run benchmarks
echo ""
echo "Available benchmarks:"
echo "  1. microbench     - Multiple micro-benchmarks (qsort, queen, etc.)"
echo "  2. coremark       - CoreMark industry standard benchmark"
echo ""

# Run microbench (without difftest for speed)
run_benchmark "microbench" "ready-to-run/microbench.bin" "false"

# Run coremark
run_benchmark "coremark" "ready-to-run/coremark-2-iteration.bin" "false"

# Summary
echo ""
echo "======================================"
echo "Benchmark Run Complete!"
echo "======================================"
echo "Results directory: $RESULTS_DIR"
echo ""
echo "To view results:"
echo "  cat $RESULTS_DIR/*_stats.txt"
echo ""
echo "Performance metrics to look for:"
echo "  - Host time: Real time taken"
echo "  - Guest instructions: Total instructions executed"
echo "  - Guest cycles: Simulated CPU cycles"
echo "  - IPC: Instructions Per Cycle"
echo ""

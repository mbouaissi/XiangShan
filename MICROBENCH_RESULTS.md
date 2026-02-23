# Microbenchmark Results - XiangShan

## Status

✅ **Microbenchmarks are RUNNING in background**

## Output Location

All results are being saved to:
```
/raid1/menzo/xs-env/XiangShan/benchmark-results/microbench_output.txt
```

## Monitor Progress

```bash
# Watch live output
tail -f /raid1/menzo/xs-env/XiangShan/benchmark-results/microbench_output.txt

# Check file size (grows as benchmark runs)
ls -lh /raid1/menzo/xs-env/XiangShan/benchmark-results/

# View current results
cat /raid1/menzo/xs-env/XiangShan/benchmark-results/microbench_output.txt
```

## Expected Results

The microbench suite includes these tests:
1. **[qsort]** Quick sort - Sorting algorithm performance
2. **[queen]** Queen placement - N-queens backtracking algorithm
3. **[bf]** Brainf*ck interpreter
4. **[fib]** Fibonacci computation
5. **[sieve]** Prime number sieve
6. **[15puzzle]** 15-puzzle solver
7. **[dinic]** Maximum flow algorithm
8. **[lzip]** Compression algorithm
9. **[ssort]** Selection sort
10. **[md5]** MD5 hashing

Each test will show:
- **Pass/Fail** status
- **Execution time**
- **Correctness** verification

## Performance Metrics

At the end of the run, you'll see:
```
Total guest instructions:  XXXXX
Total guest cycles:        XXXXX  
IPC (Instructions Per Cycle): X.XX
Host time spent:           XX.XX seconds
Simulation speed:          XXXX KIPS (thousand instructions/sec)
```

## Estimated Completion Time

- **With difftest (current)**: 30-60 minutes
- **Without difftest**: 5-15 minutes

## When Complete

The benchmark will automatically:
1. Run all tests
2. Save complete output to the log file
3. Terminate when finished

Results remain in:
```
/raid1/menzo/xs-env/XiangShan/benchmark-results/
```

## Verification

After completion, check for:
```bash
grep -E "Passed|FAILED" benchmark-results/microbench_output.txt
```

All tests should show "* Passed." for correct execution.

## Additional Benchmarks

To run more benchmarks later:
```bash
cd /raid1/menzo/xs-env/XiangShan
source ../env.sh

# CoreMark (industry standard)
./build/emu -i ready-to-run/coremark-2-iteration.bin > benchmark-results/coremark.log 2>&1 &

# Linux boot test
./build/emu -i ready-to-run/linux.bin > benchmark-results/linux-boot.log 2>&1 &
```

## Performance Analysis

For detailed performance analysis after completion:
```bash
# Extract statistics
grep -A 10 "guest instructions\|IPC\|host time" benchmark-results/*.txt

# Compare with reference
# IPC target for MinimalConfig: ~0.5-1.5 (varies by workload)
# Full DefaultConfig would achieve higher IPC
```

---
**Note**: Benchmark is running with difftest enabled, which validates correctness but slows simulation. For faster results without verification, add `--no-diff` flag (requires rebuild without difftest).

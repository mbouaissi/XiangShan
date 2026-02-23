#!/bin/bash
# Check benchmark status and view results

cd /raid1/menzo/xs-env/XiangShan

echo "======================================"
echo "XiangShan Benchmark Status Checker"
echo "======================================"
echo ""

# Check if benchmarks are running
if ps aux | grep -q "[r]un-microbench\|[e]mu.*ready-to-run"; then
    echo "✓ Benchmarks are RUNNING"
    echo ""
    ps aux | grep "[r]un-microbench\|[e]mu.*ready-to-run" | grep -v grep
    echo ""
else
    echo "✗ No benchmarks currently running"
    echo ""
fi

# Show latest log output
if [ -f benchmark-run.log ]; then
    echo "=== Latest output (last 30 lines) ==="
    tail -30 benchmark-run.log
    echo ""
fi

# List completed results
if [ -d benchmark-results ]; then
    echo "=== Completed Results ==="
    ls -lht benchmark-results/ | head -15
    echo ""
    
    # Show any stats files
    stats_files=$(find benchmark-results/ -name "*_stats.txt" 2>/dev/null)
    if [ ! -z "$stats_files" ]; then
        echo "=== Statistics Summary ==="
        for f in $stats_files; do
            echo "--- $(basename $f) ---"
            cat "$f"
            echo ""
        done
    fi
else
    echo "No results directory yet"
fi

echo "======================================"
echo "Commands:"
echo "  Watch live: tail -f benchmark-run.log"
echo "  Check this: ./check-benchmark-status.sh"
echo "  View results: cat benchmark-results/*_stats.txt"
echo "======================================"

#!/bin/bash
# Rebuild emulator with FEC logging and test with CoreMark

set -e

echo "=================================================="
echo "FEC Tracker: Rebuild & Test"
echo "=================================================="
echo ""

cd /raid1/menzo/xs-env/XiangShan
source ../env.sh

echo "Step 1: Rebuilding emulator with FEC debug logging..."
echo "        This will take 10-30 minutes depending on your machine"
echo "        (Using $(nproc) cores)"
echo ""

# Clean previous build to ensure new printf statements are included
echo "Cleaning previous build artifacts..."
make clean

echo ""
echo "Building emulator..."
echo "Progress will be shown below:"
echo ""

# Build with verbose output
make emu -j$(nproc)

echo ""
echo "✅ Build complete!"
echo ""

# Check if CoreMark binary exists
if [ ! -f "./ready-to-run/coremark-2-iteration.bin" ]; then
    echo "ERROR: CoreMark binary not found at ./ready-to-run/coremark-2-iteration.bin"
    exit 1
fi

echo "=================================================="
echo "Step 2: Running CoreMark with FEC tracking..."
echo "=================================================="
echo ""
echo "Output will be saved to: fec-coremark-test.log"
echo ""
echo "Look for these patterns in the output:"
echo "  [FEC] Miss       - Cache line missed"
echo "  [FEC] Stall      - Miss caused stall"
echo "  [FEC] Retire     - Instructions retired"
echo "  [FEC] Detected   - FEC line confirmed!"
echo "  [FEC Summary]    - Statistics every 10k cycles"
echo ""

# Run CoreMark with FEC tracking (show live output + save to file)
./build/emu -i ./ready-to-run/coremark-2-iteration.bin 2>&1 | tee fec-coremark-test.log

echo ""
echo "=================================================="
echo "Step 3: Analyzing Results"
echo "=================================================="
echo ""

# Count FEC events
MISS_COUNT=$(grep "\[FEC\] Miss" fec-coremark-test.log 2>/dev/null | wc -l)
STALL_COUNT=$(grep "\[FEC\] Stall" fec-coremark-test.log 2>/dev/null | wc -l)
RETIRE_COUNT=$(grep "\[FEC\] Retire" fec-coremark-test.log 2>/dev/null | wc -l)
FEC_COUNT=$(grep "\[FEC\] Detected FEC Line" fec-coremark-test.log 2>/dev/null | wc -l)

echo "Results:"
echo "  Cache Misses Tracked:    $MISS_COUNT"
echo "  Stalls Detected:         $STALL_COUNT"
echo "  Retirements Observed:    $RETIRE_COUNT"
echo "  FEC Lines Identified:    $FEC_COUNT"
echo ""

if [ "$FEC_COUNT" -gt 0 ]; then
    echo "✅ FEC tracking is WORKING!"
    echo ""
    echo "Sample FEC lines detected:"
    grep "\[FEC\] Detected FEC Line" fec-coremark-test.log | head -10
    echo ""
    echo "Most frequent FEC addresses:"
    grep "\[FEC\] Detected" fec-coremark-test.log | \
        sed 's/.*blkPaddr=\(0x[0-9a-f]*\).*/\1/' | \
        sort | uniq -c | sort -rn | head -5
    echo ""
else
    echo "⚠️  No FEC lines detected."
    echo ""
    echo "Checking if FEC tracker is active..."
    SUMMARY_COUNT=$(grep "\[FEC Summary\]" fec-coremark-test.log 2>/dev/null | wc -l)
    
    if [ "$SUMMARY_COUNT" -gt 0 ]; then
        echo "✅ FEC tracker IS running (found $SUMMARY_COUNT summary messages)"
        echo ""
        echo "Last summary:"
        grep "\[FEC Summary\]" fec-coremark-test.log | tail -1
        echo ""
        echo "This means FEC tracker is working but CoreMark didn't trigger"
        echo "conditions that would create FEC lines (miss + stall + retire)."
    else
        echo "❌ FEC tracker may not be active - no summary messages found"
        echo ""
        echo "Checking for any FEC debug output..."
        if grep -q "\[FEC\]" fec-coremark-test.log; then
            echo "Found some FEC messages:"
            grep "\[FEC\]" fec-coremark-test.log | head -5
        else
            echo "No FEC messages found at all. printf may not be enabled."
        fi
    fi
fi

echo ""
echo "Full log available at: fec-coremark-test.log"
echo "Use: grep '\[FEC\]' fec-coremark-test.log | less"

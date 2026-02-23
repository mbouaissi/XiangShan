# FEC Tracker Testing

This directory contains tests for the Front-End Critical (FEC) Line Tracker.

## Test Overview

The `FECTrackerTest.scala` contains comprehensive tests for the FECTracker module, verifying:

1. **Complete FEC Detection**: Tests that FEC lines are correctly identified when all three conditions are met:
   - Cache miss
   - Frontend stall
   - Instruction retirement

2. **Partial Condition Tests**: Verifies that FEC lines are NOT detected when any condition is missing

3. **Multiple Concurrent Commits**: Tests handling of multiple retirements per cycle (up to CommitWidth)

4. **Flush Behavior**: Ensures all entries are cleared on flush

5. **Tracker Full Condition**: Tests behavior when tracker reaches capacity

6. **Entry Aging**: Verifies that old entries are evicted after threshold

7. **Performance Counters**: Checks that statistics are correctly tracked

## Running the Tests

### Using Mill (if XiangShan uses Mill):
```bash
cd /raid1/menzo/xs-env/XiangShan
mill XiangShan.test.testOnly xiangshan.frontend.icache.FECTrackerTest
```

### Using SBT:
```bash
cd /raid1/menzo/xs-env/XiangShan
sbt "testOnly xiangshan.frontend.icache.FECTrackerTest"
```

### Run specific test:
```bash
sbt "testOnly xiangshan.frontend.icache.FECTrackerTest -- -z \"detect FEC line when miss\""
```

## Test Cases

### 1. Complete FEC Lifecycle
```
Cycle 1: Report miss (ftqIdx=10, blkPaddr=0x1000)
Cycle 4: Report stall (ftqIdx=10)
Cycle 7: Report retire (ftqIdx=10)
Expected: FEC line detected with correct address
```

### 2. Missing Stall Condition
```
Cycle 1: Report miss
Cycle 4: Skip stall, report retire directly
Expected: NO FEC line detected
```

### 3. Missing Retire Condition
```
Cycle 1: Report miss
Cycle 4: Report stall
Cycle 9: No retire
Expected: NO FEC line detected
```

### 4. Flush Behavior
```
Cycle 1: Report miss
Cycle 3: Flush
Cycle 5: Report stall and retire
Expected: NO FEC line detected (entry was flushed)
```

## Expected Output

When tests pass, you should see:
```
[Cycle 1] Miss reported for ftqIdx=10
[Cycle 4] Stall reported for ftqIdx=10
[Cycle 7] Retire reported for ftqIdx=10, FEC detected: true
✓ FEC line correctly detected: blkPaddr=0x1000, vSetIdx=0x10
[No Stall Test] FEC detected: false (should be false)
[No Retire Test] FEC detected: false (should be false)
...
[info] FECTrackerTest:
[info] FECTracker
[info] - should detect FEC line when miss + stall + retire conditions are met
[info] - should NOT detect FEC line if stall condition is missing
[info] - should NOT detect FEC line if retire condition is missing
[info] - should handle multiple concurrent commits
[info] - should clear all entries on flush
[info] - should handle tracker full condition
[info] - should age out old entries
[info] - should correctly track performance counters
[info] Run completed in X seconds.
[info] Total number of tests run: 8
[info] Suites: completed 1, aborted 0
[info] Tests: succeeded 8, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
```

## Debugging

If tests fail, check:

1. **FECTracker Logic**: Verify the detection logic in FECTracker.scala
2. **Signal Timing**: Ensure signals are sampled at correct cycles
3. **FtqPtr Comparison**: Verify FtqPtr equality checks work correctly
4. **Entry Allocation**: Check that entries are properly allocated in the tracker

## Performance Counter Verification

The testbench checks these counters at the end:
- `totalMisses`: Should increment on each newMiss
- `missesWithStall`: Should increment when both miss and stall occur
- `fecLinesDetected`: Should increment when all 3 conditions met
- `trackerFull`: Should increment when trying to allocate with no free entries

## Integration Testing

For full system testing, you can:

1. Run XiangShan simulation with FEC tracking enabled
2. Monitor `io.fecLine.valid` signal
3. Check that detected FEC lines correspond to actual critical misses
4. Compare performance counters with expected values from workload

## Notes

- Tests use a 16-entry tracker by default (same as production)
- Aging threshold is 1024 cycles (configurable in FECTracker)
- CommitWidth is typically 6 in XiangShan
- FtqPtr uses flag + value structure for circular queue management

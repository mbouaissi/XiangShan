# PDIP (Prefetch-Directed Instruction Prefetching) Implementation

## Overview

PDIP is a novel instruction prefetching technique that works in synergy with FDIP (Fetch-Directed Instruction Prefetching). While FDIP prefetches along the predicted path, PDIP targets specific instruction cache misses that FDIP fails to hide - particularly those that occur after pipeline flush events like branch mispredictions.

## Key Concepts

### FEC (Front-End Critical) Lines

FEC lines are instruction cache lines that:
1. **Missed** in the L1 instruction cache
2. **Stalled** the frontend pipeline
3. Had at least one instruction **retire**

These three conditions ensure we only track lines that are truly performance-critical.

### PDIP Learning Mechanism

PDIP learns **trigger-candidate associations**:
- **Trigger**: The instruction block address that caused a pipeline disruption (e.g., mispredicted branch)
- **Candidate**: An FEC cache line that subsequently missed and stalled the frontend

When the trigger instruction is encountered again during execution, PDIP proactively prefetches the associated FEC lines.

## Architecture Components

### 1. PDIPTable

A set-associative table storing trigger → FEC target mappings.

**Structure**:
- Configurable number of sets and ways (default: 64 sets × 4 ways)
- Each entry can store multiple targets per trigger (default: 4 targets)
- 2-bit confidence counter per target
- LRU replacement policy for targets

**Operations**:
- `lookup`: Match current instruction block  address against stored triggers
- `allocate`: Add new trigger-target associations from FEC detections

### 2. Prefetch Queue (PQ)

Decouples PDIP prefetch requests from ICache MSHR allocation.

**Features**:
- FIFO queue (default: 8 entries)
- Only issues prefetches when MSHR resources are available
- Prevents prefetches from interfering with demand requests

### 3. PDIP Controller

Main control logic coordinating the components.

**Responsibilities**:
- Receives current fetch address as potential trigger
- Probes PDIP table for matches
- Enqueues high-confidence targets to PQ
- Learns from FEC line detections

## Configuration Parameters

```scala
case class PDIPParams(
  enabled: Boolean = true,              // Enable/disable PDIP
  numTableSets: Int = 64,               // PDIP table sets
  numWaysPerSet: Int = 4,               // Associativity
  numTargetsPerEntry: Int = 4,          // Max targets per trigger
  prefetchQueueSize: Int = 8,           // PQ size
  mshrCheckEnabled: Boolean = true      // Check MSHR before prefetch
)
```

### Tuning Guidelines

- **numTableSets**: Increase for larger working sets to reduce aliasing
- **numTargetsPerEntry**: Increase if triggers correlate with many FEC lines
- **prefetchQueueSize**: Increase if MSHR availability is frequently blocking prefetches

## Integration Points

### FECTracker Enhancement

The existing `FECTracker` was enhanced to provide trigger address information:

```scala
// FEC line output now includes trigger
val fecLine = ValidIO(new Bundle {
  val blkPaddr = UInt(...)        // FEC line address
  val vSetIdx = UInt(...)         // Set index
  val triggerAddr = UInt(...)     // NEW: Trigger that caused this FEC
})
```

### ICache Integration

PDIP controller is instantiated in `ICache` and connected to:

1. **Input: Current fetch address** (trigger)
   - From `io.fetch.req.bits.f0addr`

2. **Input: FEC line detections** (learning)
   - From `fecTracker.io.fecLine`

3. **Output: Prefetch requests**
   - To `missUnit.io.prefetch_req` (via prefetch MSHR path)

4. **MSHR availability check**
   - From `missUnit.io.mshr_avail`

## Operation Flow

### Learning Phase

```
1. Branch misprediction occurs at PC = 0x1000
2. Frontend flushes, starts fetching from correct path
3. Cache miss occurs for line 0x2000
4. Miss stalls frontend → FECTracker detects this
5. Instructions retire → FECTracker fires FEC signal
6. PDIP allocates: trigger(0x1000) → target(0x2000)
```

### Prefetching Phase

```
1. Fetch reaches PC = 0x1000 again
2. PDIP controller probes table with trigger = 0x1000
3. Table hits, returns target = 0x2000 (confidence ≥ 2)
4. Target enqueued in Prefetch Queue
5. When MSHR available, prefetch issued for 0x2000
6. Later demand fetch to 0x2000 hits in cache!
```

## Performance Monitoring

PDIP provides performance counters:

```scala
perfInfo.totalPrefetches    // Total prefetches issued
perfInfo.tableLookups       // Table probes
perfInfo.tableHits          // Successful trigger matches
perfInfo.queueFull          // PQ full stalls
```

## Testing

Run PDIP tests:
```bash
cd /raid1/menzo/xs-env/XiangShan
mill -i xiangshan.test.testOnly xiangshan.frontend.icache.PDIPTest
```

## Future Enhancements

### Trigger Accuracy
Currently uses current fetch address as trigger - could be enhanced to:
- Track actual mispredicted branch PC
- Use redirect source from BPU
- Multiple trigger types (branch, return, indirect jump)

### Confidence Mechanism
- Currently uses simple 2-bit saturating counter with **decay**
  (counters are decremented when entries are updated to age stale targets)
- Negative confidence for harmful prefetches

### Coverage Tracking
- Track which FEC lines are covered by PDIP
- Identify uncovered patterns for further optimization

### Multi-level Prefetching
- Prefetch to L2 instead of L1 for certain patterns
- Coordinate with L2 prefetcher

## References

[19] Based on the PDIP concept from recent computer architecture research on instruction prefetching techniques that complement FDIP.

## Implementation Status

- ✅ Core PDIP components implemented
- ✅ Integration with FECTracker
- ✅ ICache integration
- ✅ Basic test suite
- ⏳ Performance evaluation pending
- ⏳ Trigger source refinement needed
- ⏳ Parameter tuning for production

## Contact

For questions or issues with the PDIP implementation, refer to the XiangShan project documentation.

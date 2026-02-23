# FEC Tracker Integration Status

## Completed Steps

### 1. FECTracker Module (✅ Complete)
- **File**: `FECTracker.scala`
- **Status**: Fully implemented with 16-entry tracking table
- **Features**:
  - Tracks cache lines through miss → stall → retire lifecycle
  - Aging policy (1024 cycle threshold) to prevent stale entries
  - Performance counters for monitoring
  - Interfaces: newMiss, stallUpdate, retireUpdate, flush

### 2. IPrefetch Integration (✅ Complete)
- **File**: `IPrefetch.scala`
- **Changes**:
  - Added `newMiss` output port to `IPrefetchIO` (line 70)
  - Added `s2_req_ftqIdx` register to track FTQ index at stage 2 (line 502)
  - Wired `io.newMiss` signal when miss goes to MSHR (lines 589-593)
  - Sends: blkPaddr, vSetIdx, ftqIdx when `toMSHR.fire`

### 3. ICache Integration (✅ Partial - Basic Structure)
- **File**: `ICache.scala` 
- **Changes**:
  - Instantiated FECTracker module (line 611)
  - Connected prefetcher's newMiss output to FECTracker (line 722)
  - Added flush signal connection (line 725)
  - **TODO markers** for stallUpdate and retireUpdate (lines 723-724)

## Remaining Work

### Phase 2: IFU Stall Attribution (⏳ Pending)
**Goal**: Connect IFU stall signals to FECTracker

**Required Changes**:
1. **Locate IFU Module**:
   - Find `IFU.scala` or equivalent frontend module
   - Identify where ICache stalls cause IFU stalls
   
2. **Add Stall Attribution**:
   - When ICache misses cause IFU to stall, identify which cache line
   - Need to track: `blkPaddr`, `vSetIdx` of stalling line
   
3. **Wire to ICache**:
   - Option A: Add `ifuStall` input to `ICacheIO` 
   - Option B: Pass through mainPipe's stall signals
   - Connect to `fecTracker.io.stallUpdate`

**Key Question**: Does IFU already track which cache line caused the stall?

### Phase 3: FTQ Retirement Notification (⏳ Pending)
**Goal**: Notify FECTracker when FTQ entries retire

**Required Changes**:
1. **Locate Ftq Module**:
   - Find `Ftq.scala` in frontend
   - Identify commit/retirement logic
   
2. **Add Retirement Output**:
   - Create output port in FtqIO: `retireNotify: Valid[FtqPtr]`
   - Fire when FTQ entry commits
   
3. **Route to ICache**:
   - Add to `ICacheIO`: `ftqRetire: Valid[FtqPtr]`
   - From top-level frontend, connect FTQ → ICache
   - Wire `io.ftqRetire` to `fecTracker.io.retireUpdate`

**Key Question**: Does FTQ retirement already have a broadcast mechanism?

### Phase 4: Performance Counters (⏳ Pending)
**Goal**: Expose FEC statistics for monitoring

**Changes**:
1. Add to `ICachePerfInfo`:
   ```scala
   val fecTotalMisses      = UInt(64.W)
   val fecMissesWithStall  = UInt(64.W) 
   val fecLinesDetected    = UInt(64.W)
   val fecTrackerFull      = UInt(64.W)
   ```

2. In `ICache.scala`, wire FECTracker counters:
   ```scala
   io.perfInfo.fecTotalMisses     := fecTracker.io.perfCounters.totalMisses
   io.perfInfo.fecMissesWithStall := fecTracker.io.perfCounters.missesWithStall
   // ... etc
   ```

### Phase 5: Testing & Validation (⏳ Pending)
**Tasks**:
- [ ] Build XiangShan with new code
- [ ] Run simulation with test workloads
- [ ] Verify FEC lines are correctly identified
- [ ] Check performance counter values make sense
- [ ] Test flush/reset behavior

### Phase 6: Prefetch Integration (⏳ Future)
**Goal**: Use FEC information to guide prefetching

**Ideas**:
- Prioritize prefetching for FEC lines
- Adjust prefetch confidence based on FEC status
- Prefetch on FEC line replacement

## File Dependencies

```
FECTracker.scala (standalone module)
    ↑
    |
IPrefetch.scala (emits newMiss) → ICache.scala → (needs) → IFU stall signals
                                  connects FECTracker      → FTQ retire signals
```

## Architecture Notes

### Current Signal Flow:
1. **Miss Detection**: IPrefetch stage 2 detects miss
2. **Miss Notification**: IPrefetch.io.newMiss fires with (blkPaddr, vSetIdx, ftqIdx)
3. **Tracking**: FECTracker allocates entry, marks as missed
4. **Stall Update**: (TODO) IFU notifies FECTracker when stall occurs
5. **Retire Update**: (TODO) FTQ notifies FECTracker when ftqIdx retires
6. **FEC Detection**: FECTracker marks line as FEC when all 3 conditions met

### Missing Links:
- **IFU → ICache**: Stall notification with cache line address
- **FTQ → ICache**: Retirement notification with ftqIdx
- **ICache → Top-level**: Performance counter propagation

## Testing Checklist

- [ ] **Compilation**: Does the code compile without errors?
- [ ] **Simulation**: Can XiangShan run with the new module?
- [ ] **Miss Tracking**: Are cache misses being tracked in FECTracker?
- [ ] **Stall Attribution**: Do stalls get correctly attributed? (after Phase 2)
- [ ] **Retirement Tracking**: Are retirements correctly recorded? (after Phase 3)
- [ ] **FEC Detection**: Are FEC lines correctly identified? (after Phase 2+3)
- [ ] **Performance**: No negative impact on IPC or critical path?
- [ ] **Counters**: Do performance counters show reasonable values?

## Open Questions

1. **IFU Stall Mechanism**: 
   - How does IFU currently detect ICache-induced stalls?
   - Is there already a signal indicating "stalled on ICache miss"?
   - Can we easily identify which cache line caused the stall?

2. **FTQ Retirement**:
   - Does FTQ already broadcast retirement to other modules?
   - Is there a `ftqPtr → ICache address` mapping available?
   - Should we track per-instruction retirement or per-FTQ-entry?

3. **Frontend Integration**:
   - Where is the top-level frontend module that connects IFU, ICache, FTQ?
   - Should we add new inter-module wires, or use existing buses?

4. **Prefetch Policy**:
   - How should FEC information influence the existing prefetcher?
   - Should we prioritize FEC lines in the MSHR or prefetch queue?
   - What's the threshold for considering a line "critical"?

## Next Immediate Steps

1. **Find IFU Module**: 
   ```bash
   find . -name "IFU.scala" -o -name "*IFU*.scala"
   grep -r "class.*IFU" --include="*.scala"
   ```

2. **Understand IFU-ICache Interface**:
   - Read `IfuToICacheIO` bundle definition
   - Identify stall signals: `io.stop`, pipeline ready signals
   - See if there's already miss address tracking

3. **Find FTQ Module**:
   ```bash
   find . -name "Ftq.scala" 
   grep -r "retirement\|commit" XiangShan/src/main/scala/xiangshan/frontend/
   ```

4. **Understand FTQ Interface**:
   - See how FTQ communicates with other modules
   - Find where FTQ entries are marked as retired/committed
   - Check if there's existing broadcast mechanism

## Summary

**What Works Now**:
- ✅ FECTracker module creates tracking infrastructure
- ✅ IPrefetch detects misses and notifies FECTracker
- ✅ Basic wiring in ICache established

**What's Missing**:
- ❌ IFU stall notifications (can't detect "stalled" condition)
- ❌ FTQ retirement notifications (can't detect "retired" condition)
- ❌ Performance counter exposure
- ❌ Testing and validation

**Critical Path**: Phases 2 & 3 must be completed before FEC detection works correctly.

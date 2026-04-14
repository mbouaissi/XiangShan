package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan.frontend._

/** FEC Miss Information - signals a new cache miss to track */
class FECMissInfo(implicit p: Parameters) extends ICacheBundle {
  val blkPaddr = UInt((PAddrBits - blockOffBits).W)
  val vSetIdx = UInt(idxBits.W)
  val ftqIdx = new FtqPtr
}

/** FEC Stall Information - signals that an FTQ entry caused a stall */
class FECStallInfo(implicit p: Parameters) extends ICacheBundle {
  val ftqIdx = new FtqPtr
  val stalled = Bool()
}

/** FEC Retire Information - signals that an FTQ entry retired */
class FECRetireInfo(implicit p: Parameters) extends ICacheBundle {
  val ftqIdx = new FtqPtr
}

/** FEC Line Candidate Entry
  */
class FECCandidateEntry(implicit p: Parameters) extends ICacheBundle {
  val valid: Bool = Bool() // entry allocated
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W) // block address
  val vSetIdx: UInt = UInt(idxBits.W) // ICache set index
  val triggerAddr: UInt = UInt((PAddrBits - blockOffBits).W) // trigger captured at miss time
  val ftqIdx: FtqPtr = new FtqPtr // FTQ entry that caused the miss
  val allocTime: UInt = UInt(64.W) // Cycle when allocated, for aging. Basically, if too old, just free it.
  val allocRetiredInstrs: UInt = UInt(64.W) // Global retired-instruction count when the miss was allocated.
  val stalledIFU: Bool = Bool() // Has caused a stall
  val retired: Bool = Bool() // Has had at least one instruction retire
  val stallCycles: UInt = UInt(4.W) // Saturating count of starvation cycles
}

/** FEC Tracker Performance Counters
  */
class FECTrackerPerfInfo(implicit p: Parameters) extends ICacheBundle {
  val totalMisses: UInt = UInt(64.W)
  val missesWithStall: UInt = UInt(64.W)
  val fecLinesDetected: UInt = UInt(64.W)
  val trackerFull: UInt = UInt(64.W)
}

/** FEC Tracker IO Bundle
  */
class FECTrackerIO(implicit p: Parameters) extends ICacheBundle {
  // Input: New cache miss to track
  val newMiss = Flipped(ValidIO(new FECMissInfo))

  // Input: Update stall status for an FTQ entry
  val stallUpdate = Flipped(ValidIO(new FECStallInfo))

  // Input: Retirement notification from FTQ/Backend
  val retireUpdate = Vec(CommitWidth, Flipped(ValidIO(new FECRetireInfo)))

  // Output: Confirmed FEC line (miss + stall + retired)
  val fecLine = ValidIO(new Bundle {
    val blkPaddr = UInt((PAddrBits - blockOffBits).W)
    val vSetIdx = UInt(idxBits.W)
    val triggerAddr =
      UInt((PAddrBits - blockOffBits).W) // Block that caused redirect/mispred
    val highCost = Bool() // Decode starvation lasted at least 10 cycles
    val starvationDistance = UInt(16.W) // Approximate retired instructions from miss allocation to FEC retirement.
  })

  // Trigger address 
  val triggerAddr = Input(UInt((PAddrBits - blockOffBits).W))

  // Flush - only for fence.i (instruction cache invalidation)
  // Regular pipeline flushes (branch mispredictions) should NOT clear FEC tracker
  val fencei = Input(Bool())

  // Performance information
  val perfInfo = Output(new FECTrackerPerfInfo)
}

/** FEC Line Tracker
  *
  * Tracks cache lines through their lifecycle to identify FEC lines. A line is
  * FEC if it:
  *   1. Missed in the instruction cache
  *   2. Caused front-end stalls
  *   3. Had at least one instruction retire
  * Cover the following scenarios:
  *   - Missed -> Stall -> Retire (Normal order)
  *   - Missed -> Retire -> Stall (late stall )
  */
class FECTracker(numEntries: Int = 16)(implicit p: Parameters)
    extends ICacheModule {
  val io: FECTrackerIO = IO(new FECTrackerIO)

  // Storage for tracking candidates (SRAM-backed)
  private val entriesSram = Module(
    new SRAMTemplate(
      new FECCandidateEntry,
      set = 1,
      way = numEntries,
      shouldReset = true,
      holdRead = true,
      singlePort = false
    )
  )
  entriesSram.io.r.req.valid := true.B
  entriesSram.io.r.req.bits.setIdx := 0.U.asTypeOf(entriesSram.io.r.req.bits.setIdx)

  private val entriesRead = entriesSram.io.r.resp.data
  private val entriesNext = Wire(Vec(numEntries, new FECCandidateEntry))
  entriesNext := entriesRead

  // The SRAM may output garbage on the first numEntries cycles before the
  // shouldReset write-sweep completes.  Force all valid bits low during that
  // window so no garbage entry can trigger aging, retire-match, or FEC
  // detection logic.
  private val initCounter = RegInit(0.U(log2Ceil(numEntries + 1).W))
  private val initializing = initCounter < numEntries.U
  when(initializing) {
    initCounter := initCounter + 1.U
    entriesNext.foreach(_.valid := false.B)
  }

  // FEC detection state (must be defined before fireFEC uses them)
  // Use registers to hold FEC detection results for one cycle
  private val fecDetected = RegInit(false.B)
  private val fecDetectedIdx = RegInit(0.U(log2Ceil(numEntries).W))
  private val fecBlkPaddr = RegInit(0.U((PAddrBits - blockOffBits).W))
  private val fecVSetIdx = RegInit(0.U(idxBits.W))
  private val fecTriggerAddr = RegInit(0.U((PAddrBits - blockOffBits).W))
  private val fecHighCost = RegInit(false.B)
  private val fecStarvationDistance = RegInit(0.U(16.W))

  // Wire to indicate if FEC is being detected this cycle
  private val fecDetectThisCycle = WireInit(false.B)
  private val fecDetectIdx = WireInit(0.U(log2Ceil(numEntries).W))
  private val fecDetectBlkPaddr = WireInit(0.U((PAddrBits - blockOffBits).W))
  private val fecDetectVSetIdx = WireInit(0.U(idxBits.W))
  private val fecDetectTriggerAddr = WireInit(0.U((PAddrBits - blockOffBits).W))
  private val fecDetectHighCost = WireInit(false.B)
  private val fecDetectStarvationDistance = WireInit(0.U(16.W))

  private def fireFEC(entry: FECCandidateEntry, i: Int): Unit = {
    val retiredDistanceWide = cycleRetiredInstrs - entry.allocRetiredInstrs
    fecDetectThisCycle := true.B
    fecDetectIdx := i.U
    fecDetectBlkPaddr := entry.blkPaddr
    fecDetectVSetIdx := entry.vSetIdx
    fecDetectTriggerAddr := entry.triggerAddr
    fecDetectHighCost := entry.stallCycles >= 10.U
    fecDetectStarvationDistance :=
      Mux(
        retiredDistanceWide > ((1 << 16) - 1).U,
        ((1 << 16) - 1).U,
        retiredDistanceWide(15, 0)
      )

    entriesNext(i).valid := false.B
    perfFECLinesDetected := perfFECLinesDetected + 1.U
  }

  // Cycle counter for aging
  private val cycleCounter = RegInit(0.U(64.W))
  cycleCounter := cycleCounter + 1.U
  private val cycleRetiredInstrs = RegInit(0.U(64.W))
  private val committedThisCycle = PopCount(io.retireUpdate.map(_.valid))
  cycleRetiredInstrs := cycleRetiredInstrs + committedThisCycle

  // Performance counters for debugging,
  // TODO: REMOVE
  private val perfTotalMisses = RegInit(0.U(64.W))
  private val perfMissesWithStall = RegInit(0.U(64.W))
  private val perfFECLinesDetected = RegInit(0.U(64.W))
  private val perfTrackerFull = RegInit(0.U(64.W))

  /** Has missed
    */
  private val freeEntryVec = VecInit(entriesRead.map(entry => !entry.valid))
  private val hasFreeEntry = freeEntryVec.asUInt.orR
  private val freeEntryIdx = PriorityEncoder(freeEntryVec)

  when(io.newMiss.valid) {
    perfTotalMisses := perfTotalMisses + 1.U // Count all miss events

    val hitVec = VecInit(
      entriesRead.map(e => e.valid && (e.ftqIdx === io.newMiss.bits.ftqIdx))
    ) // Check if this miss is already being tracked
    val hasHit = hitVec.asUInt.orR // Reduce to see if there's any hit
    val hitIdx = PriorityEncoder(hitVec)

    when(
      hasHit
    ) {
      entriesNext(hitIdx).blkPaddr := io.newMiss.bits.blkPaddr
      entriesNext(hitIdx).vSetIdx := io.newMiss.bits.vSetIdx
      entriesNext(hitIdx).triggerAddr := io.triggerAddr
      entriesNext(hitIdx).allocTime := cycleCounter // refresh allocation time on new miss for same FTQ entry
      entriesNext(hitIdx).allocRetiredInstrs := cycleRetiredInstrs
      entriesNext(hitIdx).stallCycles := 0.U

      // Debug: Log duplicate miss
      printf(
        "[FEC] Miss (update): ftqIdx=%d blkPaddr=0x%x cycle=%d\n",
        io.newMiss.bits.ftqIdx.value,
        io.newMiss.bits.blkPaddr,
        cycleCounter
      )

    }.elsewhen(hasFreeEntry) {
      entriesNext(freeEntryIdx).valid := true.B
      entriesNext(freeEntryIdx).blkPaddr := io.newMiss.bits.blkPaddr
      entriesNext(freeEntryIdx).vSetIdx := io.newMiss.bits.vSetIdx
      entriesNext(freeEntryIdx).triggerAddr := io.triggerAddr
      entriesNext(freeEntryIdx).ftqIdx := io.newMiss.bits.ftqIdx
      entriesNext(freeEntryIdx).allocTime := cycleCounter
      entriesNext(freeEntryIdx).allocRetiredInstrs := cycleRetiredInstrs
      entriesNext(freeEntryIdx).stalledIFU := false.B
      entriesNext(freeEntryIdx).retired := false.B
      entriesNext(freeEntryIdx).stallCycles := 0.U

      // Debug: Log new miss
      printf(
        "[FEC] Miss (new): ftqIdx=%d blkPaddr=0x%x cycle=%d\n",
        io.newMiss.bits.ftqIdx.value,
        io.newMiss.bits.blkPaddr,
        cycleCounter
      )

    }.otherwise {
      perfTrackerFull := perfTrackerFull + 1.U
      
      // Debug: Log tracker full with entry states
      val stalledCount = PopCount(entriesRead.map(e => e.valid && e.stalledIFU))
      val retiredCount = PopCount(entriesRead.map(e => e.valid && e.retired))
      val bothCount = PopCount(entriesRead.map(e => e.valid && e.stalledIFU && e.retired))
      
      printf(
        "[FEC] Tracker FULL (miss dropped): stalled=%d retired=%d both=%d cycle=%d\n",
        stalledCount,
        retiredCount,
        bothCount,
        cycleCounter
      )
    }
  }

  /** Caused a stall
    */
  when(io.stallUpdate.valid && !initializing) {
    entriesRead.zipWithIndex.foreach { case (entry, i) =>
      when(entry.valid && entry.ftqIdx === io.stallUpdate.bits.ftqIdx) {
        val wasStalled = entry.stalledIFU
        entriesNext(i).stalledIFU := io.stallUpdate.bits.stalled
        when(io.stallUpdate.bits.stalled && entry.stallCycles =/= 15.U) {
          entriesNext(i).stallCycles := entry.stallCycles + 1.U
        }

        // If this is a new stall indication for an allocated entry, count it
        when(io.stallUpdate.bits.stalled && !wasStalled) {
          perfMissesWithStall := perfMissesWithStall + 1.U
          
          // Debug: Log stall event
          printf(
            "[FEC] Stall: ftqIdx=%d blkPaddr=0x%x retired=%d cycle=%d\n",
            entry.ftqIdx.value,
            entry.blkPaddr,
            entry.retired,
            cycleCounter
          )
        }

        // If this entry is now stalled, check if it has already retired to fire FEC
        when(io.stallUpdate.bits.stalled && !wasStalled && entry.retired) {
          fireFEC(entry, i)
        }

      }
    }
  }

  /** Was retired
    */
  // Debug: Log all incoming retire updates
  (0 until CommitWidth).foreach { w =>
    when(io.retireUpdate(w).valid && cycleCounter % 1000.U === 0.U) {
      printf(
        "[FEC] Retire signal: slot=%d ftqIdx=%d cycle=%d\n",
        w.U,
        io.retireUpdate(w).bits.ftqIdx.value,
        cycleCounter
      )
    }
  }

  // Process all retirement updates 
  (0 until CommitWidth).foreach { w =>
    when(io.retireUpdate(w).valid) {
      val hitVec = VecInit(
        entriesRead.map(e => e.valid && (e.ftqIdx === io.retireUpdate(w).bits.ftqIdx))
      )
      val hasHit = hitVec.asUInt.orR
      val hitIdx = PriorityEncoder(hitVec)

      when(!hasHit && cycleCounter % 1000.U === 0.U) {
        // Debug: Log retire miss (no matching entry)
        printf(
          "[FEC] Retire NO MATCH: ftqIdx=%d cycle=%d\n",
          io.retireUpdate(w).bits.ftqIdx.value,
          cycleCounter
        )
      }

      entriesRead.zipWithIndex.foreach { case (entry, i) =>
        when(!initializing && entry.valid && entry.ftqIdx === io.retireUpdate(w).bits.ftqIdx) {
          val wasRetired = entry.retired
          entriesNext(i).retired := true.B

          // Debug: Log retire event
          when(!wasRetired) {
            printf(
              "[FEC] Retire MATCH: ftqIdx=%d blkPaddr=0x%x stalled=%d cycle=%d\n",
              entry.ftqIdx.value,
              entry.blkPaddr,
              entry.stalledIFU,
              cycleCounter
            )
          }

          // Check if this is now an FEC line: miss + stall + retired
          when(entry.stalledIFU && !wasRetired) {
            fireFEC(entry, i)
          }.elsewhen(!entry.stalledIFU && !wasRetired) {
            // Entry retired without ever stalling: stall always precedes retire in
            // normal flow (Miss → Stall → Fill → Retire), so this entry will
            // never become FEC.  Free the slot so it doesn't clog the tracker.
            entriesNext(i).valid := false.B
            printf(
              "[FEC] Retire (no stall, freeing): ftqIdx=%d blkPaddr=0x%x cycle=%d\n",
              entry.ftqIdx.value,
              entry.blkPaddr,
              cycleCounter
            )
          }

        }
      }
    }
  }

  // Update FEC detection registers
  when(fecDetectThisCycle) {
    fecDetected := true.B
    fecDetectedIdx := fecDetectIdx
    fecBlkPaddr := fecDetectBlkPaddr
    fecVSetIdx := fecDetectVSetIdx
    fecTriggerAddr := fecDetectTriggerAddr
    fecHighCost := fecDetectHighCost
    fecStarvationDistance := fecDetectStarvationDistance
  }.otherwise {
    fecDetected := false.B
  }

  // Output FEC line signal
  io.fecLine.valid := fecDetected
  io.fecLine.bits.blkPaddr := fecBlkPaddr
  io.fecLine.bits.vSetIdx := fecVSetIdx
  io.fecLine.bits.triggerAddr := fecTriggerAddr
  io.fecLine.bits.highCost := fecHighCost
  io.fecLine.bits.starvationDistance := fecStarvationDistance

  // Debug: Log FEC line detections
  when(fecDetected) {
    printf(
      "[FEC] Detected FEC Line: blkPaddr=0x%x vSetIdx=0x%x triggerAddr=0x%x highCost=%d starvationDistance=%d cycle=%d\n",
      fecBlkPaddr,
      fecVSetIdx,
      fecTriggerAddr,
      fecHighCost,
      fecStarvationDistance,
      cycleCounter
    )
  }

  /** Entry Aging & Eviction Free entries that are too old (likely stale due to
    * flush/redirect) Use the aging mechanism to automatically clean up entries
    * that might never get retired
    */
  private val agingThreshold = 50000.U // Cycles before considering entry stale (increased from 1024)
  entriesRead.zipWithIndex.foreach { case (entry, i) =>
    when(!initializing && entry.valid && (cycleCounter - entry.allocTime) > agingThreshold) {
      entriesNext(i).valid := false.B
      
      // Debug: Log aged entries
      printf(
        "[FEC] Aged out: ftqIdx=%d blkPaddr=0x%x stalled=%d retired=%d age=%d\n",
        entry.ftqIdx.value,
        entry.blkPaddr,
        entry.stalledIFU,
        entry.retired,
        cycleCounter - entry.allocTime
      )
    }
  }

  /** Flush Handling 
    * 
    * IMPORTANT: Only flush on fence.i, NOT on regular pipeline flushes!
    * 
    * Rationale:
    * - Pipeline flushes (branch mispredictions) happen frequently (~4999 times in CoreMark)
    * - But cache misses and stalls are still valid even after a flush
    * - We want to track FEC lines across executions to learn patterns
    * - Only fence.i (which invalidates the entire ICache) should clear tracker
    * - Stale entries will age out naturally via the aging mechanism
    */
  when(io.fencei) {
    // Count entries in different states before flushing
    val validCount = PopCount(entriesRead.map(_.valid))
    val stalledCount = PopCount(entriesRead.map(e => e.valid && e.stalledIFU))
    val retiredCount = PopCount(entriesRead.map(e => e.valid && e.retired))
    val bothCount = PopCount(entriesRead.map(e => e.valid && e.stalledIFU && e.retired))
    
    printf(
      "[FEC] FENCEI: clearing %d entries (stalled=%d retired=%d both=%d) cycle=%d\n",
      validCount,
      stalledCount,
      retiredCount,
      bothCount,
      cycleCounter
    )
    
    entriesRead.zipWithIndex.foreach { case (entry, i) =>
      entriesNext(i).valid := false.B
    }
  }

  /** Performance Counter Outputs
    */
  io.perfInfo.totalMisses := perfTotalMisses
  io.perfInfo.missesWithStall := perfMissesWithStall
  io.perfInfo.fecLinesDetected := perfFECLinesDetected
  io.perfInfo.trackerFull := perfTrackerFull

  // Debug: Print periodic summary (every 10000 cycles)
  when(cycleCounter % 10000.U === 0.U && cycleCounter =/= 0.U) {
    val validCount = PopCount(entriesRead.map(_.valid))
    val stalledOnlyCount = PopCount(entriesRead.map(e => e.valid && e.stalledIFU && !e.retired))
    val retiredOnlyCount = PopCount(entriesRead.map(e => e.valid && !e.stalledIFU && e.retired))
    val bothCount = PopCount(entriesRead.map(e => e.valid && e.stalledIFU && e.retired))
    val neitherCount = PopCount(entriesRead.map(e => e.valid && !e.stalledIFU && !e.retired))
    
    printf(
      "[FEC Summary] Cycle %d: Misses=%d StallMisses=%d FECLines=%d TrackerFull=%d\n",
      cycleCounter,
      perfTotalMisses,
      perfMissesWithStall,
      perfFECLinesDetected,
      perfTrackerFull
    )
    printf(
      "[FEC State] Valid=%d StalledOnly=%d RetiredOnly=%d Both=%d Neither=%d\n",
      validCount,
      stalledOnlyCount,
      retiredOnlyCount,
      bothCount,
      neitherCount
    )
  }

  private val entriesChanged = Wire(Vec(numEntries, Bool()))
  when(reset.asBool) {
    entriesChanged := VecInit(Seq.fill(numEntries)(false.B))
  }.otherwise {
    entriesChanged := VecInit(entriesRead.zip(entriesNext).map {
      case (prev, next) => prev.asUInt =/= next.asUInt
    })
  }
  private val entriesWriteMask = entriesChanged.asUInt
  private val entriesWriteValid = entriesWriteMask.orR

  entriesSram.io.w.req.valid := entriesWriteValid
  entriesSram.io.w.req.bits.apply(
    data = entriesNext,
    setIdx = 0.U.asTypeOf(entriesSram.io.w.req.bits.setIdx),
    waymask = entriesWriteMask
  )


}

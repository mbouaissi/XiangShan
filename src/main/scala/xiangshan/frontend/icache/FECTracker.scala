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

/** Front-End Critical (FEC) Line Candidate Entry Tracks cache lines that miss
  * and potentially cause stalls
  */
class FECCandidateEntry(implicit p: Parameters) extends ICacheBundle {
  val valid: Bool = Bool() // entry allocated
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W) // block address
  val vSetIdx: UInt = UInt(idxBits.W) // L1I set index
  val ftqIdx: FtqPtr = new FtqPtr // Associated FTQ entry index
  val allocTime: UInt = UInt(64.W) // Cycle when allocated, for aging
  val stalledIFU: Bool = Bool() // Confirmed this miss stalled frontend
  val retired: Bool = Bool() // At least one instruction retired
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
  })

  // Input: Global flush signal
  val flush = Input(Bool())

  // Output: Performance information
  val perfInfo = Output(new FECTrackerPerfInfo)
}

/** Front-End Critical (FEC) Line Tracker
  *
  * Tracks cache lines through their lifecycle to identify FEC lines. A line is
  * FEC if it:
  *   1. Missed in the instruction cache
  *   2. Caused front-end stalls
  *   3. Had at least one instruction retire
  */
class FECTracker(numEntries: Int = 16)(implicit p: Parameters)
    extends ICacheModule {
  val io: FECTrackerIO = IO(new FECTrackerIO)

  // Storage for tracking candidates
  private val entries = RegInit(
    VecInit(
      Seq.fill(numEntries)(
        0.U.asTypeOf(new FECCandidateEntry)
      )
    )
  )

  // FEC detection state (must be defined before fireFEC uses them)
  // Use registers to hold FEC detection results for one cycle
  private val fecDetected = RegInit(false.B)
  private val fecDetectedIdx = RegInit(0.U(log2Ceil(numEntries).W))
  private val fecBlkPaddr = RegInit(0.U((PAddrBits - blockOffBits).W))
  private val fecVSetIdx = RegInit(0.U(idxBits.W))

  // Wire to indicate if FEC is being detected this cycle
  private val fecDetectThisCycle = WireInit(false.B)
  private val fecDetectBlkPaddr = WireInit(0.U((PAddrBits - blockOffBits).W))
  private val fecDetectVSetIdx = WireInit(0.U(idxBits.W))
  private val fecDetectIdx = WireInit(0.U(log2Ceil(numEntries).W))

  private def fireFEC(entry: FECCandidateEntry, i: Int): Unit = {
    fecDetectThisCycle := true.B
    fecDetectIdx := i.U
    fecDetectBlkPaddr := entry.blkPaddr
    fecDetectVSetIdx := entry.vSetIdx

    entry.valid := false.B
    perfFECLinesDetected := perfFECLinesDetected + 1.U
  }

  // Cycle counter for aging
  private val cycleCounter = RegInit(0.U(64.W))
  cycleCounter := cycleCounter + 1.U

  // Performance counters
  private val perfTotalMisses = RegInit(0.U(64.W))
  private val perfMissesWithStall = RegInit(0.U(64.W))
  private val perfFECLinesDetected = RegInit(0.U(64.W))
  private val perfTrackerFull = RegInit(0.U(64.W))

  /** Entry Allocation - Track new cache misses
    */
  private val freeEntryVec = VecInit(entries.map(!_.valid))
  private val hasFreeEntry = freeEntryVec.asUInt.orR
  private val freeEntryIdx = PriorityEncoder(freeEntryVec)

  when(io.newMiss.valid) {
    val hitVec = VecInit(
      entries.map(e => e.valid && (e.ftqIdx === io.newMiss.bits.ftqIdx))
    ) // Check if this miss is already being tracked
    val hasHit = hitVec.asUInt.orR //Reduce to see if there's any hit
    val hitIdx = PriorityEncoder(hitVec)

    when(
      hasHit
    ) {       perfTotalMisses := perfTotalMisses + 1.U // Count duplicate misses too      entries(hitIdx).blkPaddr := io.newMiss.bits.blkPaddr // Update block address if already tracking
      entries(hitIdx).vSetIdx := io.newMiss.bits.vSetIdx
      entries(hitIdx).allocTime := cycleCounter // Refresh allocation time on new miss for same FTQ entry

    }.elsewhen(hasFreeEntry) {
      perfTotalMisses := perfTotalMisses + 1.U // Only count successfully allocated misses
      entries(freeEntryIdx).valid := true.B
      entries(freeEntryIdx).blkPaddr := io.newMiss.bits.blkPaddr
      entries(freeEntryIdx).vSetIdx := io.newMiss.bits.vSetIdx
      entries(freeEntryIdx).ftqIdx := io.newMiss.bits.ftqIdx
      entries(freeEntryIdx).allocTime := cycleCounter
      entries(freeEntryIdx).stalledIFU := false.B
      entries(freeEntryIdx).retired := false.B
    }.otherwise {
      // Tracker is full, count overflow
      perfTrackerFull := perfTrackerFull + 1.U
    }
  }

  /** Stall Status Update - Mark entries that caused stalls
    */
  when(io.stallUpdate.valid) {
    entries.zipWithIndex.foreach { case (entry, i) =>
      when(entry.valid && entry.ftqIdx === io.stallUpdate.bits.ftqIdx) {
        val wasStalled = entry.stalledIFU
        entry.stalledIFU := io.stallUpdate.bits.stalled

        // If this is a new stall indication for an allocated entry, count it
        when(io.stallUpdate.bits.stalled && !wasStalled) {
          perfMissesWithStall := perfMissesWithStall + 1.U
        }

        // If this entry is now stalled, check if it has already retired to fire FEC
        when(io.stallUpdate.bits.stalled && !wasStalled && entry.retired) {
          fireFEC(entry, i)
        }

      }
    }
  }

  /** Retirement Update & FEC Detection When instructions from an FTQ entry
    * retire, check if this completes the FEC condition (miss + stall + retired)
    */
  // Process all retirement updates (up to CommitWidth per cycle)
  (0 until CommitWidth).foreach { w =>
    when(io.retireUpdate(w).valid) {
      entries.zipWithIndex.foreach { case (entry, i) =>
        when(entry.valid && entry.ftqIdx === io.retireUpdate(w).bits.ftqIdx) {
          val wasRetired = entry.retired
          entry.retired := true.B

          // Check if this is now an FEC line: miss (allocated) + stall + retired
          when(entry.stalledIFU && !wasRetired) {
            fireFEC(entry, i)
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
  }.otherwise {
    fecDetected := false.B
  }

  // Output FEC line signal
  io.fecLine.valid := fecDetected
  io.fecLine.bits.blkPaddr := fecBlkPaddr
  io.fecLine.bits.vSetIdx := fecVSetIdx

  /** Entry Aging & Eviction Free entries that are too old (likely stale due to
    * flush/redirect)
    */
  private val agingThreshold = 1024.U // Cycles before considering entry stale
  entries.foreach { entry =>
    when(entry.valid && (cycleCounter - entry.allocTime) > agingThreshold) {
      entry.valid := false.B
    }
  }

  /** Flush Handling On global flush, invalidate all entries
    */
  when(io.flush) {
    entries.foreach { entry =>
      entry.valid := false.B
    }
  }

  /** Performance Counter Outputs
    */
  io.perfInfo.totalMisses := perfTotalMisses
  io.perfInfo.missesWithStall := perfMissesWithStall
  io.perfInfo.fecLinesDetected := perfFECLinesDetected
  io.perfInfo.trackerFull := perfTrackerFull

  /** Debug & Assertions
    */
  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    dontTouch(entries)
    dontTouch(fecDetected)
  }

  // Sanity check: Should never have more than one FEC detection per cycle
  assert(
    PopCount(entries.map(e => e.valid && e.stalledIFU && e.retired)) <= 1.U,
    "FECTracker: Multiple FEC lines detected in same cycle"
  )
}

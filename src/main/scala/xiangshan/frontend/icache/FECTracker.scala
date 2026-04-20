package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan.frontend._

/** FEC Miss Information - signals a new cache miss to track */
class FECMissInfo(implicit p: Parameters) extends ICacheBundle {
  val blkPaddr = UInt((PAddrBits - blockOffBits).W)
  val blkVaddr = UInt((PAddrBits - blockOffBits).W) // virtual block address of the miss
  val vSetIdx  = UInt(idxBits.W)
  val ftqIdx   = new FtqPtr
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
  val valid:               Bool = Bool()
  val blkPaddr:            UInt = UInt((PAddrBits - blockOffBits).W) // physical block address
  val blkVaddr:            UInt = UInt((PAddrBits - blockOffBits).W) // virtual block address
  val vSetIdx:             UInt = UInt(idxBits.W)
  val triggerAddr:         UInt = UInt((PAddrBits - blockOffBits).W) // preceding BPU fetch PC at miss time
  val ftqIdx:           FtqPtr = new FtqPtr
  val allocTime:           UInt = UInt(64.W)
  val allocRetiredInstrs:  UInt = UInt(64.W)
  val stalledIFU:          Bool = Bool()
  val retired:             Bool = Bool()
  val stallCycles:         UInt = UInt(4.W)
}

/** FEC Tracker Performance Counters
  */
class FECTrackerPerfInfo(implicit p: Parameters) extends ICacheBundle {
  val totalMisses:      UInt = UInt(64.W)
  val missesWithStall:  UInt = UInt(64.W)
  val fecLinesDetected: UInt = UInt(64.W)
  val trackerFull:      UInt = UInt(64.W)
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
    val blkPaddr           = UInt((PAddrBits - blockOffBits).W)
    val blkVaddr           = UInt((PAddrBits - blockOffBits).W)
    val vSetIdx            = UInt(idxBits.W)
    val triggerAddr        = UInt((PAddrBits - blockOffBits).W) // preceding BPU fetch PC
    val highCost           = Bool()    // stall lasted at least 10 cycles
    val starvationDistance = UInt(16.W) // retired instructions from alloc to FEC
  })

  // Trigger address: preceding BPU fetch PC captured at miss time (set by ICache)
  val triggerAddr = Input(UInt((PAddrBits - blockOffBits).W))

  // Flush - only for fence.i; regular pipeline flushes must NOT clear the tracker
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
  *
  * Storage uses plain registers (not SRAM) — 16 entries is too small to benefit
  * from SRAM, and register init guarantees zero state at reset, eliminating the
  * uninitialized-data spurious FEC events that the old SRAM-based design suffered.
  */
class FECTracker(numEntries: Int = 16)(implicit p: Parameters)
    extends ICacheModule {
  val io: FECTrackerIO = IO(new FECTrackerIO)

  // -------------------------------------------------------------------------
  // Storage: register-backed, zero-initialized at reset.
  // entriesRead  = current registered state (readable this cycle)
  // entriesNext  = wire computed from entriesRead; registered at end of cycle
  // -------------------------------------------------------------------------
  private val entries     = RegInit(VecInit(Seq.fill(numEntries)(0.U.asTypeOf(new FECCandidateEntry))))
  private val entriesRead = entries
  private val entriesNext = WireInit(entries)

  // -------------------------------------------------------------------------
  // FEC detection: combinatorial detect → registered output (one-cycle delay)
  // -------------------------------------------------------------------------
  private val fecDetected            = RegInit(false.B)
  private val fecBlkPaddr            = RegInit(0.U((PAddrBits - blockOffBits).W))
  private val fecBlkVaddr            = RegInit(0.U((PAddrBits - blockOffBits).W))
  private val fecVSetIdx             = RegInit(0.U(idxBits.W))
  private val fecTriggerAddr         = RegInit(0.U((PAddrBits - blockOffBits).W))
  private val fecHighCost            = RegInit(false.B)
  private val fecStarvationDistance  = RegInit(0.U(16.W))

  private val fecDetectThisCycle         = WireInit(false.B)
  private val fecDetectBlkPaddr          = WireInit(0.U((PAddrBits - blockOffBits).W))
  private val fecDetectBlkVaddr          = WireInit(0.U((PAddrBits - blockOffBits).W))
  private val fecDetectVSetIdx           = WireInit(0.U(idxBits.W))
  private val fecDetectTriggerAddr       = WireInit(0.U((PAddrBits - blockOffBits).W))
  private val fecDetectHighCost          = WireInit(false.B)
  private val fecDetectStarvationDist    = WireInit(0.U(16.W))

  private def fireFEC(entry: FECCandidateEntry, i: Int): Unit = {
    val retiredDistanceWide = cycleRetiredInstrs - entry.allocRetiredInstrs
    fecDetectThisCycle      := true.B
    fecDetectBlkPaddr       := entry.blkPaddr
    fecDetectBlkVaddr       := entry.blkVaddr
    fecDetectVSetIdx        := entry.vSetIdx
    fecDetectTriggerAddr    := entry.triggerAddr
    fecDetectHighCost       := entry.stallCycles >= 10.U
    fecDetectStarvationDist :=
      Mux(
        retiredDistanceWide > ((1 << 16) - 1).U,
        ((1 << 16) - 1).U,
        retiredDistanceWide(15, 0)
      )
    entriesNext(i).valid    := false.B
    perfFECLinesDetected    := perfFECLinesDetected + 1.U
  }

  // -------------------------------------------------------------------------
  // Cycle / retired-instruction counters
  // -------------------------------------------------------------------------
  private val cycleCounter        = RegInit(0.U(64.W))
  cycleCounter := cycleCounter + 1.U
  private val cycleRetiredInstrs  = RegInit(0.U(64.W))
  private val committedThisCycle  = PopCount(io.retireUpdate.map(_.valid))
  cycleRetiredInstrs := cycleRetiredInstrs + committedThisCycle

  // -------------------------------------------------------------------------
  // Performance counters (debug; TODO: remove before tape-out)
  // -------------------------------------------------------------------------
  private val perfTotalMisses      = RegInit(0.U(64.W))
  private val perfMissesWithStall  = RegInit(0.U(64.W))
  private val perfFECLinesDetected = RegInit(0.U(64.W))
  private val perfTrackerFull      = RegInit(0.U(64.W))

  // =========================================================================
  // New miss
  // =========================================================================
  private val freeEntryVec = VecInit(entriesRead.map(entry => !entry.valid))
  private val hasFreeEntry = freeEntryVec.asUInt.orR
  private val freeEntryIdx = PriorityEncoder(freeEntryVec)

  when(io.newMiss.valid) {
    perfTotalMisses := perfTotalMisses + 1.U

    val hitVec = VecInit(entriesRead.map(e => e.valid && (e.ftqIdx === io.newMiss.bits.ftqIdx)))
    val hasHit = hitVec.asUInt.orR
    val hitIdx = PriorityEncoder(hitVec)

    when(hasHit) {
      // Refresh the existing entry (same FTQ block, new miss address)
      entriesNext(hitIdx).blkPaddr           := io.newMiss.bits.blkPaddr
      entriesNext(hitIdx).blkVaddr           := io.newMiss.bits.blkVaddr
      entriesNext(hitIdx).vSetIdx            := io.newMiss.bits.vSetIdx
      entriesNext(hitIdx).triggerAddr        := io.triggerAddr
      entriesNext(hitIdx).allocTime          := cycleCounter
      entriesNext(hitIdx).allocRetiredInstrs := cycleRetiredInstrs
      entriesNext(hitIdx).stallCycles        := 0.U

      printf(
        "[FEC] Miss (update): ftqIdx=%d blkPaddr=0x%x cycle=%d\n",
        io.newMiss.bits.ftqIdx.value,
        io.newMiss.bits.blkPaddr,
        cycleCounter
      )

    }.elsewhen(hasFreeEntry) {
      entriesNext(freeEntryIdx).valid               := true.B
      entriesNext(freeEntryIdx).blkPaddr            := io.newMiss.bits.blkPaddr
      entriesNext(freeEntryIdx).blkVaddr            := io.newMiss.bits.blkVaddr
      entriesNext(freeEntryIdx).vSetIdx             := io.newMiss.bits.vSetIdx
      entriesNext(freeEntryIdx).triggerAddr         := io.triggerAddr
      entriesNext(freeEntryIdx).ftqIdx              := io.newMiss.bits.ftqIdx
      entriesNext(freeEntryIdx).allocTime           := cycleCounter
      entriesNext(freeEntryIdx).allocRetiredInstrs  := cycleRetiredInstrs
      entriesNext(freeEntryIdx).stalledIFU          := false.B
      entriesNext(freeEntryIdx).retired             := false.B
      entriesNext(freeEntryIdx).stallCycles         := 0.U

      printf(
        "[FEC] Miss (new): ftqIdx=%d blkPaddr=0x%x cycle=%d\n",
        io.newMiss.bits.ftqIdx.value,
        io.newMiss.bits.blkPaddr,
        cycleCounter
      )

    }.otherwise {
      perfTrackerFull := perfTrackerFull + 1.U

      val stalledCount = PopCount(entriesRead.map(e => e.valid && e.stalledIFU))
      val retiredCount = PopCount(entriesRead.map(e => e.valid && e.retired))
      val bothCount    = PopCount(entriesRead.map(e => e.valid && e.stalledIFU && e.retired))
      printf(
        "[FEC] Tracker FULL (miss dropped): stalled=%d retired=%d both=%d cycle=%d\n",
        stalledCount, retiredCount, bothCount, cycleCounter
      )
    }
  }

  // =========================================================================
  // Stall update
  // =========================================================================
  when(io.stallUpdate.valid) {
    entriesRead.zipWithIndex.foreach { case (entry, i) =>
      when(entry.valid && entry.ftqIdx === io.stallUpdate.bits.ftqIdx) {
        val wasStalled = entry.stalledIFU
        entriesNext(i).stalledIFU := io.stallUpdate.bits.stalled
        when(io.stallUpdate.bits.stalled && entry.stallCycles =/= 15.U) {
          entriesNext(i).stallCycles := entry.stallCycles + 1.U
        }

        when(io.stallUpdate.bits.stalled && !wasStalled) {
          perfMissesWithStall := perfMissesWithStall + 1.U
          printf(
            "[FEC] Stall: ftqIdx=%d blkPaddr=0x%x retired=%d cycle=%d\n",
            entry.ftqIdx.value, entry.blkPaddr, entry.retired, cycleCounter
          )
        }

        // Stall arrives after retirement → fire FEC now
        when(io.stallUpdate.bits.stalled && !wasStalled && entry.retired) {
          fireFEC(entry, i)
        }
      }
    }
  }

  // =========================================================================
  // Retirement
  // =========================================================================
  (0 until CommitWidth).foreach { w =>
    when(io.retireUpdate(w).valid && cycleCounter % 1000.U === 0.U) {
      printf(
        "[FEC] Retire signal: slot=%d ftqIdx=%d cycle=%d\n",
        w.U, io.retireUpdate(w).bits.ftqIdx.value, cycleCounter
      )
    }
  }

  (0 until CommitWidth).foreach { w =>
    when(io.retireUpdate(w).valid) {
      val hitVec = VecInit(
        entriesRead.map(e => e.valid && (e.ftqIdx === io.retireUpdate(w).bits.ftqIdx))
      )
      val hasHit = hitVec.asUInt.orR

      when(!hasHit && cycleCounter % 1000.U === 0.U) {
        printf(
          "[FEC] Retire NO MATCH: ftqIdx=%d cycle=%d\n",
          io.retireUpdate(w).bits.ftqIdx.value, cycleCounter
        )
      }

      entriesRead.zipWithIndex.foreach { case (entry, i) =>
        when(entry.valid && entry.ftqIdx === io.retireUpdate(w).bits.ftqIdx) {
          val wasRetired = entry.retired
          entriesNext(i).retired := true.B

          when(!wasRetired) {
            printf(
              "[FEC] Retire MATCH: ftqIdx=%d blkPaddr=0x%x stalled=%d cycle=%d\n",
              entry.ftqIdx.value, entry.blkPaddr, entry.stalledIFU, cycleCounter
            )
          }

          when(entry.stalledIFU && !wasRetired) {
            // Normal order: miss → stall → retire
            fireFEC(entry, i)
          }.elsewhen(!entry.stalledIFU && !wasRetired) {
            // Retired without ever stalling → will never become FEC; free the slot.
            entriesNext(i).valid := false.B
            printf(
              "[FEC] Retire (no stall, freeing): ftqIdx=%d blkPaddr=0x%x cycle=%d\n",
              entry.ftqIdx.value, entry.blkPaddr, cycleCounter
            )
          }
        }
      }
    }
  }

  // =========================================================================
  // Register FEC detection outputs
  // =========================================================================
  when(fecDetectThisCycle) {
    fecDetected           := true.B
    fecBlkPaddr           := fecDetectBlkPaddr
    fecBlkVaddr           := fecDetectBlkVaddr
    fecVSetIdx            := fecDetectVSetIdx
    fecTriggerAddr        := fecDetectTriggerAddr
    fecHighCost           := fecDetectHighCost
    fecStarvationDistance := fecDetectStarvationDist
  }.otherwise {
    fecDetected := false.B
  }

  io.fecLine.valid                   := fecDetected
  io.fecLine.bits.blkPaddr           := fecBlkPaddr
  io.fecLine.bits.blkVaddr           := fecBlkVaddr
  io.fecLine.bits.vSetIdx            := fecVSetIdx
  io.fecLine.bits.triggerAddr        := fecTriggerAddr
  io.fecLine.bits.highCost           := fecHighCost
  io.fecLine.bits.starvationDistance := fecStarvationDistance

  when(fecDetected) {
    printf(
      "[FEC] Detected FEC Line: blkPaddr=0x%x blkVaddr=0x%x vSetIdx=0x%x triggerAddr=0x%x highCost=%d starvationDistance=%d cycle=%d\n",
      fecBlkPaddr, fecBlkVaddr, fecVSetIdx, fecTriggerAddr,
      fecHighCost, fecStarvationDistance, cycleCounter
    )
  }

  // =========================================================================
  // Entry aging — free stale entries that will never fire FEC
  // =========================================================================
  private val agingThreshold = 50000.U
  entriesRead.zipWithIndex.foreach { case (entry, i) =>
    when(entry.valid && (cycleCounter - entry.allocTime) > agingThreshold) {
      entriesNext(i).valid := false.B
      printf(
        "[FEC] Aged out: ftqIdx=%d blkPaddr=0x%x stalled=%d retired=%d age=%d\n",
        entry.ftqIdx.value, entry.blkPaddr,
        entry.stalledIFU, entry.retired,
        cycleCounter - entry.allocTime
      )
    }
  }

  // =========================================================================
  // Flush (fence.i only — NOT regular branch misprediction flushes)
  // =========================================================================
  when(io.fencei) {
    val validCount   = PopCount(entriesRead.map(_.valid))
    val stalledCount = PopCount(entriesRead.map(e => e.valid && e.stalledIFU))
    val retiredCount = PopCount(entriesRead.map(e => e.valid && e.retired))
    val bothCount    = PopCount(entriesRead.map(e => e.valid && e.stalledIFU && e.retired))
    printf(
      "[FEC] FENCEI: clearing %d entries (stalled=%d retired=%d both=%d) cycle=%d\n",
      validCount, stalledCount, retiredCount, bothCount, cycleCounter
    )
    entriesRead.zipWithIndex.foreach { case (_, i) =>
      entriesNext(i).valid := false.B
    }
  }

  // =========================================================================
  // Performance counter outputs
  // =========================================================================
  io.perfInfo.totalMisses      := perfTotalMisses
  io.perfInfo.missesWithStall  := perfMissesWithStall
  io.perfInfo.fecLinesDetected := perfFECLinesDetected
  io.perfInfo.trackerFull      := perfTrackerFull

  when(cycleCounter % 10000.U === 0.U && cycleCounter =/= 0.U) {
    val validCount      = PopCount(entriesRead.map(_.valid))
    val stalledOnlyCount = PopCount(entriesRead.map(e => e.valid && e.stalledIFU && !e.retired))
    val retiredOnlyCount = PopCount(entriesRead.map(e => e.valid && !e.stalledIFU && e.retired))
    val bothCount       = PopCount(entriesRead.map(e => e.valid && e.stalledIFU && e.retired))
    val neitherCount    = PopCount(entriesRead.map(e => e.valid && !e.stalledIFU && !e.retired))
    printf(
      "[FEC Summary] Cycle %d: Misses=%d StallMisses=%d FECLines=%d TrackerFull=%d\n",
      cycleCounter, perfTotalMisses, perfMissesWithStall, perfFECLinesDetected, perfTrackerFull
    )
    printf(
      "[FEC State] Valid=%d StalledOnly=%d RetiredOnly=%d Both=%d Neither=%d\n",
      validCount, stalledOnlyCount, retiredOnlyCount, bothCount, neitherCount
    )
  }

  // =========================================================================
  // Commit next-state to registers
  // =========================================================================
  entries := entriesNext
}

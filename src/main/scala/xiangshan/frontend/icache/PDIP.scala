package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan.frontend._

/** PDIP (Prefetch-Directed Instruction Prefetching) Parameters
  */
case class PDIPParams(
    enabled: Boolean = true,
    numTableSets: Int = 64, // Number of sets in PDIP table
    numWaysPerSet: Int = 4, // Associativity of PDIP table
    numTargetsPerEntry: Int = 4, // Max FEC targets per trigger
    prefetchQueueSize: Int = 8, // Size of prefetch queue
    mshrCheckEnabled: Boolean = true // Check MSHR availability before prefetch
)

/** PDIP Trigger-Candidate Association Maps a trigger instruction block address
  * to FEC cache line candidates
  */
class PDIPTableEntry(numTargets: Int)(implicit p: Parameters)
    extends ICacheBundle {
  val valid: Bool = Bool() // Entry is valid
  val trigger: UInt = UInt(
    (PAddrBits - blockOffBits).W
  ) // Trigger block address
  val targets: Vec[PDIPTarget] =
    Vec(numTargets, new PDIPTarget) // FEC line targets
  val lru: UInt = UInt(log2Ceil(numTargets).W) // LRU for target replacement
}

/** PDIP Prefetch Target (FEC Line)
  */
class PDIPTarget(implicit p: Parameters) extends ICacheBundle {
  val valid: Bool = Bool() // Target is valid
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W) // FEC block address
  val vSetIdx: UInt = UInt(idxBits.W) // Virtual set index
  val blkVaddr: UInt = UInt((VAddrBits - blockOffBits).W)
  val confidence: UInt = UInt(2.W) // 2-bit confidence counter
}

/** PDIP Table IO
  */
class PDIPTableIO(params: PDIPParams)(implicit p: Parameters)
    extends ICacheBundle {
  // Lookup by trigger address
  val lookup = new Bundle {
    val req = Flipped(ValidIO(new Bundle {
      val trigger = UInt((PAddrBits - blockOffBits).W)
    }))
    val resp = ValidIO(Vec(params.numTargetsPerEntry, new PDIPTarget))
  }

  // Allocate new trigger-target association
  val allocate = Flipped(ValidIO(new Bundle {
    val trigger = UInt((PAddrBits - blockOffBits).W)
    val target = new PDIPTarget
  }))

  // Flush signal
  val flush = Input(Bool())
}

/** PDIP Table Set-associative structure to store trigger -> FEC target
  * associations
  */
class PDIPTable(params: PDIPParams)(implicit p: Parameters)
    extends ICacheModule {
  val io: PDIPTableIO = IO(new PDIPTableIO(params))

  // Storage: 2D array [sets][ways]
  private val table = RegInit(
    VecInit(
      Seq.fill(params.numTableSets)(
        VecInit(
          Seq.fill(params.numWaysPerSet)(
            0.U.asTypeOf(new PDIPTableEntry(params.numTargetsPerEntry))
          )
        )
      )
    )
  )

  // Lookup logic (combinational)
  private val lookupSet = table(io.lookup.req.bits.trigger(log2Ceil(params.numTableSets) - 1, 0))

  // Find matching way
  private val matchWay = VecInit(
    lookupSet.map(entry =>
      entry.valid && entry.trigger === io.lookup.req.bits.trigger
    )
  ).asUInt

  private val hitWay = PriorityEncoder(
    matchWay
  ) // PriorityEncoder is safe even if >1 bit set
  private val hit = matchWay =/= 0.U

  // Output targets if hit
  io.lookup.resp.valid := io.lookup.req.valid && hit
  // If stored, return stored target, otherwise, return 0s
  io.lookup.resp.bits := Mux(
    hit,
    lookupSet(hitWay).targets,
    VecInit(Seq.fill(params.numTargetsPerEntry)(0.U.asTypeOf(new PDIPTarget)))
  )

  // Allocation logic
  when(io.allocate.valid && !io.flush) {
    val allocSetIdx =
      io.allocate.bits.trigger(log2Ceil(params.numTableSets) - 1, 0)
    val allocSet = table(allocSetIdx)

    // Check if trigger already exists
    val existingWayOH = VecInit(
      allocSet.map(entry =>
        entry.valid && entry.trigger === io.allocate.bits.trigger
      )
    ).asUInt

    val existingWay = PriorityEncoder(
      existingWayOH
    ) // PriorityEncoder is safe even if >1 bit set
    val triggerExists = existingWayOH =/= 0.U

    when(triggerExists) {
      // Update existing entry: add target
      val entry = allocSet(existingWay)
      val targetSlot = entry.targets

      // Find invalid slot or replace LRU
      val invalidSlot = VecInit(targetSlot.map(!_.valid)).asUInt
      val hasInvalidSlot = invalidSlot.orR
      val replaceIdx =
        Mux(hasInvalidSlot, PriorityEncoder(invalidSlot), entry.lru)

      // Check if target already exists (avoid duplicates)
      val targetExists = VecInit(
        targetSlot.map(t =>
          t.valid && t.blkPaddr === io.allocate.bits.target.blkPaddr
        )
      ).asUInt.orR

      when(!targetExists) {
        targetSlot(replaceIdx) := io.allocate.bits.target
        targetSlot(replaceIdx).valid := true.B
        // Update confidence for new target
        targetSlot(replaceIdx).confidence := 2.U // Initial confidence

        // Decay the confidence of the other slots in the same entry
        // (simple aging mechanism so values are not strictly monotonic)
        for (i <- 0 until params.numTargetsPerEntry) {
          when(i.U =/= replaceIdx && targetSlot(i).confidence =/= 0.U) {
            targetSlot(i).confidence := targetSlot(i).confidence - 1.U
          }
        }

        // Update LRU
        entry.lru := Mux(
          replaceIdx === (params.numTargetsPerEntry - 1).U,
          0.U,
          replaceIdx + 1.U
        )
      }.otherwise {
        // Target exists, boost confidence
        val existingTargetIdx = PriorityEncoder(
          VecInit(
            targetSlot.map(t =>
              t.valid && t.blkPaddr === io.allocate.bits.target.blkPaddr
            )
          )
        )
        when(targetSlot(existingTargetIdx).confidence < 3.U) {
          targetSlot(existingTargetIdx).confidence := targetSlot(
            existingTargetIdx
          ).confidence + 1.U
        }
        // Decay other targets in the same entry on an update/hit as well
        for (i <- 0 until params.numTargetsPerEntry) {
          when(i.U =/= existingTargetIdx && targetSlot(i).confidence =/= 0.U) {
            targetSlot(i).confidence := targetSlot(i).confidence - 1.U
          }
        }
      }
    }.otherwise {
      // Allocate new entry or replace invalid/LRU way
      val invalidWay = VecInit(allocSet.map(!_.valid)).asUInt
      val hasInvalidWay = invalidWay.orR

      // Simple FIFO replacement for ways (can be enhanced with true LRU)
      val rrWay = RegInit(0.U(log2Ceil(params.numWaysPerSet).W))
      val replaceWayOH = Mux(
        hasInvalidWay,
        invalidWay,
        UIntToOH(rrWay, params.numWaysPerSet)
      )
      val replaceWay = OHToUInt(replaceWayOH)
      when(!hasInvalidWay) {
        rrWay := Mux(rrWay === (params.numWaysPerSet - 1).U, 0.U, rrWay + 1.U)
      }

      // Initialize new entry
      val newEntry = Wire(new PDIPTableEntry(params.numTargetsPerEntry))
      newEntry.valid := true.B
      newEntry.trigger := io.allocate.bits.trigger
      newEntry.targets := VecInit(
        Seq.fill(params.numTargetsPerEntry)(
          0.U.asTypeOf(new PDIPTarget)
        )
      )
      newEntry.targets(0) := io.allocate.bits.target
      newEntry.targets(0).valid := true.B
      newEntry.targets(0).confidence := 2.U // Initial confidence
      newEntry.lru := 1.U // Next target goes to slot 1

      table(allocSetIdx)(replaceWay) := newEntry
    }
  }

  // Flush: invalidate all entries
  when(io.flush) {
    table.foreach(set => set.foreach(entry => entry.valid := false.B))
  }
}

/** Prefetch Queue Entry
  */
class PrefetchEntry(implicit p: Parameters) extends ICacheBundle {
  // Physical address used directly (PDIP learns from FEC which already has physical addresses).
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W)
  val vSetIdx: UInt = UInt(log2Ceil(nSets).W)
  val vaddr: UInt = UInt(
    VAddrBits.W
  ) // kept for debugging / potential future use
}

class PrefetchQueueEntry(implicit p: Parameters) extends ICacheBundle {
  val valid: Bool = Bool()
  val vaddr: UInt = UInt(VAddrBits.W)
}

/** Prefetch Queue IO
  */
class PrefetchQueueIO(queueSize: Int)(implicit p: Parameters)
    extends ICacheBundle {
  val enq = Flipped(DecoupledIO(new PrefetchEntry))
  val deq = DecoupledIO(new PrefetchEntry)

  val flush = Input(Bool())
  val mshrAvailable = Input(Bool())

  val empty = Output(Bool())
  val full = Output(Bool())
}

/** Prefetch Queue Decouples PDIP prefetch requests from ICache MSHR allocation
  */
class PrefetchQueue(queueSize: Int)(implicit p: Parameters)
    extends ICacheModule {
  val io: PrefetchQueueIO = IO(new PrefetchQueueIO(queueSize))

  private val queue = RegInit(
    VecInit(Seq.fill(queueSize)(0.U.asTypeOf(new PrefetchEntry)))
  )

  private val head = RegInit(0.U(log2Ceil(queueSize).W))
  private val tail = RegInit(0.U(log2Ceil(queueSize).W))
  private val maybe_full = RegInit(false.B)

  private val ptr_match = head === tail
  private val empty = ptr_match && !maybe_full
  private val full = ptr_match && maybe_full

  io.empty := empty
  io.full := full

  // Enqueue
  io.enq.ready := !full
  when(io.enq.fire && !io.flush) {
    queue(tail) := io.enq.bits

    val tailNext = Mux(tail === (queueSize - 1).U, 0.U, tail + 1.U)
    tail := tailNext

    when(ptr_match) { maybe_full := true.B }
  }

  // Dequeue (only when MSHR available)
  io.deq.valid := !empty && io.mshrAvailable
  io.deq.bits := queue(head)

  when(io.deq.fire && !io.flush) {
    val headNext = Mux(head === (queueSize - 1).U, 0.U, head + 1.U)
    head := headNext

    maybe_full := false.B
  }

  // Flush
  when(io.flush) {
    head := 0.U
    tail := 0.U
    maybe_full := false.B
  }
}

/** PDIP Controller IO
  */
class PDIPControllerIO(params: PDIPParams)(implicit p: Parameters)
    extends ICacheBundle {
  // Input: Current instruction block address from BPU/IFU
  val trigger = Flipped(ValidIO(new Bundle {
    val blkPaddr = UInt((PAddrBits - blockOffBits).W)
  }))

  // Input: FEC line detected (from FECTracker)
  val fecLine = Flipped(ValidIO(new Bundle {
    val blkPaddr = UInt((PAddrBits - blockOffBits).W)
    val blkVaddr = UInt((VAddrBits - blockOffBits).W)
    val vSetIdx = UInt(idxBits.W)
    val triggerAddr =
      UInt((PAddrBits - blockOffBits).W) // Trigger that caused this FEC
  }))

  // Output: Prefetch request to ICache
  val prefetchVaddr: DecoupledIO[PrefetchEntry] = DecoupledIO(new PrefetchEntry)
  // MSHR availability check
  val mshrAvailable = Input(Bool())

  // Flush signal
  val flush = Input(Bool())

  // Enable/disable PDIP
  val enable = Input(Bool())

  // Performance counters
  val perfInfo = Output(new Bundle {
    val totalPrefetches = UInt(64.W)
    val tableLookups = UInt(64.W)
    val tableHits = UInt(64.W)
    val queueFull = UInt(64.W)
  })
}

/** PDIP Controller Main control logic for PDIP prefetching
  */
class PDIPController(params: PDIPParams)(implicit p: Parameters)
    extends ICacheModule {
  val io: PDIPControllerIO = IO(new PDIPControllerIO(params))

  private val pdipTable = Module(new PDIPTable(params))
  private val prefetchQueue = Module(
    new PrefetchQueue(params.prefetchQueueSize)
  )

  private val active = io.enable && !io.flush

  pdipTable.io.flush := io.flush
  prefetchQueue.io.flush := io.flush
  prefetchQueue.io.mshrAvailable := io.mshrAvailable

  // Table lookup on trigger
  pdipTable.io.lookup.req.valid := io.trigger.valid && active
  pdipTable.io.lookup.req.bits.trigger := io.trigger.bits.blkPaddr

  private val targetsValid = pdipTable.io.lookup.resp.valid
  private val targets = pdipTable.io.lookup.resp.bits

  private val validTargetsOH = VecInit(
    targets.map(t => t.valid && (t.confidence >= 2.U))
  ).asUInt
  private val hasValidTarget = validTargetsOH.orR
  private val targetSel = PriorityEncoder(validTargetsOH)

  // Enqueue one chosen target each cycle (simple)
  prefetchQueue.io.enq.valid := targetsValid && hasValidTarget && active
  val vaddr = Cat(targets(targetSel).blkVaddr, 0.U(blockOffBits.W))
  prefetchQueue.io.enq.bits.blkPaddr := targets(targetSel).blkPaddr
  prefetchQueue.io.enq.bits.vSetIdx := targets(targetSel).vSetIdx
  prefetchQueue.io.enq.bits.vaddr := vaddr

  // Learn from FEC line detections: allocate trigger-target associations
  // Since fecLine doesn't carry blkVaddr, reconstruct a best-effort blkVaddr from (blkPaddr, vSetIdx).
  private def reconstructVAddr(blkPaddr: UInt, vSetIdx: UInt): UInt = {
    val blkHi = blkPaddr.getWidth - 1
    val upper = blkPaddr(blkHi, idxBits)

    val raw = Cat(
      upper,
      vSetIdx,
      0.U(blockOffBits.W)
    )

    // Make sure we always return exactly VAddrBits bits (pad or truncate)
    if (raw.getWidth >= VAddrBits) {
      raw(VAddrBits - 1, 0)
    } else {
      Cat(0.U((VAddrBits - raw.getWidth).W), raw)
    }
  }

  private val learnedBlkVaddr = reconstructVAddr(
    io.fecLine.bits.blkPaddr,
    io.fecLine.bits.vSetIdx
  )(VAddrBits - 1, blockOffBits)

  pdipTable.io.allocate.valid := io.fecLine.valid && active
  pdipTable.io.allocate.bits.trigger := io.fecLine.bits.triggerAddr
  pdipTable.io.allocate.bits.target.valid := true.B
  pdipTable.io.allocate.bits.target.blkPaddr := io.fecLine.bits.blkPaddr
  pdipTable.io.allocate.bits.target.vSetIdx := io.fecLine.bits.vSetIdx
  pdipTable.io.allocate.bits.target.confidence := 2.U
  pdipTable.io.allocate.bits.target.blkVaddr := learnedBlkVaddr

  // Output
  io.prefetchVaddr <> prefetchQueue.io.deq

  // Performance counters
  private val perfTotalPrefetches = RegInit(0.U(64.W))
  private val perfTableLookups = RegInit(0.U(64.W))
  private val perfTableHits = RegInit(0.U(64.W))
  private val perfQueueFull = RegInit(0.U(64.W))

  when(io.prefetchVaddr.fire) {
    perfTotalPrefetches := perfTotalPrefetches + 1.U
  }
  when(pdipTable.io.lookup.req.valid) {
    perfTableLookups := perfTableLookups + 1.U
  }
  when(pdipTable.io.lookup.resp.valid) {
    perfTableHits := perfTableHits + 1.U
  }
  when(prefetchQueue.io.enq.valid && !prefetchQueue.io.enq.ready) {
    perfQueueFull := perfQueueFull + 1.U
  }

  io.perfInfo.totalPrefetches := perfTotalPrefetches
  io.perfInfo.tableLookups := perfTableLookups
  io.perfInfo.tableHits := perfTableHits
  io.perfInfo.queueFull := perfQueueFull
}

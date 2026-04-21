package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan.frontend._

/** EPIP (Enhanced Priority Instruction Prefetcher) Parameters
  */
case class EPIPParams(
    enabled: Boolean = true,
    numTableSets: Int = 512, 
    numWaysPerSet: Int = 8,  
    numTargetsPerEntry: Int = 2,
    prefetchQueueSize: Int = 40,
    minPrefetchConfidence: Int = 1,
    insertProbabilityDivisor: Int = 1,
    triggerMetaEntries: Int = 64,
    lfuCounterBits: Int = 12,
    priorityAdaptWindow: Int = 5000,
    priority0Distance: Int = 100,
    priority1Distance: Int = 1000,
    priority2Distance: Int = 2000
)

object EPIPPriority {
  val width = 2
  val p0 = 0.U(width.W) // Highest prio: D < 100
  val p1 = 1.U(width.W) // High prio:    100 <= D <= 1000
  val p2 = 2.U(width.W) // Medium prio:  1000 < D <= 2000
  val p3 = 3.U(width.W) // Low prio:     D > 2000
}

/** EPIP prefetch target (one FEC line stored per candidate slot). Each target
  * holds the block address, virtual set index, a 4-bit compaction mask for up
  * to four sequential successor blocks, a 2-bit confidence counter, and a 2-bit
  * priority derived from the starvation-to-reuse distance.
  */
class EPIPTarget(implicit p: Parameters) extends ICacheBundle {
  val valid: Bool = Bool()
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W)
  val vSetIdx: UInt = UInt(idxBits.W)
  val compactMask: UInt = UInt(4.W)
  val confidence: UInt = UInt(2.W)
  val priority: UInt = UInt(EPIPPriority.width.W)
}

/** One entry in the EPIP table. Replaces PDIP's LRU bit with a multi-bit LFU
  * frequency counter.
  */
class EPIPTableEntry(numTargets: Int, lfuCounterBits: Int)(implicit
    p: Parameters
) extends ICacheBundle {
  val valid: Bool = Bool()
  val trigger: UInt = UInt((PAddrBits - blockOffBits).W)
  val targets: Vec[EPIPTarget] = Vec(numTargets, new EPIPTarget)
  val lfuCount: UInt = UInt(lfuCounterBits.W)
}

class EPIPTriggerMeta(implicit p: Parameters) extends ICacheBundle {
  val valid: Bool = Bool()
  val trigger: UInt = UInt((PAddrBits - blockOffBits).W)
  val highCost: Bool = Bool()
  val controlTrigger: Bool = Bool()
  val btbMissTrigger: Bool = Bool()
}

/** Issue entry carries priority and confidence through the prefetch queue. */
class EPIPIssueEntry(implicit p: Parameters) extends ICacheBundle {
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W)
  val vSetIdx: UInt = UInt(idxBits.W)
  val priority: UInt = UInt(EPIPPriority.width.W)
  val confidence: UInt = UInt(2.W)
}

class EPIPTableIO(params: EPIPParams)(implicit p: Parameters)
    extends ICacheBundle {
  val lookup = new Bundle {
    val req = Flipped(ValidIO(new Bundle {
      val trigger = UInt((PAddrBits - blockOffBits).W)
    }))
    val resp = ValidIO(Vec(params.numTargetsPerEntry, new EPIPTarget))
  }

  val allocate = Flipped(ValidIO(new Bundle {
    val trigger = UInt((PAddrBits - blockOffBits).W)
    val target = new EPIPTarget
  }))

  val flush = Input(Bool())
}

/** EPIP Table 
  */
class EPIPTable(params: EPIPParams)(implicit p: Parameters)
    extends ICacheModule {
  val io: EPIPTableIO = IO(new EPIPTableIO(params))

  private val zeroTarget = 0.U.asTypeOf(new EPIPTarget)
  private val zeroTargets =
    VecInit(Seq.fill(params.numTargetsPerEntry)(zeroTarget))
  private val zeroEntry = 0.U.asTypeOf(
    new EPIPTableEntry(params.numTargetsPerEntry, params.lfuCounterBits)
  )

  private val table = Module(
    new SRAMTemplate(
      new EPIPTableEntry(params.numTargetsPerEntry, params.lfuCounterBits),
      set = params.numTableSets,
      way = params.numWaysPerSet,
      shouldReset = true,
      holdRead = true,
      singlePort = false
    )
  )

  private val setIdxBits = log2Ceil(params.numTableSets)
  private val allocSetIdx = io.allocate.bits.trigger(setIdxBits - 1, 0)
  private val lookupSetIdx = io.lookup.req.bits.trigger(setIdxBits - 1, 0)

  private val flushActive = RegInit(false.B)
  private val flushSetIdx = RegInit(0.U(setIdxBits.W))

  when(io.flush) {
    flushActive := true.B
    flushSetIdx := 0.U
  }.elsewhen(flushActive) {
    when(flushSetIdx === (params.numTableSets - 1).U) {
      flushActive := false.B
    }.otherwise {
      flushSetIdx := flushSetIdx + 1.U
    }
  }

  private val readDoAlloc = io.allocate.valid && !flushActive && !io.flush
  private val readDoLookup =
    io.lookup.req.valid && !readDoAlloc && !flushActive && !io.flush

  table.io.r.req.valid := readDoAlloc || readDoLookup
  table.io.r.req.bits.setIdx := Mux(readDoAlloc, allocSetIdx, lookupSetIdx)

  private val s1DoAlloc = RegNext(readDoAlloc, false.B)
  private val s1DoLookup = RegNext(readDoLookup, false.B)
  private val s1AllocTrigger = RegEnable(io.allocate.bits.trigger, readDoAlloc)
  private val s1AllocTarget = RegEnable(io.allocate.bits.target, readDoAlloc)
  private val s1LookupTrigger =
    RegEnable(io.lookup.req.bits.trigger, readDoLookup)
  private val readSet = table.io.r.resp.data

  // Offset of newBlkPaddr relative to the base block of an existing target.
  // Valid range is 1..4 (we subtract 1 to map to compactMask bits 0..3).
  private def compactableOffset(target: EPIPTarget, newBlkPaddr: UInt): UInt =
    newBlkPaddr - target.blkPaddr - 1.U

  private def canCompact(target: EPIPTarget, newBlkPaddr: UInt): Bool =
    target.valid &&
      newBlkPaddr > target.blkPaddr &&
      newBlkPaddr <= (target.blkPaddr + 4.U)

  // Saturating increment for the LFU counter.
  private def bumpLfu(count: UInt): UInt =
    Mux(count.andR, count, count + 1.U)

  // Priority-Based Eviction for candidates:
  // A candidate is a "better victim" (should be evicted sooner) when it has
  // a higher numerical priority value (= lower urgency).  On a tie, prefer
  // the one with lower confidence.
  private def betterVictimTarget(
      candidate: EPIPTarget,
      current: EPIPTarget
  ): Bool =
    !current.valid ||
      (candidate.valid && (
        (candidate.priority > current.priority) ||
          (candidate.priority === current.priority && candidate.confidence < current.confidence)
      ))

  // LFU-Based Victim Selection for table entries:
  // Evict the entry with the lowest access-frequency count.
  private def betterVictimWay(
      candidate: EPIPTableEntry,
      current: EPIPTableEntry
  ): Bool =
    !current.valid ||
      (candidate.valid && (
        (candidate.lfuCount < current.lfuCount) ||
          (candidate.lfuCount === current.lfuCount && candidate.trigger < current.trigger)
      ))

  // Use MUX to avoid the combinational loop from Wire self-indexing
  private def selectTargetVictim(targets: Vec[EPIPTarget]): UInt = {
    var bestIdx: UInt = 0.U(log2Ceil(params.numTargetsPerEntry).W)
    var bestPriority: UInt = targets(0).priority
    var bestConfidence: UInt = targets(0).confidence
    var bestValid: Bool = targets(0).valid
    for (i <- 1 until params.numTargetsPerEntry) {
      val isBetter = !bestValid ||
        (targets(i).valid && (
          (targets(i).priority > bestPriority) ||
            (targets(i).priority === bestPriority && targets(
              i
            ).confidence < bestConfidence)
        ))
      bestIdx =
        Mux(isBetter, i.U(log2Ceil(params.numTargetsPerEntry).W), bestIdx)
      bestPriority = Mux(isBetter, targets(i).priority, bestPriority)
      bestConfidence = Mux(isBetter, targets(i).confidence, bestConfidence)
      bestValid = Mux(isBetter, targets(i).valid, bestValid)
    }
    bestIdx
  }

  // Use MUX to avoid the combinational loop from Wire self-indexing
  private def selectWayVictim(entries: Vec[EPIPTableEntry]): UInt = {
    var bestIdx: UInt = 0.U(log2Ceil(params.numWaysPerSet).W)
    var bestLfu: UInt = entries(0).lfuCount
    var bestTrigger: UInt = entries(0).trigger
    var bestValid: Bool = entries(0).valid
    for (i <- 1 until params.numWaysPerSet) {
      val isBetter = !bestValid ||
        (entries(i).valid && (
          (entries(i).lfuCount < bestLfu) ||
            (entries(i).lfuCount === bestLfu && entries(
              i
            ).trigger < bestTrigger)
        ))
      bestIdx = Mux(isBetter, i.U(log2Ceil(params.numWaysPerSet).W), bestIdx)
      bestLfu = Mux(isBetter, entries(i).lfuCount, bestLfu)
      bestTrigger = Mux(isBetter, entries(i).trigger, bestTrigger)
      bestValid = Mux(isBetter, entries(i).valid, bestValid)
    }
    bestIdx
  }

  private val matchWay = VecInit(
    readSet.map(entry => entry.valid && entry.trigger === s1LookupTrigger)
  ).asUInt
  private val hitWay = PriorityEncoder(matchWay)
  private val hitWayOH = PriorityEncoderOH(matchWay)
  private val hit = matchWay.orR

  io.lookup.resp.valid := s1DoLookup && hit
  io.lookup.resp.bits := Mux(hit, readSet(hitWay).targets, zeroTargets)

  private val writeData = Wire(
    Vec(
      params.numWaysPerSet,
      new EPIPTableEntry(params.numTargetsPerEntry, params.lfuCounterBits)
    )
  )
  private val writeMask = Wire(UInt(params.numWaysPerSet.W))
  private val writeValid = Wire(Bool())
  writeData := readSet
  writeMask := 0.U
  writeValid := false.B

  when(s1DoAlloc) {
    val existingWayOH = VecInit(
      readSet.map(entry => entry.valid && entry.trigger === s1AllocTrigger)
    ).asUInt

    when(existingWayOH.orR) {
      val existingWay = PriorityEncoder(existingWayOH)
      val existingEntry = readSet(existingWay)
      val updatedEntry = WireInit(existingEntry)
      val targetSlots = existingEntry.targets

      updatedEntry.lfuCount := bumpLfu(existingEntry.lfuCount)

      // Check whether the new target is already represented (exact or compacted).
      val existingTargetOH = VecInit(targetSlots.map(t => {
        val offset = compactableOffset(t, s1AllocTarget.blkPaddr)
        (t.valid && t.blkPaddr === s1AllocTarget.blkPaddr) ||
        (canCompact(t, s1AllocTarget.blkPaddr) && t.compactMask(offset(1, 0)))
      })).asUInt
      val targetCanCompactOH = VecInit(
        targetSlots.map(t => canCompact(t, s1AllocTarget.blkPaddr))
      ).asUInt
      val invalidTargetOH = VecInit(targetSlots.map(t => !t.valid)).asUInt

      when(existingTargetOH.orR) {
        // Target already covered: strengthen confidence and keep the higher
        // priority (lower numerical) priority.
        val idx = PriorityEncoder(existingTargetOH)
        updatedEntry.targets(idx).confidence :=
          Mux(
            targetSlots(idx).confidence === 3.U,
            3.U,
            targetSlots(idx).confidence + 1.U
          )
        updatedEntry.targets(idx).priority :=
          Mux(
            s1AllocTarget.priority < targetSlots(idx).priority,
            s1AllocTarget.priority,
            targetSlots(idx).priority
          )
      }.elsewhen(targetCanCompactOH.orR) {
        // Compact new block into an existing target's mask.
        val compactIdx = PriorityEncoder(targetCanCompactOH)
        val compactOffset =
          compactableOffset(targetSlots(compactIdx), s1AllocTarget.blkPaddr)
        updatedEntry.targets(compactIdx).compactMask :=
          targetSlots(compactIdx).compactMask | UIntToOH(compactOffset(1, 0), 4)
        updatedEntry.targets(compactIdx).confidence :=
          Mux(
            targetSlots(compactIdx).confidence === 3.U,
            3.U,
            targetSlots(compactIdx).confidence + 1.U
          )
        updatedEntry.targets(compactIdx).priority :=
          Mux(
            s1AllocTarget.priority < targetSlots(compactIdx).priority,
            s1AllocTarget.priority,
            targetSlots(compactIdx).priority
          )
      }.otherwise {
        // Priority-Based Eviction: replace the least-urgent (highest numerical
        // priority) candidate, or fill an empty slot if one exists.
        val replaceIdx = Mux(
          invalidTargetOH.orR,
          PriorityEncoder(invalidTargetOH),
          selectTargetVictim(targetSlots)
        )
        updatedEntry.targets(replaceIdx) := s1AllocTarget
        updatedEntry.targets(replaceIdx).valid := true.B
      }

      writeData(existingWay) := updatedEntry
      writeMask := UIntToOH(existingWay, params.numWaysPerSet)
      writeValid := true.B

    }.otherwise {
      // ---- New trigger: allocate a fresh entry via LFU victim selection ----
      val invalidWayOH = VecInit(readSet.map(entry => !entry.valid)).asUInt
      val replaceWay = Mux(
        invalidWayOH.orR,
        PriorityEncoder(invalidWayOH),
        selectWayVictim(readSet)
      )
      val newEntry = Wire(
        new EPIPTableEntry(params.numTargetsPerEntry, params.lfuCounterBits)
      )
      newEntry.valid := true.B
      newEntry.trigger := s1AllocTrigger
      newEntry.lfuCount := 1.U // First training event for this trigger
      newEntry.targets := zeroTargets
      newEntry.targets(0) := s1AllocTarget
      newEntry.targets(0).valid := true.B
      writeData(replaceWay) := newEntry
      writeMask := UIntToOH(replaceWay, params.numWaysPerSet)
      writeValid := true.B
    }

  }.elsewhen(s1DoLookup && hit) {
    // LFU "Update on Hit": bump frequency on every successful trigger lookup.
    val updatedEntry = WireInit(readSet(hitWay))
    updatedEntry.lfuCount := bumpLfu(readSet(hitWay).lfuCount)
    writeData(hitWay) := updatedEntry
    writeMask := hitWayOH
    writeValid := true.B
  }

  private val flushWriteValid = flushActive
  private val flushWriteData = VecInit(
    Seq.fill(params.numWaysPerSet)(zeroEntry)
  )
  private val flushWriteMask = Fill(params.numWaysPerSet, 1.U(1.W))

  table.io.w.req.valid := flushWriteValid || writeValid
  table.io.w.req.bits.apply(
    data = Mux(flushWriteValid, flushWriteData, writeData),
    setIdx = Mux(
      flushWriteValid,
      flushSetIdx,
      Mux(
        s1DoAlloc,
        s1AllocTrigger(setIdxBits - 1, 0),
        s1LookupTrigger(setIdxBits - 1, 0)
      )
    ),
    waymask = Mux(flushWriteValid, flushWriteMask, writeMask)
  )
}

class EPIPPrefetchQueueIO(queueSize: Int)(implicit p: Parameters)
    extends ICacheBundle {
  val enq = Flipped(DecoupledIO(new EPIPIssueEntry))
  val deq = DecoupledIO(new PrefetchEntry)
  val flush = Input(Bool())
  val mshrThresholdMet = Input(Bool())
  val empty = Output(Bool())
  val full = Output(Bool())
}

/** Priority-based prefetch queue. Dequeues the entry with the highest urgency
  * (lowest numerical priority) first, breaking ties by confidence then by
  * address. This ensures the most time-critical FEC misses are issued to the
  * cache before less urgent ones.
  */
class EPIPPrefetchQueue(queueSize: Int)(implicit p: Parameters)
    extends ICacheModule {
  val io: EPIPPrefetchQueueIO = IO(new EPIPPrefetchQueueIO(queueSize))

  private val entries = RegInit(
    VecInit(Seq.fill(queueSize)(0.U.asTypeOf(new EPIPIssueEntry)))
  )
  private val valids = RegInit(0.U(queueSize.W))

  private def betterDeq(lhs: EPIPIssueEntry, rhs: EPIPIssueEntry): Bool =
    (lhs.priority < rhs.priority) ||
      (lhs.priority === rhs.priority && lhs.confidence > rhs.confidence) ||
      (lhs.priority === rhs.priority && lhs.confidence === rhs.confidence &&
        lhs.blkPaddr < rhs.blkPaddr)

  private val invalidMask = (~valids)(queueSize - 1, 0)
  private val empty = !valids.orR
  private val full = valids.andR
  private val enqIdx = PriorityEncoder(invalidMask)

  // Use MUX to avoid the combinational loop from Wire self-indexing
  private val deqIdx: UInt = {
    var bestIdx: UInt = 0.U(log2Ceil(queueSize).W)
    var bestPriority: UInt = entries(0).priority
    var bestConfidence: UInt = entries(0).confidence
    var bestBlkPaddr: UInt = entries(0).blkPaddr
    var bestValid: Bool = valids(0)
    for (i <- 1 until queueSize) {
      val isBetter = valids(i) && (
        !bestValid ||
          (entries(i).priority < bestPriority) ||
          (entries(i).priority === bestPriority && entries(
            i
          ).confidence > bestConfidence) ||
          (entries(i).priority === bestPriority && entries(
            i
          ).confidence === bestConfidence &&
            entries(i).blkPaddr < bestBlkPaddr)
      )
      bestIdx = Mux(isBetter, i.U(log2Ceil(queueSize).W), bestIdx)
      bestPriority = Mux(isBetter, entries(i).priority, bestPriority)
      bestConfidence = Mux(isBetter, entries(i).confidence, bestConfidence)
      bestBlkPaddr = Mux(isBetter, entries(i).blkPaddr, bestBlkPaddr)
      bestValid = Mux(isBetter, valids(i), bestValid)
    }
    bestIdx
  }

  io.empty := empty
  io.full := full
  io.enq.ready := !full
  io.deq.valid := !empty && io.mshrThresholdMet
  io.deq.bits.blkPaddr := entries(deqIdx).blkPaddr
  io.deq.bits.vSetIdx := entries(deqIdx).vSetIdx

  when(io.enq.fire && !io.flush) {
    entries(enqIdx) := io.enq.bits
    valids := valids | UIntToOH(enqIdx, queueSize)
  }

  when(io.deq.fire && !io.flush) {
    valids := valids & ~UIntToOH(deqIdx, queueSize)
  }

  when(io.flush) { valids := 0.U }
}

// ---------------------------------------------------------------------------
// Controller
// ---------------------------------------------------------------------------

class EPIPControllerIO(params: EPIPParams)(implicit p: Parameters)
    extends ICacheBundle {
  // Current instruction block address observed by the BPU (used for table lookup)
  val trigger = Flipped(ValidIO(new Bundle {
    val blkPaddr = UInt((PAddrBits - blockOffBits).W)
    val highCostHint = Bool()
    val controlTrigger = Bool()
    val btbMissTrigger = Bool()
  }))

  // FEC starvation event reported by the retire stage.
  // starvationDistance: number of dynamic instructions retired between the
  // last front-end resteer and this FEC line's use (paper § IV-A).
  val fecLine = Flipped(ValidIO(new Bundle {
    val blkPaddr           = UInt((PAddrBits - blockOffBits).W)
    val blkVaddr           = UInt((PAddrBits - blockOffBits).W)
    val vSetIdx            = UInt(idxBits.W)
    val triggerAddr        = UInt((PAddrBits - blockOffBits).W)
    val highCost           = Bool()
    val starvationDistance = UInt(16.W)
  }))

  val prefetchVaddr = DecoupledIO(new PrefetchEntry)
  val mshrThresholdMet = Input(Bool())
  val flush = Input(Bool())
  val enable = Input(Bool())
  val dropDupWithFtq = Input(Bool())

  val perfInfo = Output(new Bundle {
    val totalPrefetches = UInt(64.W)
    val tableLookups = UInt(64.W)
    val tableHits = UInt(64.W)
    val queueFull = UInt(64.W)
    val crossPageDropped = UInt(64.W)
    val adaptivePromotions = UInt(64.W)
  })
}

/** EPIP Controller
  *
  * Orchestrates three components on top of the PDIP baseline:
  *
  *   1. Counter Controller (adaptive priority assignment): Observes ALL FEC
  *      starvation events coming from the retire stage and maintains two
  *      counters — one for P0 events (distance < 100) and one for P1 events
  *      (100 ≤ distance ≤ 1000) — within a sliding window of 5000 FEC events.
  *      At the end of each window, if P0 events were fewer than P1 events the
  *      controller sets `promoteP1`, which causes P1 candidates to be treated
  *      as P0 (highest urgency) during both learning and issue in the next
  *      window.
  *   2. Enhanced EPIP Table: LFU entry replacement + priority-based candidate
  *      eviction.
  *   3. Page Event Detector: Cross-page candidates are discarded at training
  *      time — when the VPN of the FEC target differs from the VPN of its
  *      trigger, the candidate is never written to the table, saving storage
  *      and avoiding useless cross-page prefetches.
  */
class EPIPController(params: EPIPParams)(implicit p: Parameters)
    extends ICacheModule {
  val io: EPIPControllerIO = IO(new EPIPControllerIO(params))

  private val expandedTargetsPerGroup = 5
  private val maxExpandedTargets =
    params.numTargetsPerEntry * expandedTargetsPerGroup
  // 12 bc Log2Ceil(64B block / 4B word) = block offset bits
  private val pageBlockBits = 12 - blockOffBits

  private def myPercent(numerator: UInt, denominator: UInt): UInt =
    Mux(denominator === 0.U, 0.U, (numerator * 100.U) / denominator)

  // Extract the virtual page number from a block address.
  private def pageTag(blkAddr: UInt): UInt =
    if (pageBlockBits > 0) blkAddr(blkAddr.getWidth - 1, pageBlockBits)
    else blkAddr

  // wrap to avoid exceeding the maximum vSetIdx
  private def incrementVSetIdx(base: UInt, delta: Int): UInt =
    ((base + delta.U)(idxBits - 1, 0)).asUInt

  private def classifyPriority(distance: UInt): UInt =
    Mux(
      distance < params.priority0Distance.U,
      EPIPPriority.p0,
      Mux(
        distance <= params.priority1Distance.U,
        EPIPPriority.p1,
        Mux(
          distance <= params.priority2Distance.U,
          EPIPPriority.p2,
          EPIPPriority.p3
        )
      )
    )

  // -----------------------------------------------------------------------
  // Adaptive priority remapping 
  // -----------------------------------------------------------------------
  private def remapPriority(priority: UInt, promoteP1: Bool): UInt =
    Mux(promoteP1 && priority === EPIPPriority.p1, EPIPPriority.p0, priority)

  private def betterIssue(lhs: EPIPIssueEntry, rhs: EPIPIssueEntry): Bool =
    (lhs.priority < rhs.priority) ||
      (lhs.priority === rhs.priority && lhs.confidence > rhs.confidence) ||
      (lhs.priority === rhs.priority && lhs.confidence === rhs.confidence &&
        lhs.blkPaddr < rhs.blkPaddr)

  private val epipTable = Module(new EPIPTable(params))
  private val prefetchQueue = Module(
    new EPIPPrefetchQueue(params.prefetchQueueSize)
  )
  private val triggerMeta = RegInit(
    VecInit(
      Seq.fill(params.triggerMetaEntries)(0.U.asTypeOf(new EPIPTriggerMeta))
    )
  )

  private val active = io.enable && !io.flush
  private val activeReg = RegNext(active, false.B)

  private val triggerMetaIdxBits = log2Ceil(params.triggerMetaEntries)
  private val triggerMetaIdx =
    io.trigger.bits.blkPaddr(triggerMetaIdxBits - 1, 0)
  private val learnMetaIdx =
    io.fecLine.bits.triggerAddr(triggerMetaIdxBits - 1, 0)
  private val learnedTriggerMeta = triggerMeta(learnMetaIdx)
  private val learnedMetaHit =
    learnedTriggerMeta.valid && learnedTriggerMeta.trigger === io.fecLine.bits.triggerAddr

  when(io.trigger.valid && active) {
    triggerMeta(triggerMetaIdx).valid := true.B
    triggerMeta(triggerMetaIdx).trigger := io.trigger.bits.blkPaddr
    triggerMeta(triggerMetaIdx).highCost := io.trigger.bits.highCostHint
    triggerMeta(triggerMetaIdx).controlTrigger := io.trigger.bits.controlTrigger
    triggerMeta(triggerMetaIdx).btbMissTrigger := io.trigger.bits.btbMissTrigger
  }
  when(io.flush) {
    triggerMeta.foreach(_ := 0.U.asTypeOf(new EPIPTriggerMeta))
  }

  // =========================================================================
  // Counter Controller — Adaptive Priority Assignment
  // =========================================================================
  private val fecEventSeen = io.fecLine.valid && active
  private val fecEventPriority = classifyPriority(
    io.fecLine.bits.starvationDistance
  )

  private val windowEvents = RegInit(
    0.U(log2Ceil(params.priorityAdaptWindow + 1).W)
  )
  private val p0WindowCount = RegInit(0.U(16.W))
  private val p1WindowCount = RegInit(0.U(16.W))
  private val promoteP1 = RegInit(false.B)

  when(fecEventSeen) {
    when(windowEvents === (params.priorityAdaptWindow - 1).U) {
      // End of interval: compare counts and decide for the NEXT interval.
      // Per paper: if P0 events < P1 events, elevate P1 to highest urgency.
      promoteP1 := p0WindowCount < p1WindowCount
      p0WindowCount := 0.U
      p1WindowCount := 0.U
      windowEvents := 0.U
    }.otherwise {
      when(fecEventPriority === EPIPPriority.p0) {
        p0WindowCount := Mux(
          p0WindowCount.andR,
          p0WindowCount,
          p0WindowCount + 1.U
        )
      }
      when(fecEventPriority === EPIPPriority.p1) {
        p1WindowCount := Mux(
          p1WindowCount.andR,
          p1WindowCount,
          p1WindowCount + 1.U
        )
      }
      windowEvents := windowEvents + 1.U
    }
  }
  when(io.flush) {
    windowEvents := 0.U
    p0WindowCount := 0.U
    p1WindowCount := 0.U
    promoteP1 := false.B
  }

  // =========================================================================
  // Page Event Detector 
  // =========================================================================
  private val samePageCandidate =
    pageTag(io.fecLine.bits.blkVaddr) === pageTag(io.fecLine.bits.triggerAddr)

  // Learning path: FEC line seen + high cost + same page.
  // learnedMetaHit is NOT checked here: the triggerMeta has only 64 entries
  // and the hot loop overwrites them long before a cold-miss FEC event fires,
  // making the check structurally always false.  The triggerAddr in the FEC
  // line already comes from s2_req_vaddr on the real MSHR request (via
  // ICache.scala), so it is inherently a valid address — no confirmation
  // against triggerMeta is needed.
  private val fecLearnSeen = io.fecLine.valid && active
  private val learnCandidateSeen =
    fecLearnSeen && io.fecLine.bits.highCost
  private val learnEligible = learnCandidateSeen && samePageCandidate

  // Apply current adaptive remapping to the priority of the incoming FEC miss.
  private val rawLearnPriority = classifyPriority(
    io.fecLine.bits.starvationDistance
  )
  private val effectiveLearnPriority =
    remapPriority(rawLearnPriority, promoteP1)

  private val probabilityPass = if (params.insertProbabilityDivisor <= 1) {
    true.B
  } else {
    val randomWidth = log2Ceil(params.insertProbabilityDivisor)
    val randomValue = LFSR(randomWidth max 2)
    randomValue === 0.U
  }

  epipTable.io.flush := io.flush
  prefetchQueue.io.flush := io.flush
  prefetchQueue.io.mshrThresholdMet := io.mshrThresholdMet

  // Table lookup: probe on every trigger from the BPU
  epipTable.io.lookup.req.valid := io.trigger.valid && active
  epipTable.io.lookup.req.bits.trigger := io.trigger.bits.blkPaddr

  // Table training: store (trigger → FEC target + priority) for eligible events
  epipTable.io.allocate.valid := learnEligible && probabilityPass
  epipTable.io.allocate.bits.trigger := io.fecLine.bits.triggerAddr
  epipTable.io.allocate.bits.target.valid := true.B
  epipTable.io.allocate.bits.target.blkPaddr := io.fecLine.bits.blkPaddr
  epipTable.io.allocate.bits.target.vSetIdx := io.fecLine.bits.vSetIdx
  epipTable.io.allocate.bits.target.compactMask := 0.U
  epipTable.io.allocate.bits.target.confidence := 2.U
  epipTable.io.allocate.bits.target.priority := effectiveLearnPriority

  private val targetsValid = epipTable.io.lookup.resp.valid
  private val targets = epipTable.io.lookup.resp.bits

  // -----------------------------------------------------------------------
  // Expand each target group into up to 5 issue entries
  // (base block + up to 4 compact-mask successors).
  // Apply adaptive remapping at issue time so that priority changes take
  // effect immediately for all entries already in the table.
  // -----------------------------------------------------------------------
  private val expandedValidVec = Wire(Vec(maxExpandedTargets, Bool()))
  private val expandedEntries = Wire(
    Vec(maxExpandedTargets, new EPIPIssueEntry)
  )
  expandedValidVec := VecInit(Seq.fill(maxExpandedTargets)(false.B))
  expandedEntries := VecInit(
    Seq.fill(maxExpandedTargets)(0.U.asTypeOf(new EPIPIssueEntry))
  )

  for (groupIdx <- 0 until params.numTargetsPerEntry) {
    val group = targets(groupIdx)
    val groupEnabled =
      group.valid && (group.confidence >= params.minPrefetchConfidence.U)
    val baseIdx = groupIdx * expandedTargetsPerGroup
    val issuePriority = remapPriority(group.priority, promoteP1)

    expandedValidVec(baseIdx) := groupEnabled
    expandedEntries(baseIdx).blkPaddr := group.blkPaddr
    expandedEntries(baseIdx).vSetIdx := group.vSetIdx
    expandedEntries(baseIdx).priority := issuePriority
    expandedEntries(baseIdx).confidence := group.confidence

    for (offset <- 0 until 4) {
      val slotIdx = baseIdx + offset + 1
      expandedValidVec(slotIdx) := groupEnabled && group.compactMask(offset)
      expandedEntries(slotIdx).blkPaddr := group.blkPaddr + (offset + 1).U
      expandedEntries(slotIdx).vSetIdx := incrementVSetIdx(
        group.vSetIdx,
        offset + 1
      )
      expandedEntries(slotIdx).priority := issuePriority
      expandedEntries(slotIdx).confidence := group.confidence
    }
  }

  private val issueEntries = Reg(Vec(maxExpandedTargets, new EPIPIssueEntry))
  private val issueValids = RegInit(0.U(maxExpandedTargets.W))
  private val issueBusy = issueValids.orR
  private val expandedValidMask = expandedValidVec.asUInt

  when(targetsValid && expandedValidMask.orR && !issueBusy) {
    issueEntries := expandedEntries
    issueValids := expandedValidMask
  }
  when(io.flush) { issueValids := 0.U }

  // Use MUX to avoid the combinational loop from Wire self-indexing
  private val issueSel: UInt = {
    var bestIdx: UInt = 0.U(log2Ceil(maxExpandedTargets).W)
    var bestPriority: UInt = issueEntries(0).priority
    var bestConfidence: UInt = issueEntries(0).confidence
    var bestBlkPaddr: UInt = issueEntries(0).blkPaddr
    var bestValid: Bool = issueValids(0)
    for (i <- 1 until maxExpandedTargets) {
      val isBetter = issueValids(i) && (
        !bestValid ||
          (issueEntries(i).priority < bestPriority) ||
          (issueEntries(i).priority === bestPriority &&
            issueEntries(i).confidence > bestConfidence) ||
          (issueEntries(i).priority === bestPriority &&
            issueEntries(i).confidence === bestConfidence &&
            issueEntries(i).blkPaddr < bestBlkPaddr)
      )
      bestIdx = Mux(isBetter, i.U(log2Ceil(maxExpandedTargets).W), bestIdx)
      bestPriority = Mux(isBetter, issueEntries(i).priority, bestPriority)
      bestConfidence = Mux(isBetter, issueEntries(i).confidence, bestConfidence)
      bestBlkPaddr = Mux(isBetter, issueEntries(i).blkPaddr, bestBlkPaddr)
      bestValid = Mux(isBetter, issueValids(i), bestValid)
    }
    bestIdx
  }

  private val issueCanSend = issueValids.orR && active
  prefetchQueue.io.enq.valid := issueCanSend && !io.dropDupWithFtq
  prefetchQueue.io.enq.bits := issueEntries(issueSel)

  when(prefetchQueue.io.enq.fire) {
    issueValids := issueValids & ~UIntToOH(issueSel, maxExpandedTargets)
  }
  when(issueCanSend && io.dropDupWithFtq) {
    issueValids := issueValids & ~UIntToOH(issueSel, maxExpandedTargets)
  }

  io.prefetchVaddr <> prefetchQueue.io.deq

  // =========================================================================
  // Performance counters
  // =========================================================================
  private val perfTotalPrefetches = RegInit(0.U(64.W))
  private val perfTableLookups = RegInit(0.U(64.W))
  private val perfTableHits = RegInit(0.U(64.W))
  private val perfQueueFull = RegInit(0.U(64.W))
  private val perfCrossPageDropped = RegInit(0.U(64.W))
  private val perfAdaptivePromotions = RegInit(0.U(64.W))
  // Per-priority counts over ALL observed FEC events (not just learned ones)
  private val perfFecP0Events = RegInit(0.U(64.W))
  private val perfFecP1Events = RegInit(0.U(64.W))
  private val perfFecP2Events = RegInit(0.U(64.W))
  private val perfFecP3Events = RegInit(0.U(64.W))
  private val perfFecEventsSeen = RegInit(0.U(64.W))
  private val perfLearnEvents = RegInit(0.U(64.W))

  when(epipTable.io.lookup.req.valid) {
    perfTableLookups := perfTableLookups + 1.U
  }
  when(epipTable.io.lookup.resp.valid) { perfTableHits := perfTableHits + 1.U }
  when(io.prefetchVaddr.fire) {
    perfTotalPrefetches := perfTotalPrefetches + 1.U
  }
  when(prefetchQueue.io.enq.valid && !prefetchQueue.io.enq.ready) {
    perfQueueFull := perfQueueFull + 1.U
  }
  when(fecEventSeen) {
    perfFecEventsSeen := perfFecEventsSeen + 1.U
    switch(fecEventPriority) {
      is(EPIPPriority.p0) { perfFecP0Events := perfFecP0Events + 1.U }
      is(EPIPPriority.p1) { perfFecP1Events := perfFecP1Events + 1.U }
      is(EPIPPriority.p2) { perfFecP2Events := perfFecP2Events + 1.U }
      is(EPIPPriority.p3) { perfFecP3Events := perfFecP3Events + 1.U }
    }
  }
  when(learnEligible && probabilityPass) {
    perfLearnEvents := perfLearnEvents + 1.U
  }
  when(learnCandidateSeen && !samePageCandidate) {
    perfCrossPageDropped := perfCrossPageDropped + 1.U
  }
  // Count adaptive promotion decisions (end-of-interval where P0 < P1)
  when(
    fecEventSeen &&
      windowEvents === (params.priorityAdaptWindow - 1).U &&
      (p0WindowCount < p1WindowCount)
  ) {
    perfAdaptivePromotions := perfAdaptivePromotions + 1.U
  }

  private val hitRate = myPercent(perfTableHits, perfTableLookups)

  when(active && !activeReg) {
    printf(p"[EPIP] active enable=${io.enable} flush=${io.flush}\n")
  }
  when(io.trigger.valid && active) {
    printf(
      p"[EPIP] triggerSeen blkPaddr=0x${Hexadecimal(io.trigger.bits.blkPaddr)}" +
        p" metaIdx=${io.trigger.bits.blkPaddr(triggerMetaIdxBits - 1, 0)}\n"
    )
  }
  when(fecLearnSeen) {
    printf(
      p"[EPIP] fecLearnSeen trigger=0x${Hexadecimal(io.fecLine.bits.triggerAddr)}" +
        p" highCost=${io.fecLine.bits.highCost} metaHit=${learnedMetaHit}\n"
    )
  }
  when(learnCandidateSeen) {
    printf(
      p"[EPIP] learnCandidateSeen trigger=0x${Hexadecimal(io.fecLine.bits.triggerAddr)}" +
        p" samePageCandidate=${samePageCandidate} probabilityPass=${probabilityPass}\n"
    )
  }
  when(learnEligible && probabilityPass) {
    printf(
      p"[EPIP] learn trigger=0x${Hexadecimal(io.fecLine.bits.triggerAddr)}" +
        p" target=0x${Hexadecimal(io.fecLine.bits.blkPaddr)}\n"
    )
  }
  when(targetsValid) {
    printf(
      p"[EPIP] tableHit issueBusy=${issueBusy} expandedValid=${expandedValidMask.orR}\n"
    )
  }
  when(issueCanSend) {
    printf(
      p"[EPIP] issueCanSend dropDup=${io.dropDupWithFtq} enqReady=${prefetchQueue.io.enq.ready}\n"
    )
  }
  when(prefetchQueue.io.deq.valid) {
    printf(p"[EPIP] deqPending prefetchReady=${io.prefetchVaddr.ready}\n")
  }
  when(io.prefetchVaddr.fire) {
    printf(
      p"[EPIP] prefetch blkPaddr=0x${Hexadecimal(io.prefetchVaddr.bits.blkPaddr)}" +
        p" vSetIdx=0x${Hexadecimal(io.prefetchVaddr.bits.vSetIdx)} promoteP1=${promoteP1}\n"
    )
  }
  when(io.flush) {
    printf(
      p"[EPIP] stats totalPrefetches=${perfTotalPrefetches}" +
        p" tableLookups=${perfTableLookups} tableHits=${perfTableHits} hitRate=${hitRate}%" +
        p" queueFull=${perfQueueFull} crossPageDropped=${perfCrossPageDropped}" +
        p" adaptivePromotions=${perfAdaptivePromotions}" +
        p" fecEventsSeen=${perfFecEventsSeen} learnEvents=${perfLearnEvents}" +
        p" fecP0=${perfFecP0Events} fecP1=${perfFecP1Events}" +
        p" fecP2=${perfFecP2Events} fecP3=${perfFecP3Events} promoteP1=${promoteP1}\n"
    )
  }

  io.perfInfo.totalPrefetches := perfTotalPrefetches
  io.perfInfo.tableLookups := perfTableLookups
  io.perfInfo.tableHits := perfTableHits
  io.perfInfo.queueFull := perfQueueFull
  io.perfInfo.crossPageDropped := perfCrossPageDropped
  io.perfInfo.adaptivePromotions := perfAdaptivePromotions
}

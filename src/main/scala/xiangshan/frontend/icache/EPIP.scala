package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
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

/** EPIP prefetch target. Each target holds the block address, virtual set
  * index, and a 2-bit priority derived from the starvation-to-reuse distance.
  */
class EPIPTarget(implicit p: Parameters) extends ICacheBundle {
  val valid: Bool = Bool()
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W)
  val vSetIdx: UInt = UInt(idxBits.W)
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

class EPIPIssueEntry(implicit p: Parameters) extends ICacheBundle {
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W)
  val vSetIdx: UInt = UInt(idxBits.W)
  val priority: UInt = UInt(EPIPPriority.width.W)
}

class EPIPPrefetchEntry(implicit p: Parameters) extends ICacheBundle {
  val blkPaddr = UInt((PAddrBits - blockOffBits).W)
  val vSetIdx  = UInt(idxBits.W)
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
  private val nothing =
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

  // ---- s0: issue read ----
  private val readDoAlloc = io.allocate.valid && !flushActive && !io.flush
  private val readDoLookup =
    io.lookup.req.valid && !readDoAlloc && !flushActive && !io.flush

  table.io.r.req.valid := readDoAlloc || readDoLookup
  table.io.r.req.bits.setIdx := Mux(readDoAlloc, allocSetIdx, lookupSetIdx)

  // ---- s1: BRAM access in flight ----
  private val s1DoAlloc = RegNext(readDoAlloc, false.B)
  private val s1DoLookup = RegNext(readDoLookup, false.B)
  private val s1AllocTrigger = RegEnable(io.allocate.bits.trigger, readDoAlloc)
  private val s1AllocTarget = RegEnable(io.allocate.bits.target, readDoAlloc)
  private val s1LookupTrigger =
    RegEnable(io.lookup.req.bits.trigger, readDoLookup)

  // ---- s1 -> s2: register the BRAM output (cuts the RMW loop in half) ----
  private val s2ReadSet =
    RegEnable(table.io.r.resp.data, s1DoAlloc || s1DoLookup)
  private val s2DoAlloc = RegNext(s1DoAlloc && !io.flush, false.B)
  private val s2DoLookup = RegNext(s1DoLookup && !io.flush, false.B)
  private val s2AllocTrigger = RegEnable(s1AllocTrigger, s1DoAlloc)
  private val s2AllocTarget = RegEnable(s1AllocTarget, s1DoAlloc)
  private val s2LookupTrigger = RegEnable(s1LookupTrigger, s1DoLookup)

  private def increaseLFU(count: UInt): UInt =
    Mux(count.andR, count, count + 1.U)

  private def selectTargetVictim(targets: Vec[EPIPTarget]): UInt = {
    var bestIdx: UInt = 0.U(log2Ceil(params.numTargetsPerEntry).W)
    var bestPriority: UInt = targets(0).priority
    var bestValid: Bool = targets(0).valid
    for (i <- 1 until params.numTargetsPerEntry) {
      val isBetter = !bestValid ||
        (targets(i).valid && targets(i).priority > bestPriority)
      bestIdx = Mux(isBetter, i.U(log2Ceil(params.numTargetsPerEntry).W), bestIdx)
      bestPriority = Mux(isBetter, targets(i).priority, bestPriority)
      bestValid = Mux(isBetter, targets(i).valid, bestValid)
    }
    bestIdx
  }

  // Tournament tree, depth log2(ways). Only used when all ways are valid
  // (caller checks invalidTriggerOH first), so valid bits are not needed.
  // Ties resolve to the lower way index — the trigger-address tiebreak is
  // dropped; it was an arbitrary determinism choice and cost a wide
  // comparator per chain stage.
  private def selectTriggerVictim(entries: Vec[EPIPTableEntry]): UInt = {
    def tournament(cands: Seq[(UInt, UInt)]): (UInt, UInt) = cands match {
      case Seq(only) => only
      case _ =>
        val (l, r) = cands.splitAt(cands.length / 2)
        val (lIdx, lLfu) = tournament(l)
        val (rIdx, rLfu) = tournament(r)
        val takeRight = rLfu < lLfu
        (Mux(takeRight, rIdx, lIdx), Mux(takeRight, rLfu, lLfu))
    }
    tournament(entries.zipWithIndex.map { case (e, i) =>
      (i.U(log2Ceil(params.numWaysPerSet).W), e.lfuCount)
    })._1
  }

  // ---- s2: match / victim / update on REGISTERED data ----
  private val matchTrigger = VecInit(
    s2ReadSet.map(entry => entry.valid && entry.trigger === s2LookupTrigger)
  ).asUInt
  private val hitTrigger = PriorityEncoder(matchTrigger)
  private val hitTriggerOH = PriorityEncoderOH(matchTrigger)
  private val hit = matchTrigger.orR

  io.lookup.resp.valid := s2DoLookup && hit
  io.lookup.resp.bits := Mux(hit, s2ReadSet(hitTrigger).targets, nothing)

  private val writeData = Wire(
    Vec(
      params.numWaysPerSet,
      new EPIPTableEntry(params.numTargetsPerEntry, params.lfuCounterBits)
    )
  )
  private val writeMask = Wire(UInt(params.numWaysPerSet.W))
  private val writeValid = Wire(Bool())
  writeData := s2ReadSet
  writeMask := 0.U
  writeValid := false.B

  when(s2DoAlloc) {
    val existingTriggerOH = VecInit(
      s2ReadSet.map(entry => entry.valid && entry.trigger === s2AllocTrigger)
    ).asUInt

    when(existingTriggerOH.orR) {
      val existingTrigger = PriorityEncoder(existingTriggerOH)
      val updatedEntry = WireInit(s2ReadSet(existingTrigger))
      val targetSlots = s2ReadSet(existingTrigger).targets

      updatedEntry.lfuCount := increaseLFU(s2ReadSet(existingTrigger).lfuCount)

      val existingTargetOH = VecInit(
        targetSlots.map(t => t.valid && t.blkPaddr === s2AllocTarget.blkPaddr)
      ).asUInt
      val invalidTargetOH = VecInit(targetSlots.map(t => !t.valid)).asUInt

      when(existingTargetOH.orR) {
        val idx = PriorityEncoder(existingTargetOH)
        updatedEntry.targets(idx).priority :=
          Mux(
            s2AllocTarget.priority < targetSlots(idx).priority,
            s2AllocTarget.priority,
            targetSlots(idx).priority
          )
      }.otherwise {
        val replaceIdx = Mux(
          invalidTargetOH.orR,
          PriorityEncoder(invalidTargetOH),
          selectTargetVictim(targetSlots)
        )
        updatedEntry.targets(replaceIdx) := s2AllocTarget
        updatedEntry.targets(replaceIdx).valid := true.B
      }

      writeData(existingTrigger) := updatedEntry
      writeMask := UIntToOH(existingTrigger, params.numWaysPerSet)
      writeValid := true.B

    }.otherwise {
      val invalidTriggerOH = VecInit(s2ReadSet.map(e => !e.valid)).asUInt
      val replaceTrigger = Mux(
        invalidTriggerOH.orR,
        PriorityEncoder(invalidTriggerOH),
        selectTriggerVictim(s2ReadSet)
      )
      val newEntry = Wire(
        new EPIPTableEntry(params.numTargetsPerEntry, params.lfuCounterBits)
      )
      newEntry.valid := true.B
      newEntry.trigger := s2AllocTrigger
      newEntry.lfuCount := 1.U
      newEntry.targets := nothing
      newEntry.targets(0) := s2AllocTarget
      newEntry.targets(0).valid := true.B
      writeData(replaceTrigger) := newEntry
      writeMask := UIntToOH(replaceTrigger, params.numWaysPerSet)
      writeValid := true.B
    }

  }.elsewhen(s2DoLookup && hit) {
    val updatedEntry = WireInit(s2ReadSet(hitTrigger))
    updatedEntry.lfuCount := increaseLFU(s2ReadSet(hitTrigger).lfuCount)
    writeData(hitTrigger) := updatedEntry
    writeMask := hitTriggerOH
    writeValid := true.B
  }

  private val flushWriteValid = flushActive
  private val flushWriteData = VecInit(Seq.fill(params.numWaysPerSet)(zeroEntry))
  private val flushWriteMask = Fill(params.numWaysPerSet, 1.U(1.W))

  table.io.w.req.valid := flushWriteValid || writeValid
  table.io.w.req.bits.apply(
    data = Mux(flushWriteValid, flushWriteData, writeData),
    setIdx = Mux(
      flushWriteValid,
      flushSetIdx,
      Mux(
        s2DoAlloc,
        s2AllocTrigger(setIdxBits - 1, 0),
        s2LookupTrigger(setIdxBits - 1, 0)
      )
    ),
    waymask = Mux(flushWriteValid, flushWriteMask, writeMask)
  )
}


class EPIPPrefetchQueueIO(queueSize: Int)(implicit p: Parameters)
    extends ICacheBundle {
  val enq = Flipped(DecoupledIO(new EPIPIssueEntry))
  val deq = DecoupledIO(new EPIPPrefetchEntry)
  val flush = Input(Bool())
  val mshrThresholdMet = Input(Bool())
  val empty = Output(Bool())
  val full = Output(Bool())
}

/** Priority-banked prefetch queue: one circular FIFO per priority level.
 *  Dequeue always takes from the lowest-numbered non-empty bank.
 *  Eliminates the N-wide priority scan; cross-bank selection is a 4-wide
 *  PriorityEncoder which is trivially fast.
 */
class EPIPPrefetchQueue(queueSize: Int)(implicit p: Parameters)
    extends ICacheModule {
  val io: EPIPPrefetchQueueIO = IO(new EPIPPrefetchQueueIO(queueSize))

  private val numBanks  = 4
  private val bankDepth = queueSize / numBanks
  require(queueSize % numBanks == 0, s"queueSize ($queueSize) must be divisible by $numBanks")

  private val bankEntries = Seq.tabulate(numBanks) { _ =>
    RegInit(VecInit(Seq.fill(bankDepth)(0.U.asTypeOf(new EPIPPrefetchEntry))))
  }
  private val bankHead  = Seq.fill(numBanks)(RegInit(0.U(log2Ceil(bankDepth).W)))
  private val bankTail  = Seq.fill(numBanks)(RegInit(0.U(log2Ceil(bankDepth).W)))
  private val bankCount = Seq.fill(numBanks)(RegInit(0.U((log2Ceil(bankDepth) + 1).W)))

  private val bankNonEmpty = VecInit(bankCount.map(_ =/= 0.U))
  private val bankFull     = VecInit(bankCount.map(_ === bankDepth.U))
  private val anyValid     = bankNonEmpty.asUInt.orR
  private val deqBank      = PriorityEncoder(bankNonEmpty.asUInt)

  // -------------------------------------------------------------------
  // Output register stage: deq.bits is driven directly from a flop.
  // The head-pointer mux chain now terminates here instead of feeding
  // downstream logic combinationally.
  // -------------------------------------------------------------------
  private val outValid = RegInit(false.B)
  private val outBits  = Reg(new EPIPPrefetchEntry)

  private val outFree = !outValid || io.deq.fire
  private val refill  = anyValid && outFree && !io.flush

  private val selBits = Mux1H((0 until numBanks).map(b =>
    (deqBank === b.U) -> bankEntries(b)(bankHead(b))
  ))

  when(refill) { outBits := selBits }
  when(io.flush)           { outValid := false.B }
    .elsewhen(refill)      { outValid := true.B }
    .elsewhen(io.deq.fire) { outValid := false.B }

  io.deq.valid := outValid && io.mshrThresholdMet
  io.deq.bits  := outBits

  io.empty     := !anyValid && !outValid
  io.full      := bankFull.asUInt.andR
  io.enq.ready := !bankFull(io.enq.bits.priority)

  for (b <- 0 until numBanks) {
    val doEnq = io.enq.fire && !io.flush && io.enq.bits.priority === b.U
    val doDeq = refill && deqBank === b.U   // pop when entry moves into out reg

    when(doEnq) {
      bankEntries(b)(bankTail(b)).blkPaddr := io.enq.bits.blkPaddr
      bankEntries(b)(bankTail(b)).vSetIdx  := io.enq.bits.vSetIdx
      bankTail(b) := Mux(bankTail(b) === (bankDepth - 1).U, 0.U, bankTail(b) + 1.U)
    }
    when(doDeq) {
      bankHead(b) := Mux(bankHead(b) === (bankDepth - 1).U, 0.U, bankHead(b) + 1.U)
    }
    when(doEnq && !doDeq)      { bankCount(b) := bankCount(b) + 1.U }
      .elsewhen(doDeq && !doEnq) { bankCount(b) := bankCount(b) - 1.U }
  }

  when(io.flush) {
    for (b <- 0 until numBanks) {
      bankHead(b)  := 0.U
      bankTail(b)  := 0.U
      bankCount(b) := 0.U
    }
  }
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

  val fecLine = Flipped(ValidIO(new Bundle {
    val blkPaddr           = UInt((PAddrBits - blockOffBits).W)
    val blkVaddr           = UInt((PAddrBits - blockOffBits).W)
    val vSetIdx            = UInt(idxBits.W)
    val triggerAddr        = UInt((PAddrBits - blockOffBits).W)
    val highCost           = Bool()
    val starvationDistance = UInt(16.W)
  }))

  val prefetchVaddr = DecoupledIO(new EPIPPrefetchEntry)
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

/** EPIP Controllerrr
  */
class EPIPController(params: EPIPParams)(implicit p: Parameters)
    extends ICacheModule {
  val io: EPIPControllerIO = IO(new EPIPControllerIO(params))

  // 12 bc Log2Ceil(64B block / 4B word) = block offset bits
  private val pageBlockBits = 12 - blockOffBits

  private def myPercent(numerator: UInt, denominator: UInt): UInt =
    Mux(denominator === 0.U, 0.U, (numerator * 100.U) / denominator)

  // Extract the virtual page number from a block address.
  private def pageTag(blkAddr: UInt): UInt =
    if (pageBlockBits > 0) blkAddr(blkAddr.getWidth - 1, pageBlockBits)
    else blkAddr

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
    when(windowEvents === (params.priorityAdaptWindow - 1).U) {//When 5000 FEC events
      //Elevate P1 to P0 if there were more P1 events than P0
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

  private val fecLearnSeen = io.fecLine.valid && active
  private val learnCandidateSeen =
    fecLearnSeen && io.fecLine.bits.highCost
  private val learnEligible = learnCandidateSeen && samePageCandidate


  private val rawLearnPriority = classifyPriority(
    io.fecLine.bits.starvationDistance
  )
  private val effectiveLearnPriority =
    remapPriority(rawLearnPriority, promoteP1)

  epipTable.io.flush := io.flush
  prefetchQueue.io.flush := io.flush
  prefetchQueue.io.mshrThresholdMet := io.mshrThresholdMet
 
  epipTable.io.lookup.req.valid := io.trigger.valid && active
  epipTable.io.lookup.req.bits.trigger := io.trigger.bits.blkPaddr

  epipTable.io.allocate.valid := learnEligible
  epipTable.io.allocate.bits.trigger := io.fecLine.bits.triggerAddr
  epipTable.io.allocate.bits.target.valid := true.B
  epipTable.io.allocate.bits.target.blkPaddr := io.fecLine.bits.blkPaddr
  epipTable.io.allocate.bits.target.vSetIdx := io.fecLine.bits.vSetIdx
  epipTable.io.allocate.bits.target.priority := effectiveLearnPriority

  private val targetsValid = epipTable.io.lookup.resp.valid
  private val targets = epipTable.io.lookup.resp.bits

  // -----------------------------------------------------------------------
  // Issue the 2 stored targets directly, applying adaptive remapping.
  // -----------------------------------------------------------------------
  private val issueEntries = Reg(Vec(params.numTargetsPerEntry, new EPIPIssueEntry))
  private val issueValids  = RegInit(0.U(params.numTargetsPerEntry.W))
  private val issueBusy    = issueValids.orR

  when(targetsValid && targets.map(target => target.valid).reduce(_ || _) && !issueBusy) {
    for (i <- 0 until params.numTargetsPerEntry) {
      issueEntries(i).blkPaddr := targets(i).blkPaddr
      issueEntries(i).vSetIdx  := targets(i).vSetIdx
      issueEntries(i).priority := remapPriority(targets(i).priority, promoteP1)
    }
    issueValids := VecInit(targets.map(_.valid)).asUInt
  }
  when(io.flush) { issueValids := 0.U }

  private val issueSel: UInt = {
    var bestIdx: UInt      = 0.U(log2Ceil(params.numTargetsPerEntry).W)
    var bestPriority: UInt = issueEntries(0).priority
    var bestValid: Bool    = issueValids(0)
    for (i <- 1 until params.numTargetsPerEntry) {
      val isBetter = issueValids(i) && (!bestValid || issueEntries(i).priority < bestPriority)
      bestIdx      = Mux(isBetter, i.U(log2Ceil(params.numTargetsPerEntry).W), bestIdx)
      bestPriority = Mux(isBetter, issueEntries(i).priority, bestPriority)
      bestValid    = Mux(isBetter, issueValids(i), bestValid)
    }
    bestIdx
  }

  private val issueCanSend = issueValids.orR && active
  prefetchQueue.io.enq.valid := issueCanSend && !io.dropDupWithFtq
  prefetchQueue.io.enq.bits  := issueEntries(issueSel)

  when(prefetchQueue.io.enq.fire) {
    issueValids := issueValids & ~UIntToOH(issueSel, params.numTargetsPerEntry)
  }
  when(issueCanSend && io.dropDupWithFtq) {
    issueValids := issueValids & ~UIntToOH(issueSel, params.numTargetsPerEntry)
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
  when(learnEligible) {
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
        p" samePageCandidate=${samePageCandidate}\n"
    )
  }
  when(learnEligible) {
    printf(
      p"[EPIP] learn trigger=0x${Hexadecimal(io.fecLine.bits.triggerAddr)}" +
        p" target=0x${Hexadecimal(io.fecLine.bits.blkPaddr)}\n"
    )
  }
  when(targetsValid) {
    printf(
      p"[EPIP] tableHit issueBusy=${issueBusy} anyTargetValid=${targets.map(_.valid).reduce(_ || _)}\n"
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

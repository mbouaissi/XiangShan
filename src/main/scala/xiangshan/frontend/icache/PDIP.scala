package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR
import org.chipsalliance.cde.config.Parameters
import utility._
import utility._
import xiangshan.frontend._

/** PDIP (Prefetch-Directed Instruction Prefetching) Parameters
  */
case class PDIPParams(
    enabled: Boolean = true,
    numTableSets: Int = 512, // Number of sets in PDIP table
    numWaysPerSet: Int = 8, // Associativity of PDIP table
    numTargetsPerEntry: Int = 2, // Paper default: 2 target groups per trigger
    prefetchQueueSize: Int = 40, // Size of prefetch queue (cacheline entries)
    mshrThreshold: Int = 2, // Paper-style minimum free MSHR budget before issue
    insertProbabilityDivisor: Int = 4, // Paper-style reduced-probability learning
    minPrefetchConfidence: Int = 2, // Keep local confidence gating by default
    triggerMetaEntries: Int = 64 // Small trigger-class sidecar for selective learning
)

/** PDIP Trigger-Candidate Association Maps
  */
class PDIPTableEntry(numTargets: Int)(implicit p: Parameters)
    extends ICacheBundle {
  val valid: Bool = Bool() // Entry is valid
  val trigger: UInt = UInt(
    (PAddrBits - blockOffBits).W
  ) // Trigger block address
  val targets: Vec[PDIPTarget] = Vec(numTargets, new PDIPTarget) // Line targets
  val lru: UInt = UInt(log2Ceil(numTargets).W) // LRU for target replacement
}

/** PDIP Prefetch Target which is a FEC line 
  */
class PDIPTarget(implicit p: Parameters) extends ICacheBundle {
  val valid: Bool = Bool() // Target is valid
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W) // Base FEC block address
  val vSetIdx: UInt = UInt(idxBits.W) // Virtual set index of the base block
  val compactMask: UInt = UInt(4.W) // Following four sequential blocks
  val confidence: UInt = UInt(2.W) // 2-bit confidence counter
}

class PDIPTriggerMeta(implicit p: Parameters) extends ICacheBundle {
  val valid: Bool = Bool()
  val trigger: UInt = UInt((PAddrBits - blockOffBits).W)
  val highCost: Bool = Bool()
  val controlTrigger: Bool = Bool()
  val btbMissTrigger: Bool = Bool()
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

  private val zeroEntry = 0.U.asTypeOf(new PDIPTableEntry(params.numTargetsPerEntry))
  private val zeroTargets =
    VecInit(Seq.fill(params.numTargetsPerEntry)(0.U.asTypeOf(new PDIPTarget)))

  private val table = Module(
    new SRAMTemplate(
      new PDIPTableEntry(params.numTargetsPerEntry),
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
  private val rrWay = RegInit(0.U(log2Ceil(params.numWaysPerSet).W))

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
  private val s1LookupTrigger = RegEnable(io.lookup.req.bits.trigger, readDoLookup)
  private val readSet = table.io.r.resp.data

  private def compactableOffset(
      target: PDIPTarget,
      newBlkPaddr: UInt
  ): UInt = {
    newBlkPaddr - target.blkPaddr - 1.U
  }

  private def canCompact(target: PDIPTarget, newBlkPaddr: UInt): Bool = {
    target.valid &&
    newBlkPaddr > target.blkPaddr &&
    newBlkPaddr <= (target.blkPaddr + 4.U)
  }

  // Lookup logic
  private val matchWay = VecInit(
    readSet.map(entry =>
      entry.valid && entry.trigger === s1LookupTrigger
    )
  ).asUInt

  private val hitWay = PriorityEncoder(
    matchWay
  ) // PriorityEncoder is safe even if >1 bit set
  private val hitWayOH = PriorityEncoderOH(matchWay)
  private val hit = matchWay =/= 0.U

  // Output targets if hit
  io.lookup.resp.valid := s1DoLookup && hit
  io.lookup.resp.bits := Mux(
    hit,
    readSet(hitWay).targets,
    zeroTargets
  )

  private val allocWriteData = Wire(Vec(params.numWaysPerSet, new PDIPTableEntry(params.numTargetsPerEntry)))
  private val allocWriteMask = Wire(UInt(params.numWaysPerSet.W))
  private val allocWriteValid = Wire(Bool())
  allocWriteData := readSet
  allocWriteMask := 0.U
  allocWriteValid := false.B

  when(s1DoAlloc) {
    val existingWayOH = VecInit(
      readSet.map(entry =>
        entry.valid && entry.trigger === s1AllocTrigger
      )
    ).asUInt

    when(existingWayOH =/= 0.U) {
      val existingWay = PriorityEncoder(
        existingWayOH
      )
      val existingWayOH1 = PriorityEncoderOH(existingWayOH)
      val extraWays = existingWayOH & ~existingWayOH1

      when(extraWays.orR) {
        for (w <- 0 until params.numWaysPerSet) {
          when(extraWays(w)) {
            allocWriteData(w).valid := false.B
          }
        }
      }

      val entry = readSet(existingWay)
      val updatedEntry = WireInit(entry)
      val targetSlot = entry.targets
      val invalidSlot = VecInit(targetSlot.map(x => !x.valid)).asUInt
      val hasInvalidSlot = invalidSlot.orR
      val replaceIdx =
        Mux(hasInvalidSlot, PriorityEncoder(invalidSlot), entry.lru)

      val targetExists = VecInit(
        targetSlot.map(t => {
          val offset = compactableOffset(t, s1AllocTarget.blkPaddr)
          (t.valid && t.blkPaddr === s1AllocTarget.blkPaddr) ||
          (canCompact(t, s1AllocTarget.blkPaddr) && t.compactMask(offset(1, 0)))
        })
      ).asUInt.orR

      val targetCanCompact = VecInit(
        targetSlot.map(t =>
          canCompact(t, s1AllocTarget.blkPaddr)
        )
      ).asUInt.orR

      when(!targetExists) {
        when(targetCanCompact) {
          val compactIdx = PriorityEncoder(
            VecInit(
              targetSlot.map(t => canCompact(t, s1AllocTarget.blkPaddr))
            )
          )
          val compactOffset = compactableOffset(
            targetSlot(compactIdx),
            s1AllocTarget.blkPaddr
          )
          updatedEntry.targets(compactIdx).compactMask :=
            targetSlot(compactIdx).compactMask | UIntToOH(compactOffset(1, 0), 4)
          when(targetSlot(compactIdx).confidence < 3.U) {
            updatedEntry.targets(compactIdx).confidence :=
              targetSlot(compactIdx).confidence + 1.U
          }
          for (i <- 0 until params.numTargetsPerEntry) {
            when(i.U =/= compactIdx && targetSlot(i).confidence =/= 0.U) {
              updatedEntry.targets(i).confidence := targetSlot(i).confidence - 1.U
            }
          }
        }.otherwise {
          updatedEntry.targets(replaceIdx) := s1AllocTarget
          updatedEntry.targets(replaceIdx).valid := true.B
          updatedEntry.targets(replaceIdx).compactMask := 0.U
          updatedEntry.targets(replaceIdx).confidence := 2.U

          for (i <- 0 until params.numTargetsPerEntry) {
            when(i.U =/= replaceIdx && targetSlot(i).confidence =/= 0.U) {
              updatedEntry.targets(i).confidence := targetSlot(i).confidence - 1.U
            }
          }

          updatedEntry.lru := Mux(
            replaceIdx === (params.numTargetsPerEntry - 1).U,
            0.U,
            replaceIdx + 1.U
          )
        }
      }.otherwise {
        val existingTargetIdx = PriorityEncoder(
          VecInit(targetSlot.map(t => {
            val offset = compactableOffset(t, s1AllocTarget.blkPaddr)
            (t.valid && t.blkPaddr === s1AllocTarget.blkPaddr) ||
            (canCompact(t, s1AllocTarget.blkPaddr) && t.compactMask(offset(1, 0)))
          }))
        )
        when(targetSlot(existingTargetIdx).confidence < 3.U) {
          updatedEntry.targets(existingTargetIdx).confidence :=
            targetSlot(existingTargetIdx).confidence + 1.U
        }
        for (i <- 0 until params.numTargetsPerEntry) {
          when(i.U =/= existingTargetIdx && targetSlot(i).confidence =/= 0.U) {
            updatedEntry.targets(i).confidence := targetSlot(i).confidence - 1.U
          }
        }
      }

      allocWriteData(existingWay) := updatedEntry
      allocWriteMask := extraWays | existingWayOH1
      allocWriteValid := true.B
    }.otherwise {
      val invalidWay = VecInit(readSet.map(x => !x.valid)).asUInt
      val hasInvalidWay = invalidWay.orR

      when(!hasInvalidWay) {
        rrWay := Mux(rrWay === (params.numWaysPerSet - 1).U, 0.U, rrWay + 1.U)
      }

      val replaceWayOH = Mux(
        hasInvalidWay,
        invalidWay,
        UIntToOH(rrWay, params.numWaysPerSet)
      )
      val newEntry = Wire(new PDIPTableEntry(params.numTargetsPerEntry))
      newEntry.valid := true.B
      newEntry.trigger := s1AllocTrigger
      newEntry.targets := zeroTargets
      newEntry.targets(0) := s1AllocTarget
      newEntry.targets(0).valid := true.B
      newEntry.targets(0).compactMask := 0.U
      newEntry.targets(0).confidence := 2.U
      newEntry.lru := 1.U

      val replaceWay = OHToUInt(replaceWayOH)
      allocWriteData(replaceWay) := newEntry
      allocWriteMask := replaceWayOH
      allocWriteValid := true.B
    }
  }

  private val flushWriteValid = flushActive
  private val flushWriteData = VecInit(Seq.fill(params.numWaysPerSet)(zeroEntry))
  private val flushWriteMask = Fill(params.numWaysPerSet, 1.U(1.W))

  table.io.w.req.valid := flushWriteValid || allocWriteValid
  table.io.w.req.bits.apply(
    data = Mux(flushWriteValid, flushWriteData, allocWriteData),
    setIdx = Mux(flushWriteValid, flushSetIdx, s1AllocTrigger(setIdxBits - 1, 0)),
    waymask = Mux(flushWriteValid, flushWriteMask, allocWriteMask)
  )
}

/** Prefetch Queue Entry
  */
class PrefetchEntry(implicit p: Parameters) extends ICacheBundle {
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W)
  val vSetIdx: UInt = UInt(log2Ceil(nSets).W)
}

class PrefetchQueueEntry(implicit p: Parameters) extends ICacheBundle {
  val valid: Bool = Bool()
}

/** Prefetch Queue IO
  */
class PDIPPrefetchQueueIO(queueSize: Int)(implicit p: Parameters)
    extends ICacheBundle {
  val enq = Flipped(DecoupledIO(new PrefetchEntry))
  val deq = DecoupledIO(new PrefetchEntry)

  val flush = Input(Bool())
  val mshrThresholdMet = Input(Bool())

  val empty = Output(Bool())
  val full = Output(Bool())
}

/** Prefetch Queue Decouples PDIP prefetch requests from ICache MSHR allocation
  */
class PDIPPrefetchQueue(queueSize: Int)(implicit p: Parameters)
    extends ICacheModule {
  val io: PDIPPrefetchQueueIO = IO(new PDIPPrefetchQueueIO(queueSize))

  private val queue = RegInit(
    VecInit(Seq.fill(queueSize)(0.U.asTypeOf(new PrefetchEntry)))
  )

  private val head = RegInit(0.U(log2Ceil(queueSize).W))
  private val tail = RegInit(0.U(log2Ceil(queueSize).W))
  private val fullFlag = RegInit(false.B)

  private val ptr_match = head === tail
  private val empty = ptr_match && !fullFlag
  private val full = ptr_match && fullFlag

  io.empty := empty
  io.full := full

  // Enqueue
  io.enq.ready := !full
  when(io.enq.fire && !io.flush) {
    queue(tail) := io.enq.bits

    val tailNext = Mux(tail === (queueSize - 1).U, 0.U, tail + 1.U)
    tail := tailNext

    when(ptr_match) { fullFlag := true.B }
  }

  // Dequeue
  io.deq.valid := !empty && io.mshrThresholdMet
  io.deq.bits := queue(head)

  when(io.deq.fire && !io.flush) {
    val headNext = Mux(head === (queueSize - 1).U, 0.U, head + 1.U)
    head := headNext

    fullFlag := false.B
  }

  // Flush
  when(io.flush) {
    head := 0.U
    tail := 0.U
    fullFlag := false.B
  }
}

/** PDIP Controller IO
  */
class PDIPControllerIO(params: PDIPParams)(implicit p: Parameters)
    extends ICacheBundle {
  // Input: Current instruction block address from BPU/IFU
  val trigger = Flipped(ValidIO(new Bundle {
    val blkPaddr = UInt((PAddrBits - blockOffBits).W)
    val highCostHint = Bool()
    val controlTrigger = Bool()
    val btbMissTrigger = Bool()
  }))

  // Input: FEC line detected (from FECTracker)
  val fecLine = Flipped(ValidIO(new Bundle {
    val blkPaddr = UInt((PAddrBits - blockOffBits).W)
    val vSetIdx = UInt(idxBits.W)
    val triggerAddr =
      UInt((PAddrBits - blockOffBits).W) // Trigger that caused this FEC
    val highCost = Bool()
  }))

  // Output: Prefetch request to ICache
  val prefetchVaddr: DecoupledIO[PrefetchEntry] = DecoupledIO(new PrefetchEntry)
  // MSHR availability check
  val mshrThresholdMet = Input(Bool())

  // Flush signal
  val flush = Input(Bool())

  // Enable/disable PDIP
  val enable = Input(Bool())

  // Pulse when a PDIP prefetch is dropped because FTQ already has same target.
  val dropDupWithFtq = Input(Bool())

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

  private val expandedTargetsPerGroup = 5
  private val maxExpandedTargets =
    params.numTargetsPerEntry * expandedTargetsPerGroup

  // Helper for debug-friendly percentage printing without divide-by-zero.
  private def safePercent(numerator: UInt, denominator: UInt): UInt = {
    Mux(denominator === 0.U, 0.U, (numerator * 100.U) / denominator)
  }

  private def safeBasisPoints(numerator: UInt, denominator: UInt): UInt = {
    Mux(denominator === 0.U, 0.U, (numerator * 10000.U) / denominator)
  }

  private val pdipTable = Module(new PDIPTable(params))
  private val prefetchQueue = Module(
    new PDIPPrefetchQueue(params.prefetchQueueSize)
  )
  private val triggerMeta = RegInit(
    VecInit(Seq.fill(params.triggerMetaEntries)(0.U.asTypeOf(new PDIPTriggerMeta)))
  )

  private val active = io.enable && !io.flush
  private val activeReg = RegNext(active, false.B)

  private def incrementVSetIdx(base: UInt, delta: Int): UInt = {
    ((base + delta.U)(idxBits - 1, 0)).asUInt
  }

  private val triggerMetaIdxBits = log2Ceil(params.triggerMetaEntries)
  private val triggerMetaIdx =
    io.trigger.bits.blkPaddr(triggerMetaIdxBits - 1, 0)
  private val learnMetaIdx =
    io.fecLine.bits.triggerAddr(triggerMetaIdxBits - 1, 0)
  private val learnedTriggerMeta = triggerMeta(learnMetaIdx)
  private val learnedMetaHit =
    learnedTriggerMeta.valid &&
      learnedTriggerMeta.trigger === io.fecLine.bits.triggerAddr

  when(io.trigger.valid && active) {
    triggerMeta(triggerMetaIdx).valid := true.B
    triggerMeta(triggerMetaIdx).trigger := io.trigger.bits.blkPaddr
    triggerMeta(triggerMetaIdx).highCost := io.trigger.bits.highCostHint
    triggerMeta(triggerMetaIdx).controlTrigger := io.trigger.bits.controlTrigger
    triggerMeta(triggerMetaIdx).btbMissTrigger := io.trigger.bits.btbMissTrigger
  }
  when(io.flush) {
    triggerMeta.foreach(_ := 0.U.asTypeOf(new PDIPTriggerMeta))
  }

  private val probabilityPass = if (params.insertProbabilityDivisor <= 1) {
    true.B
  } else {
    val randomWidth = log2Ceil(params.insertProbabilityDivisor)
    val randomValue = LFSR(randomWidth max 2)
    randomValue === 0.U
  }

  private val fecLearnSeen = io.fecLine.valid && active
  private val highPriorityLearn =
    fecLearnSeen &&
      learnedMetaHit &&
      io.fecLine.bits.highCost
  private val learnEligible =
    highPriorityLearn && probabilityPass

  pdipTable.io.flush := io.flush
  prefetchQueue.io.flush := io.flush
  prefetchQueue.io.mshrThresholdMet := io.mshrThresholdMet

  // Table lookup on trigger
  pdipTable.io.lookup.req.valid := io.trigger.valid && active
  pdipTable.io.lookup.req.bits.trigger := io.trigger.bits.blkPaddr

  // Learn only high-cost FEC lines, then apply reduced-probability insertion.
  pdipTable.io.allocate.valid := learnEligible
  pdipTable.io.allocate.bits.trigger := io.fecLine.bits.triggerAddr
  pdipTable.io.allocate.bits.target.valid := true.B
  pdipTable.io.allocate.bits.target.blkPaddr := io.fecLine.bits.blkPaddr
  pdipTable.io.allocate.bits.target.vSetIdx := io.fecLine.bits.vSetIdx
  pdipTable.io.allocate.bits.target.compactMask := 0.U
  pdipTable.io.allocate.bits.target.confidence := 2.U

  private val targetsValid = pdipTable.io.lookup.resp.valid
  private val targets = pdipTable.io.lookup.resp.bits

  private val issueEntries = Reg(Vec(maxExpandedTargets, new PrefetchEntry))
  private val issueValids = RegInit(0.U(maxExpandedTargets.W))
  private val issueBusy = issueValids.orR

  private val expandedValidVec = Wire(Vec(maxExpandedTargets, Bool()))
  private val expandedEntries = Wire(Vec(maxExpandedTargets, new PrefetchEntry))
  expandedValidVec := VecInit(Seq.fill(maxExpandedTargets)(false.B))
  expandedEntries := VecInit(Seq.fill(maxExpandedTargets)(0.U.asTypeOf(new PrefetchEntry)))

  for (groupIdx <- 0 until params.numTargetsPerEntry) {
    val group = targets(groupIdx)
    val groupEnabled = group.valid && (group.confidence >= params.minPrefetchConfidence.U)
    val baseIdx = groupIdx * expandedTargetsPerGroup
    expandedValidVec(baseIdx) := groupEnabled
    expandedEntries(baseIdx).blkPaddr := group.blkPaddr
    expandedEntries(baseIdx).vSetIdx := group.vSetIdx
    for (offset <- 0 until 4) {
      expandedValidVec(baseIdx + offset + 1) := groupEnabled && group.compactMask(offset)
      expandedEntries(baseIdx + offset + 1).blkPaddr := group.blkPaddr + (offset + 1).U
      expandedEntries(baseIdx + offset + 1).vSetIdx :=
        incrementVSetIdx(group.vSetIdx, offset + 1)
    }
  }

  private val expandedValidOH = expandedValidVec.asUInt
  private val hasExpandedTargets = expandedValidOH.orR

  when(targetsValid && hasExpandedTargets && !issueBusy) {
    issueEntries := expandedEntries
    issueValids := expandedValidOH
  }

  private val issueSel = PriorityEncoder(issueValids)
  private val issueCanSend = issueValids.orR && active

  prefetchQueue.io.enq.valid := issueCanSend && !io.dropDupWithFtq
  prefetchQueue.io.enq.bits := issueEntries(issueSel)

  when(prefetchQueue.io.enq.fire) {
    issueValids := issueValids & ~UIntToOH(issueSel, maxExpandedTargets)
  }
  when(issueCanSend && io.dropDupWithFtq) {
    issueValids := issueValids & ~UIntToOH(issueSel, maxExpandedTargets)
  }
  when(io.flush) {
    issueValids := 0.U
  }

  // Output
  io.prefetchVaddr <> prefetchQueue.io.deq

  when(active && !activeReg) {
    printf(p"[PDIP] active enable=${io.enable} flush=${io.flush}\n")
  }
  when(io.prefetchVaddr.fire) {
    printf(p"[PDIP] prefetch blkPaddr=0x${Hexadecimal(io.prefetchVaddr.bits.blkPaddr)} vSetIdx=0x${Hexadecimal(io.prefetchVaddr.bits.vSetIdx)}\n")
  }
  when(prefetchQueue.io.enq.valid && !prefetchQueue.io.enq.ready) {
    printf(p"[PDIP] queue backpressure enqBlkPaddr=0x${Hexadecimal(prefetchQueue.io.enq.bits.blkPaddr)}\n")
  }

  // Perf counters, for bencmarking 
  private val perfTotalPrefetches = RegInit(0.U(64.W))
  private val perfTableLookups = RegInit(0.U(64.W))
  private val perfTableHits = RegInit(0.U(64.W))
  private val perfQueueFull = RegInit(0.U(64.W))
  private val perfTriggerReqs = RegInit(0.U(64.W))
  private val perfFecLineSeen = RegInit(0.U(64.W))
  private val perfLearnEvents = RegInit(0.U(64.W))
  private val perfPreferredLearnEvents = RegInit(0.U(64.W))
  private val perfLookupMisses = RegInit(0.U(64.W))
  private val perfNoConfTarget = RegInit(0.U(64.W))
  private val perfEnqAttempts = RegInit(0.U(64.W))
  private val perfEnqAccepted = RegInit(0.U(64.W))
  private val perfMshrBlockedCycles = RegInit(0.U(64.W))
  private val perfDupDropWithFtq = RegInit(0.U(64.W))
  private val perfSkipNoMetaMatch = RegInit(0.U(64.W))
  private val perfSkipNotHighCost = RegInit(0.U(64.W))
  private val perfSkipProbability = RegInit(0.U(64.W))
  private val perfPrintInterval = 1024.U(64.W)

  when(io.trigger.valid && active) {
    perfTriggerReqs := perfTriggerReqs + 1.U
  }
  when(fecLearnSeen) {
    perfFecLineSeen := perfFecLineSeen + 1.U
  }
  when(learnEligible) {
    perfLearnEvents := perfLearnEvents + 1.U
  }
  when(highPriorityLearn) {
    perfPreferredLearnEvents := perfPreferredLearnEvents + 1.U
  }
  when(fecLearnSeen && !learnedMetaHit) {
    perfSkipNoMetaMatch := perfSkipNoMetaMatch + 1.U
  }
  when(fecLearnSeen && learnedMetaHit && !io.fecLine.bits.highCost) {
    perfSkipNotHighCost := perfSkipNotHighCost + 1.U
  }
  when(fecLearnSeen && !highPriorityLearn && !probabilityPass) {
    perfSkipProbability := perfSkipProbability + 1.U
  }
  when(io.prefetchVaddr.fire) {
    perfTotalPrefetches := perfTotalPrefetches + 1.U
  }
  when(pdipTable.io.lookup.req.valid) {
    perfTableLookups := perfTableLookups + 1.U
  }
  when(pdipTable.io.lookup.resp.valid) {
    perfTableHits := perfTableHits + 1.U
  }
  when(pdipTable.io.lookup.req.valid && !pdipTable.io.lookup.resp.valid) {
    perfLookupMisses := perfLookupMisses + 1.U
  }
  when(targetsValid && !hasExpandedTargets) {
    perfNoConfTarget := perfNoConfTarget + 1.U
  }
  when(prefetchQueue.io.enq.valid) {
    perfEnqAttempts := perfEnqAttempts + 1.U
  }
  when(prefetchQueue.io.enq.fire) {
    perfEnqAccepted := perfEnqAccepted + 1.U
  }
  when(prefetchQueue.io.enq.valid && !prefetchQueue.io.enq.ready) {
    perfQueueFull := perfQueueFull + 1.U
  }
  when(active && !prefetchQueue.io.empty && !io.mshrThresholdMet) {
    perfMshrBlockedCycles := perfMshrBlockedCycles + 1.U
  }
  when(io.dropDupWithFtq) {
    perfDupDropWithFtq := perfDupDropWithFtq + 1.U
  }

  private val hitRate = safePercent(perfTableHits, perfTableLookups)
  private val hitRateBp = safeBasisPoints(perfTableHits, perfTableLookups)
  private val enqAcceptRate = safePercent(perfEnqAccepted, perfEnqAttempts)
  private val enqAcceptRateBp = safeBasisPoints(perfEnqAccepted, perfEnqAttempts)
  private val issueRate = safePercent(perfTotalPrefetches, perfEnqAccepted)
  private val issueRateBp = safeBasisPoints(perfTotalPrefetches, perfEnqAccepted)
  private val learnToIssueRate = safePercent(perfTotalPrefetches, perfLearnEvents)
  private val learnToIssueRateBp = safeBasisPoints(perfTotalPrefetches, perfLearnEvents)

  when(io.flush) {
    printf(p"[PDIP] stats totalPrefetches=${perfTotalPrefetches} tableLookups=${perfTableLookups} tableHits=${perfTableHits} lookupMisses=${perfLookupMisses} hitRate=${hitRate}%(${hitRateBp}bp) queueFull=${perfQueueFull} enqAttempts=${perfEnqAttempts} enqAccepted=${perfEnqAccepted} enqAcceptRate=${enqAcceptRate}%(${enqAcceptRateBp}bp) triggerReqs=${perfTriggerReqs} fecLineSeen=${perfFecLineSeen} learnEvents=${perfLearnEvents} preferredLearnEvents=${perfPreferredLearnEvents} skipNoMetaMatch=${perfSkipNoMetaMatch} skipNotHighCost=${perfSkipNotHighCost} skipProbability=${perfSkipProbability} noConfTarget=${perfNoConfTarget} mshrBlockedCycles=${perfMshrBlockedCycles} dupDropWithFtq=${perfDupDropWithFtq} issueRate=${issueRate}%(${issueRateBp}bp) learnToIssueRate=${learnToIssueRate}%(${learnToIssueRateBp}bp) active=${active}\n")
  }

  private val periodicPrintFire =
    pdipTable.io.lookup.req.valid &&
      (((perfTableLookups + 1.U) & (perfPrintInterval - 1.U)) === 0.U)

  when(periodicPrintFire) {
    printf(p"[PDIP] stats (periodic) totalPrefetches=${perfTotalPrefetches} tableLookups=${perfTableLookups} tableHits=${perfTableHits} lookupMisses=${perfLookupMisses} hitRate=${hitRate}%(${hitRateBp}bp) fecLineSeen=${perfFecLineSeen} learnEvents=${perfLearnEvents} preferredLearnEvents=${perfPreferredLearnEvents} skipNoMetaMatch=${perfSkipNoMetaMatch} skipNotHighCost=${perfSkipNotHighCost} skipProbability=${perfSkipProbability} enqAttempts=${perfEnqAttempts} enqAccepted=${perfEnqAccepted} enqAcceptRate=${enqAcceptRate}%(${enqAcceptRateBp}bp) queueFull=${perfQueueFull} noConfTarget=${perfNoConfTarget} mshrBlockedCycles=${perfMshrBlockedCycles} dupDropWithFtq=${perfDupDropWithFtq}\n")
  }

  io.perfInfo.totalPrefetches := perfTotalPrefetches
  io.perfInfo.tableLookups := perfTableLookups
  io.perfInfo.tableHits := perfTableHits
  io.perfInfo.queueFull := perfQueueFull
}

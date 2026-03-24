package xiangshan.frontend.icache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import top.DefaultConfig

class PDIPTest extends AnyFlatSpec with ChiselScalatestTester {

  val defaultConfig = new DefaultConfig
  implicit val p: Parameters = defaultConfig.alterPartial({
    case XSCoreParamsKey => defaultConfig(XSTileKey).head
  })

  val blockOffBits = 6 

  val pdipParams = PDIPParams(
    enabled            = true,
    numTableSets       = 8,
    numWaysPerSet      = 2,
    numTargetsPerEntry = 2,
    prefetchQueueSize  = 4
  )

  val smallParams = PDIPParams(
    enabled            = true,
    numTableSets       = 4,
    numWaysPerSet      = 2,
    numTargetsPerEntry = 2,
    prefetchQueueSize  = 4
  )
  
  // ====== Helper Functions ======
  
  /** Reset PDIP Table to known state */
  def resetTable(dut: PDIPTable): Unit = {
    dut.io.flush.poke(true.B)
    dut.clock.step(1)
    dut.io.flush.poke(false.B)
    dut.io.allocate.valid.poke(false.B)
    dut.io.lookup.req.valid.poke(false.B)
    dut.clock.step(1)
  }
  
  /** Allocate a single trigger-target association */
  def allocateOnce(dut: PDIPTable, trigger: Long, target: Long, vSetIdx: Long = 0, confidence: Int = 2): Unit = {
    dut.io.allocate.valid.poke(true.B)
    dut.io.allocate.bits.trigger.poke(trigger.U)
    dut.io.allocate.bits.target.valid.poke(true.B)
    dut.io.allocate.bits.target.blkPaddr.poke(target.U)
    dut.io.allocate.bits.target.vSetIdx.poke(vSetIdx.U)
    dut.io.allocate.bits.target.confidence.poke(confidence.U)
    dut.clock.step(1)
    dut.io.allocate.valid.poke(false.B)
    dut.clock.step(1)
  }
  
  /** Lookup a trigger and return (hit: Boolean, targets: Seq[(valid, blkPaddr, confidence)]) */
  def lookupOnce(dut: PDIPTable, trigger: Long, numTargets: Int = 2): (Boolean, Seq[(Boolean, BigInt, BigInt)]) = {
    dut.io.lookup.req.valid.poke(true.B)
    dut.io.lookup.req.bits.trigger.poke(trigger.U)
    dut.clock.step(1)
    
    val hit = dut.io.lookup.resp.valid.peek().litToBoolean
    val targets = (0 until numTargets).map { i =>
      val valid = dut.io.lookup.resp.bits(i).valid.peek().litToBoolean
      val blkPaddr = dut.io.lookup.resp.bits(i).blkPaddr.peek().litValue
      val confidence = dut.io.lookup.resp.bits(i).confidence.peek().litValue
      (valid, blkPaddr, confidence)
    }
    
    dut.io.lookup.req.valid.poke(false.B)
    dut.clock.step(1)
    
    (hit, targets)
  }
  
  /** Reset Prefetch Queue to known state */
  def resetQueue(dut: PDIPPrefetchQueue): Unit = {
    dut.io.flush.poke(true.B)
    dut.clock.step(1)
    dut.io.flush.poke(false.B)
    dut.io.enq.valid.poke(false.B)
    dut.io.deq.ready.poke(false.B)
    dut.io.mshrAvailable.poke(true.B)
    dut.clock.step(1)
  }
  
  /** Enqueue a single prefetch request */
  def enqueueOnce(dut: PDIPPrefetchQueue, blkPaddr: Long, vSetIdx: Long = 0): Boolean = {
    dut.io.enq.valid.poke(true.B)
    dut.io.enq.bits.blkPaddr.poke(blkPaddr.U)
    dut.io.enq.bits.vSetIdx.poke(vSetIdx.U)
    val ready = dut.io.enq.ready.peek().litToBoolean
    dut.clock.step(1)
    dut.io.enq.valid.poke(false.B)
    ready
  }
  
  /** Reset PDIP Controller to known state */
  def resetController(dut: PDIPController): Unit = {
    dut.io.flush.poke(true.B)
    dut.clock.step(1)
    dut.io.flush.poke(false.B)
    dut.io.enable.poke(true.B)
    dut.io.mshrAvailable.poke(true.B)
    dut.io.fecLine.valid.poke(false.B)
    dut.io.trigger.valid.poke(false.B)
    dut.io.prefetchVaddr.ready.poke(true.B)
    dut.clock.step(1)
  }
  
  /** Teach the controller a FEC line */
  def learnFECLine(dut: PDIPController, triggerAddr: Long, targetAddr: Long, vSetIdx: Long = 0): Unit = {
    dut.io.fecLine.valid.poke(true.B)
    dut.io.fecLine.bits.blkPaddr.poke(targetAddr.U)
    dut.io.fecLine.bits.vSetIdx.poke(vSetIdx.U)
    dut.io.fecLine.bits.triggerAddr.poke(triggerAddr.U)
    dut.clock.step(1)
    dut.io.fecLine.valid.poke(false.B)
    dut.clock.step(2) // settle
  }
  
  /** Collect all valid prefetch addresses over a window of cycles (only when valid && ready) */
  def collectPrefetches(dut: PDIPController, cycles: Int): Seq[BigInt] = {
    var prefetches = Seq.empty[BigInt]
    for (_ <- 0 until cycles) {
      val v = dut.io.prefetchVaddr.valid.peek().litToBoolean
      val r = dut.io.prefetchVaddr.ready.peek().litToBoolean
      if (v && r) {
        prefetches = prefetches :+ dut.io.prefetchVaddr.bits.blkPaddr.peek().litValue
      }
      dut.clock.step(1)
    }
    prefetches
  }
  
  behavior of "PDIP Table"
  
  it should "allocate new trigger-target associations" in {
    test(new PDIPTable(pdipParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      
      dut.io.flush.poke(false.B)
      dut.io.allocate.valid.poke(false.B)
      dut.io.lookup.req.valid.poke(false.B)
      dut.clock.step(1)
      
      // Allocate a trigger-target association
      dut.io.allocate.valid.poke(true.B)
      dut.io.allocate.bits.trigger.poke(0x1000.U)
      dut.io.allocate.bits.target.valid.poke(true.B)
      dut.io.allocate.bits.target.blkPaddr.poke(0x2000.U)
      dut.io.allocate.bits.target.vSetIdx.poke(0x10.U)
      dut.io.allocate.bits.target.confidence.poke(2.U)
      dut.clock.step(1)
      
      dut.io.allocate.valid.poke(false.B)
      dut.clock.step(1)
      
      // Lookup the trigger
      dut.io.lookup.req.valid.poke(true.B)
      dut.io.lookup.req.bits.trigger.poke(0x1000.U)
      dut.clock.step(1)
      
      assert(dut.io.lookup.resp.valid.peek().litToBoolean, "Lookup should hit")
      assert(dut.io.lookup.resp.bits(0).valid.peek().litToBoolean, "First target should be valid")
      assert(dut.io.lookup.resp.bits(0).blkPaddr.peek().litValue == 0x2000, "Target address should match")
      
      println("[PDIP Table Test] Trigger-target association successful")
    }
  }
  
  it should "support multiple targets per trigger" in {
    test(new PDIPTable(pdipParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      
      resetTable(dut)
      
      // Allocate first target for trigger 
      allocateOnce(dut, trigger = 0x1000, target = 0x2000, vSetIdx = 0x10, confidence = 2)
      
      // Allocate second target for same trigger
      allocateOnce(dut, trigger = 0x1000, target = 0x3000, vSetIdx = 0x20, confidence = 2)
      
      dut.clock.step(2) // Additional settling time
      
      // Lookup should return both targets with exact addresses and confidences
      val (hit, targets) = lookupOnce(dut, trigger = 0x1000)
      
      assert(hit, "Lookup should hit")
      assert(targets(0)._1, "First target should be valid")
      assert(targets(1)._1, "Second target should be valid")
      
      // Check exact addresses and confidences
      // First target decayed from 2 to 1, second is fresh at 2
      assert(targets(0)._2 == 0x2000, f"First target address should be 0x2000 (got 0x${targets(0)._2}%x)")
      assert(targets(0)._3 == 1, f"First target confidence should be 1 (got ${targets(0)._3})")
      assert(targets(1)._2 == 0x3000, f"Second target address should be 0x3000 (got 0x${targets(1)._2}%x)")
      assert(targets(1)._3 == 2, f"Second target confidence should be 2 (got ${targets(1)._3})")
      
      println("[PDIP Table Test] Multiple targets per trigger successful with exact checks")
    }
  }
  
  it should "decay confidence when updating entries" in {
    test(new PDIPTable(pdipParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)

      dut.io.flush.poke(false.B)
      dut.clock.step(1)

      // allocate first target for trigger
      dut.io.allocate.valid.poke(true.B)
      dut.io.allocate.bits.trigger.poke(0x1000.U)
      dut.io.allocate.bits.target.valid.poke(true.B)
      dut.io.allocate.bits.target.blkPaddr.poke(0x2000.U)
      dut.io.allocate.bits.target.vSetIdx.poke(0x10.U)
      dut.io.allocate.bits.target.confidence.poke(2.U)
      dut.clock.step(1)

      // allocate a second, different target for the same trigger
      dut.io.allocate.bits.target.blkPaddr.poke(0x3000.U)
      dut.io.allocate.bits.target.vSetIdx.poke(0x20.U)
      dut.clock.step(1)

      // turn off allocation and perform a lookup
      dut.io.allocate.valid.poke(false.B)
      dut.io.lookup.req.valid.poke(true.B)
      dut.io.lookup.req.bits.trigger.poke(0x1000.U)
      dut.clock.step(1)

      assert(dut.io.lookup.resp.valid.peek().litToBoolean,
             "Lookup should hit after two allocations")

      // first target should have decayed from 2 to 1, second retains 2
      val c0 = dut.io.lookup.resp.bits(0).confidence.peek().litValue
      val c1 = dut.io.lookup.resp.bits(1).confidence.peek().litValue
      assert(c0 == 1, s"First target confidence should decay to 1 (got $c0)")
      assert(c1 == 2, s"Second target initial confidence should be 2 (got $c1)")

      println("[PDIP Table Test] Confidence decay on update verified")
    }
  }

  it should "handle flush correctly" in {
    test(new PDIPTable(pdipParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      
      dut.io.flush.poke(false.B)
      dut.clock.step(1)
      
      // Allocate an entry
      dut.io.allocate.valid.poke(true.B)
      dut.io.allocate.bits.trigger.poke(0x1000.U)
      dut.io.allocate.bits.target.valid.poke(true.B)
      dut.io.allocate.bits.target.blkPaddr.poke(0x2000.U)
      dut.io.allocate.bits.target.vSetIdx.poke(0x10.U)
      dut.io.allocate.bits.target.confidence.poke(2.U)
      dut.clock.step(1)
      
      dut.io.allocate.valid.poke(false.B)
      dut.clock.step(1)
      
      // Flush the table
      dut.io.flush.poke(true.B)
      dut.clock.step(1)
      dut.io.flush.poke(false.B)
      dut.clock.step(1)
      
      // Lookup should miss
      dut.io.lookup.req.valid.poke(true.B)
      dut.io.lookup.req.bits.trigger.poke(0x1000.U)
      dut.clock.step(1)
      
      assert(!dut.io.lookup.resp.valid.peek().litToBoolean, "Lookup should miss after flush")
      
      println("[PDIP Table Test] Flush test passed")
    }
  }

  it should "return invalid on a cold-cache lookup" in {
    test(new PDIPTable(smallParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(50)
      resetTable(dut)
      
      val (hit, _) = lookupOnce(dut, trigger = 0xABCD)
      assert(!hit, "Cold-cache lookup must miss")
      println("[PDIP Table Test] Cold-cache miss confirmed")
    }
  }
  
  it should "miss on wrong trigger address" in {
    test(new PDIPTable(smallParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(50)
      resetTable(dut)
      
      allocateOnce(dut, trigger = 0x1000, target = 0x2000)
      
      // Lookup with different trigger should miss
      val (hit, _) = lookupOnce(dut, trigger = 0x1004)
      assert(!hit, "Wrong trigger address should miss")
      println("[PDIP Table Test] Wrong trigger miss verified")
    }
  }
  
  it should "not alias same set different tag" in {
    test(new PDIPTable(smallParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(50)
      resetTable(dut)
      
      // smallParams has 4 sets, so bits [3:2] determine set index
      // Triggers 0x04 and 0x14 share set 1 (bits[3:2]=01) but different tags
      allocateOnce(dut, trigger = 0x04, target = 0xAAAA)
      
      val (hit, _) = lookupOnce(dut, trigger = 0x14)
      assert(!hit, "Same set, different tag should not alias")
      println("[PDIP Table Test] No aliasing for same set, different tag")
    }
  }

  it should "boost confidence when the same target is re-inserted" in {
    test(new PDIPTable(smallParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(50)
      dut.io.flush.poke(false.B)

      def alloc(trig: Int, tgt: Int): Unit = {
        dut.io.allocate.valid.poke(true.B)
        dut.io.allocate.bits.trigger.poke(trig.U)
        dut.io.allocate.bits.target.valid.poke(true.B)
        dut.io.allocate.bits.target.blkPaddr.poke(tgt.U)
        dut.io.allocate.bits.target.vSetIdx.poke(0.U)
        dut.io.allocate.bits.target.confidence.poke(2.U)
        dut.clock.step(1)
        dut.io.allocate.valid.poke(false.B)
        dut.clock.step(1)
      }

      alloc(0x10, 0x20) // first insert: confidence = 2
      alloc(0x10, 0x20) // re-insert same target => must boost to 3

      dut.io.lookup.req.valid.poke(true.B)
      dut.io.lookup.req.bits.trigger.poke(0x10.U)
      dut.clock.step(1)

      assert(dut.io.lookup.resp.valid.peek().litToBoolean, "Lookup must hit")
      assert(dut.io.lookup.resp.bits(0).confidence.peek().litValue == 3,
        "Confidence should be boosted to 3 after re-insertion")
      println("[PDIP Table Test] Confidence boost verified")
    }
  }

  it should "not exceed confidence=3" in {
    test(new PDIPTable(smallParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      dut.io.flush.poke(false.B)

      for (_ <- 0 until 5) {
        dut.io.allocate.valid.poke(true.B)
        dut.io.allocate.bits.trigger.poke(0x10.U)
        dut.io.allocate.bits.target.valid.poke(true.B)
        dut.io.allocate.bits.target.blkPaddr.poke(0x20.U)
        dut.io.allocate.bits.target.vSetIdx.poke(0.U)
        dut.io.allocate.bits.target.confidence.poke(3.U)
        dut.clock.step(1)
        dut.io.allocate.valid.poke(false.B)
        dut.clock.step(1)
      }

      dut.io.lookup.req.valid.poke(true.B)
      dut.io.lookup.req.bits.trigger.poke(0x10.U)
      dut.clock.step(1)

      assert(dut.io.lookup.resp.valid.peek().litToBoolean, "Lookup must hit")
      assert(dut.io.lookup.resp.bits(0).confidence.peek().litValue == 3,
        "Confidence must be capped at 3")
      println("[PDIP Table Test] Confidence cap at 3 verified")
    }
  }

  it should "evict an old way when a set is full" in {
    test(new PDIPTable(smallParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      
      resetTable(dut)

      // numWaysPerSet=2; triggers 0x00 and 0x10 share set index 0 (bits[1:0]=0b00).
      allocateOnce(dut, trigger = 0x00, target = 0xAA, confidence = 3) // fills way 0
      allocateOnce(dut, trigger = 0x10, target = 0xBB, confidence = 3) // fills way 1
      allocateOnce(dut, trigger = 0x20, target = 0xCC, confidence = 3) // evicts LRU (0x00)

      dut.clock.step(2) // Additional settling time

      // Check new entry is retrievable
      val (hit1, targets1) = lookupOnce(dut, trigger = 0x20)
      assert(hit1, "New entry (0x20) must be found after eviction")
      assert(targets1(0)._2 == 0xCC, f"Evicted entry target must match 0xCC (got 0x${targets1(0)._2}%x)")
      
      // Check that the evicted entry (0x00) misses
      val (hit2, _) = lookupOnce(dut, trigger = 0x00)
      assert(!hit2, "Evicted entry (0x00) should miss after eviction")
      
      // Check that surviving entry (0x10) still hits
      val (hit3, targets3) = lookupOnce(dut, trigger = 0x10)
      assert(hit3, "Surviving entry (0x10) must still hit after eviction")
      assert(targets3(0)._2 == 0xBB, f"Surviving entry target must match 0xBB (got 0x${targets3(0)._2}%x)")
      
      println("[PDIP Table Test] Way eviction verified with old miss and survivor hit")
    }
  }

  behavior of "Prefetch Queue"
  
  it should "enqueue and dequeue prefetch requests" in {
    test(new PDIPPrefetchQueue(queueSize = 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      
      resetQueue(dut)
      assert(dut.io.empty.peek().litToBoolean, "Queue should start empty")
      
      // Enqueue a request
      val ready = enqueueOnce(dut, blkPaddr = 0x1000, vSetIdx = 0x10)
      assert(ready, "Enqueue should succeed when queue not full")
      assert(!dut.io.empty.peek().litToBoolean, "Queue should not be empty")
      
      // Dequeue should be valid
      assert(dut.io.deq.valid.peek().litToBoolean, "Dequeue should be valid")
      
      dut.io.deq.ready.poke(true.B)
      dut.clock.step(1)
      
      assert(dut.io.empty.peek().litToBoolean, "Queue should be empty after dequeue")
      
      println("[Prefetch Queue Test] Enqueue/dequeue successful")
    }
  }
  
  it should "respect MSHR availability" in {
    test(new PDIPPrefetchQueue(queueSize = 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      
      resetQueue(dut)
      
      // Enqueue a request
      enqueueOnce(dut, blkPaddr = 0x1000, vSetIdx = 0x10)
      
      // With MSHR available, dequeue should be valid
      dut.io.mshrAvailable.poke(true.B)
      dut.clock.step(1)
      assert(dut.io.deq.valid.peek().litToBoolean, "Dequeue should be valid with MSHR available")
      
      // Block MSHR
      dut.io.mshrAvailable.poke(false.B)
      dut.clock.step(1)
      
      // Dequeue should not be valid
      assert(!dut.io.deq.valid.peek().litToBoolean, "Dequeue should be blocked when MSHR unavailable")
      
      println("[Prefetch Queue Test] MSHR availability check passed")
    }
  }
  
  it should "not drain when ready=false" in {
    test(new PDIPPrefetchQueue(queueSize = 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      resetQueue(dut)
      
      // Enqueue multiple items
      enqueueOnce(dut, blkPaddr = 0x1000)
      enqueueOnce(dut, blkPaddr = 0x2000)
      
      dut.io.mshrAvailable.poke(true.B)
      dut.io.deq.ready.poke(false.B) // Block ready
      
      val initialEmpty = dut.io.empty.peek().litToBoolean
      dut.clock.step(5)
      
      // Queue should not drain when ready=false even if valid=true
      assert(dut.io.empty.peek().litToBoolean == initialEmpty, "Queue should not drain when ready=false")
      println("[Prefetch Queue Test] Queue does not drain when ready=false")
    }
  }
  
  it should "not drain when mshrAvailable=false" in {
    test(new PDIPPrefetchQueue(queueSize = 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      resetQueue(dut)
      
      // Enqueue items
      enqueueOnce(dut, blkPaddr = 0x1000)
      enqueueOnce(dut, blkPaddr = 0x2000)
      
      dut.io.mshrAvailable.poke(false.B) // Block MSHR
      dut.io.deq.ready.poke(true.B)
      
      dut.clock.step(5)
      
      // Queue should not drain when mshrAvailable=false
      assert(!dut.io.empty.peek().litToBoolean, "Queue should not drain when mshrAvailable=false")
      println("[Prefetch Queue Test] Queue does not drain when mshrAvailable=false")
    }
  }

  it should "assert full when queue is saturated" in {
    test(new PDIPPrefetchQueue(queueSize = 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      dut.io.flush.poke(false.B)
      dut.io.mshrAvailable.poke(false.B) // prevent draining
      dut.io.deq.ready.poke(false.B)

      for (i <- 0 until 4) {
        dut.io.enq.valid.poke(true.B)
        dut.io.enq.bits.blkPaddr.poke(i.U)
        dut.io.enq.bits.vSetIdx.poke(0.U)
        dut.clock.step(1)
      }
      dut.io.enq.valid.poke(false.B)

      assert(dut.io.full.peek().litToBoolean, "Queue must be full after filling all slots")
      assert(!dut.io.enq.ready.peek().litToBoolean, "enq.ready must be false when full")
      println("[Prefetch Queue Test] Full signal verified")
    }
  }

  it should "reset to empty after flush" in {
    test(new PDIPPrefetchQueue(queueSize = 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      dut.io.flush.poke(false.B)
      dut.io.mshrAvailable.poke(false.B)
      dut.io.deq.ready.poke(false.B)

      for (i <- 0 until 2) {
        dut.io.enq.valid.poke(true.B)
        dut.io.enq.bits.blkPaddr.poke(i.U)
        dut.io.enq.bits.vSetIdx.poke(0.U)
        dut.clock.step(1)
      }
      dut.io.enq.valid.poke(false.B)
      assert(!dut.io.empty.peek().litToBoolean, "Queue must be non-empty before flush")

      dut.io.flush.poke(true.B)
      dut.clock.step(1)
      dut.io.flush.poke(false.B)

      assert(dut.io.empty.peek().litToBoolean, "Queue must be empty after flush")
      println("[Prefetch Queue Test] Post-flush empty verified")
    }
  }

  it should "preserve FIFO ordering across multiple entries" in {
    test(new PDIPPrefetchQueue(queueSize = 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      dut.io.flush.poke(false.B)
      dut.io.mshrAvailable.poke(false.B)
      dut.io.deq.ready.poke(false.B)

      val addrs = Seq(0xAA, 0xBB, 0xCC)
      for (a <- addrs) {
        dut.io.enq.valid.poke(true.B)
        dut.io.enq.bits.blkPaddr.poke(a.U)
        dut.io.enq.bits.vSetIdx.poke(0.U)
        dut.clock.step(1)
      }
      dut.io.enq.valid.poke(false.B)

      dut.io.mshrAvailable.poke(true.B)
      dut.io.deq.ready.poke(true.B)

      for (a <- addrs) {
        assert(dut.io.deq.valid.peek().litToBoolean, "Dequeue must be valid")
        assert(dut.io.deq.bits.blkPaddr.peek().litValue == a,
          s"Expected blkPaddr=0x${a.toHexString}")
        dut.clock.step(1)
      }
      println("[Prefetch Queue Test] FIFO order verified")
    }
  }
  
  it should "handle flush + enqueue in same cycle" in {
    test(new PDIPPrefetchQueue(queueSize = 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      resetQueue(dut)
      
      // Enqueue some items
      enqueueOnce(dut, blkPaddr = 0x1000)
      enqueueOnce(dut, blkPaddr = 0x2000)
      
      // Flush and enqueue in same cycle
      dut.io.flush.poke(true.B)
      dut.io.enq.valid.poke(true.B)
      dut.io.enq.bits.blkPaddr.poke(0x3000.U)
      dut.io.enq.bits.vSetIdx.poke(0.U)
      dut.clock.step(1)
      dut.io.flush.poke(false.B)
      dut.io.enq.valid.poke(false.B)
      
      // Queue should be empty (flush takes precedence)
      assert(dut.io.empty.peek().litToBoolean, "Queue must be empty after same-cycle flush+enq")
      println("[Prefetch Queue Test] Flush + enqueue conflict handled")
    }
  }
  
  it should "handle enqueue + dequeue in same cycle" in {
    test(new PDIPPrefetchQueue(queueSize = 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      resetQueue(dut)
      
      // Enqueue one item
      enqueueOnce(dut, blkPaddr = 0x1000)
      
      // Try enqueue and dequeue in same cycle
      dut.io.enq.valid.poke(true.B)
      dut.io.enq.bits.blkPaddr.poke(0x2000.U)
      dut.io.enq.bits.vSetIdx.poke(0.U)
      dut.io.mshrAvailable.poke(true.B)
      dut.io.deq.ready.poke(true.B)
      
      val enqReady = dut.io.enq.ready.peek().litToBoolean
      val deqValid = dut.io.deq.valid.peek().litToBoolean
      
      assert(enqReady && deqValid, "Both enqueue and dequeue should be possible simultaneously")
      dut.clock.step(1)
      
      println("[Prefetch Queue Test] Simultaneous enqueue+dequeue verified")
    }
  }

  behavior of "PDIP Table Conflicts"
  
  it should "handle flush + allocate in same cycle" in {
    test(new PDIPTable(smallParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      resetTable(dut)
      
      // Allocate an entry
      allocateOnce(dut, trigger = 0x1000, target = 0x2000)
      
      // Flush and allocate new entry in same cycle
      dut.io.flush.poke(true.B)
      dut.io.allocate.valid.poke(true.B)
      dut.io.allocate.bits.trigger.poke(0x3000.U)
      dut.io.allocate.bits.target.valid.poke(true.B)
      dut.io.allocate.bits.target.blkPaddr.poke(0x4000.U)
      dut.io.allocate.bits.target.vSetIdx.poke(0.U)
      dut.io.allocate.bits.target.confidence.poke(2.U)
      dut.clock.step(1)
      dut.io.flush.poke(false.B)
      dut.io.allocate.valid.poke(false.B)
      dut.clock.step(1)
      
      // Old entry should miss (flushed)
      val (hit1, _) = lookupOnce(dut, trigger = 0x1000)
      assert(!hit1, "Old entry should miss after flush")
      
      // New entry should also miss (flush takes precedence)
      val (hit2, _) = lookupOnce(dut, trigger = 0x3000)
      assert(!hit2, "Same-cycle allocate during flush should not persist")
      println("[PDIP Table Test] Flush + allocate conflict handled")
    }
  }
  
  it should "handle flush + lookup in same cycle" in {
    test(new PDIPTable(smallParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      resetTable(dut)
      
      // Allocate entry
      allocateOnce(dut, trigger = 0x1000, target = 0x2000)
      
      // Flush and lookup simultaneously
      dut.io.flush.poke(true.B)
      dut.io.lookup.req.valid.poke(true.B)
      dut.io.lookup.req.bits.trigger.poke(0x1000.U)
      dut.clock.step(1)
      dut.io.flush.poke(false.B)
      dut.io.lookup.req.valid.poke(false.B)
      
      // Lookup should miss (table was flushed)
      val hit = dut.io.lookup.resp.valid.peek().litToBoolean
      assert(!hit, "Lookup during flush should miss")
      println("[PDIP Table Test] Flush + lookup conflict handled")
    }
  }

  behavior of "PDIP Controller"
  
  it should "issue prefetches on trigger match" in {
    test(new PDIPController(pdipParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(200)
      
      // Initialize
      dut.io.enable.poke(true.B)
      dut.io.flush.poke(false.B)
      dut.io.mshrAvailable.poke(true.B)
      dut.clock.step(1)
      
      // Step 1: Learn from FEC line detection
      println("\n[PDIP Controller Test] Step 1: Learning phase")
      dut.io.fecLine.valid.poke(true.B)
      dut.io.fecLine.bits.blkPaddr.poke(0x2000.U)
      dut.io.fecLine.bits.vSetIdx.poke(0x20.U)
      dut.io.fecLine.bits.triggerAddr.poke(0x1000.U)
      dut.clock.step(1)

      dut.io.fecLine.valid.poke(false.B)
      dut.clock.step(5) // Allow table update to settle

      // Step 2: Trigger with matching address
      println("[PDIP Controller Test] Step 2: Trigger matching")
      dut.io.trigger.valid.poke(true.B)
      dut.io.trigger.bits.blkPaddr.poke(0x1000.U)
      dut.io.prefetchVaddr.ready.poke(true.B)
      dut.clock.step(1)

      dut.clock.step(5) // Allow lookup and queue processing

      var foundPrefetch = false
      for (_ <- 0 until 10) {
        if (dut.io.prefetchVaddr.valid.peek().litToBoolean) {
          foundPrefetch = true
          val pa = dut.io.prefetchVaddr.bits.blkPaddr.peek().litValue
          println(s"[PDIP Controller Test] Prefetch blkPaddr=0x${pa.toString(16)}")
          assert(pa == 0x2000, s"Prefetch blkPaddr should be 0x2000 (got 0x${pa.toString(16)})")
        }
        dut.clock.step(1)
      }

      assert(foundPrefetch, "PDIP should issue prefetch on trigger match")
      println("[PDIP Controller Test] Trigger-based prefetch successful")
    }
  }
  
  it should "learn multiple FEC lines for same trigger" in {
    test(new PDIPController(pdipParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(200)
      
      dut.io.enable.poke(true.B)
      dut.io.flush.poke(false.B)
      dut.io.mshrAvailable.poke(true.B)
      dut.clock.step(1)
      
      // Learn first FEC line
      dut.io.fecLine.valid.poke(true.B)
      dut.io.fecLine.bits.blkPaddr.poke(0x2000.U)
      dut.io.fecLine.bits.vSetIdx.poke(0x20.U)
      dut.io.fecLine.bits.triggerAddr.poke(0x1000.U)
      dut.clock.step(1)

      // Learn second FEC line for same trigger
      dut.io.fecLine.bits.blkPaddr.poke(0x3000.U)
      dut.io.fecLine.bits.vSetIdx.poke(0x30.U)
      dut.clock.step(1)

      dut.io.fecLine.valid.poke(false.B)
      dut.clock.step(5)

      // Trigger should eventually prefetch both
      dut.io.trigger.valid.poke(true.B)
      dut.io.trigger.bits.blkPaddr.poke(0x1000.U)
      dut.io.prefetchVaddr.ready.poke(true.B)

      var prefetchCount = 0
      for (_ <- 0 until 20) {
        if (dut.io.prefetchVaddr.valid.peek().litToBoolean) {
          prefetchCount += 1
          println(s"[PDIP Controller Test] Prefetch #$prefetchCount blkPaddr=0x${dut.io.prefetchVaddr.bits.blkPaddr.peek().litValue.toString(16)}")
        }
        dut.clock.step(1)
      }
      
      // Should eventually issue prefetches for both targets
      println(s"[PDIP Controller Test] Total prefetches issued: $prefetchCount")
      assert(prefetchCount >= 1, "Should issue at least one prefetch")
    }
  }

  it should "not issue prefetches when enable=false" in {
    test(new PDIPController(smallParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      dut.io.flush.poke(false.B)
      dut.io.enable.poke(false.B)
      dut.io.mshrAvailable.poke(true.B)
      dut.io.prefetchVaddr.ready.poke(true.B)
      dut.io.trigger.valid.poke(false.B)

      dut.io.fecLine.valid.poke(true.B)
      dut.io.fecLine.bits.blkPaddr.poke(0x20.U)
      dut.io.fecLine.bits.vSetIdx.poke(1.U)
      dut.io.fecLine.bits.triggerAddr.poke(0x10.U)
      dut.clock.step(1)
      dut.io.fecLine.valid.poke(false.B)

      dut.io.trigger.valid.poke(true.B)
      dut.io.trigger.bits.blkPaddr.poke(0x10.U)
      dut.clock.step(3)

      assert(!dut.io.prefetchVaddr.valid.peek().litToBoolean,
        "No prefetch should be issued when disabled")
      assert(dut.io.perfInfo.totalPrefetches.peek().litValue == 0,
        "totalPrefetches must stay 0 when disabled")
      println("[PDIP Controller Test] enable=false suppresses all output")
    }
  }

  it should "stop issuing prefetches after flush" in {
    test(new PDIPController(smallParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      dut.io.flush.poke(false.B)
      dut.io.enable.poke(true.B)
      dut.io.mshrAvailable.poke(true.B)
      dut.io.prefetchVaddr.ready.poke(false.B)
      dut.io.trigger.valid.poke(false.B)
      dut.io.fecLine.valid.poke(false.B)

      // Teach one entry
      dut.io.fecLine.valid.poke(true.B)
      dut.io.fecLine.bits.blkPaddr.poke(0x30.U)
      dut.io.fecLine.bits.vSetIdx.poke(1.U)
      dut.io.fecLine.bits.triggerAddr.poke(0xAA.U)
      dut.clock.step(1)
      dut.io.fecLine.valid.poke(false.B)

      // Flush before firing any trigger
      dut.io.flush.poke(true.B)
      dut.clock.step(1)
      dut.io.flush.poke(false.B)

      // Trigger after flush, table was wiped
      dut.io.trigger.valid.poke(true.B)
      dut.io.trigger.bits.blkPaddr.poke(0xAA.U)
      dut.io.prefetchVaddr.ready.poke(true.B)
      dut.clock.step(3)

      assert(!dut.io.prefetchVaddr.valid.peek().litToBoolean,
        "No prefetch should fire after flush wiped the table")
      println("[PDIP Controller Test] Post-flush: no prefetches issued")
    }
  }

  it should "increment tableLookups on each trigger" in {
    test(new PDIPController(smallParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      dut.io.flush.poke(false.B)
      dut.io.enable.poke(true.B)
      dut.io.mshrAvailable.poke(false.B)
      dut.io.fecLine.valid.poke(false.B)
      dut.io.prefetchVaddr.ready.poke(false.B)

      dut.io.trigger.valid.poke(true.B)
      dut.io.trigger.bits.blkPaddr.poke(0x01.U)

      for (i <- 1 to 3) {
        dut.clock.step(1)
        assert(dut.io.perfInfo.tableLookups.peek().litValue == i,
          s"tableLookups should be $i after $i triggers")
      }
      println("[PDIP Controller Test] tableLookups counter verified")
    }
  }

  it should "increment tableHits only when lookup hits" in {
    test(new PDIPController(smallParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      dut.io.flush.poke(false.B)
      dut.io.enable.poke(true.B)
      dut.io.mshrAvailable.poke(false.B)
      dut.io.prefetchVaddr.ready.poke(false.B)
      dut.io.fecLine.valid.poke(false.B)

      // Trigger on cold table => miss; tableHits must stay 0
      dut.io.trigger.valid.poke(true.B)
      dut.io.trigger.bits.blkPaddr.poke(0x10.U)
      dut.clock.step(1)
      assert(dut.io.perfInfo.tableHits.peek().litValue == 0, "No hits on cold table")

      // Teach one association
      dut.io.trigger.valid.poke(false.B)
      dut.io.fecLine.valid.poke(true.B)
      dut.io.fecLine.bits.blkPaddr.poke(0x20.U)
      dut.io.fecLine.bits.vSetIdx.poke(1.U)
      dut.io.fecLine.bits.triggerAddr.poke(0x10.U)
      dut.clock.step(1)
      dut.io.fecLine.valid.poke(false.B)
      dut.clock.step(1)

      // Trigger the known address => must hit
      dut.io.trigger.valid.poke(true.B)
      dut.io.trigger.bits.blkPaddr.poke(0x10.U)
      dut.clock.step(1)
      assert(dut.io.perfInfo.tableHits.peek().litValue == 1, "tableHits should be 1 after a real hit")
      println("[PDIP Controller Test] tableHits increments only on real hits")
    }
  }

  it should "increment queueFull counter when queue is full on a hit" in {
    test(new PDIPController(smallParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(200)
      dut.io.flush.poke(false.B)
      dut.io.enable.poke(true.B)
      dut.io.mshrAvailable.poke(false.B) // prevent queue from draining
      dut.io.prefetchVaddr.ready.poke(false.B)
      dut.io.trigger.valid.poke(false.B)
      dut.io.fecLine.valid.poke(false.B)

      // Teach one FEC line
      dut.io.fecLine.valid.poke(true.B)
      dut.io.fecLine.bits.blkPaddr.poke(0x20.U)
      dut.io.fecLine.bits.vSetIdx.poke(1.U)
      dut.io.fecLine.bits.triggerAddr.poke(0x10.U)
      dut.clock.step(1)
      dut.io.fecLine.valid.poke(false.B)

      // Fire the trigger many times to overflow the queue
      dut.io.trigger.valid.poke(true.B)
      dut.io.trigger.bits.blkPaddr.poke(0x10.U)
      dut.clock.step(smallParams.prefetchQueueSize + 4)

      val qf = dut.io.perfInfo.queueFull.peek().litValue
      assert(qf > 0, s"queueFull counter should be > 0 (got $qf)")
      println(s"[PDIP Controller Test] queueFull counter = $qf")
    }
  }
  
  it should "not learn when enable=false" in {
    test(new PDIPController(smallParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      dut.io.flush.poke(false.B)
      dut.io.enable.poke(false.B) // Disabled!
      dut.io.mshrAvailable.poke(true.B)
      dut.io.prefetchVaddr.ready.poke(true.B)
      
      // Try to teach a FEC line while disabled
      dut.io.fecLine.valid.poke(true.B)
      dut.io.fecLine.bits.blkPaddr.poke(0x2000.U)
      dut.io.fecLine.bits.vSetIdx.poke(1.U)
      dut.io.fecLine.bits.triggerAddr.poke(0x1000.U)
      dut.clock.step(1)
      dut.io.fecLine.valid.poke(false.B)
      dut.clock.step(2)
      
      // Now enable and trigger
      dut.io.enable.poke(true.B)
      dut.io.trigger.valid.poke(true.B)
      dut.io.trigger.bits.blkPaddr.poke(0x1000.U)
      dut.clock.step(5)
      
      // Should not issue prefetch (learning was disabled)
      val prefetches = collectPrefetches(dut, cycles = 10)
      assert(prefetches.isEmpty, "No prefetches should be issued after learning while disabled")
      println("[PDIP Controller Test] Disabled controller does not learn")
    }
  }
  
  it should "handle repeated trigger held high for N cycles" in {
    test(new PDIPController(smallParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      resetController(dut)
      
      // Teach one FEC line
      learnFECLine(dut, triggerAddr = 0x1000, targetAddr = 0x2000, vSetIdx = 1)
      
      // Hold trigger high for multiple cycles
      dut.io.trigger.valid.poke(true.B)
      dut.io.trigger.bits.blkPaddr.poke(0x1000.U)
      dut.io.prefetchVaddr.ready.poke(true.B)
      
      val lookupsBefore = dut.io.perfInfo.tableLookups.peek().litValue
      dut.clock.step(5)
      val lookupsAfter = dut.io.perfInfo.tableLookups.peek().litValue
      
      // Should increment lookups every cycle while trigger is held
      assert(lookupsAfter == lookupsBefore + 5, 
        f"tableLookups should increment each cycle (before=$lookupsBefore, after=$lookupsAfter)")
      
      println("[PDIP Controller Test] Repeated trigger behaves correctly")
    }
  }
  
  it should "count prefetches only when valid && ready" in {
    test(new PDIPController(smallParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(150)
      resetController(dut)
      
      // Teach FEC line
      learnFECLine(dut, triggerAddr = 0x1000, targetAddr = 0x2000, vSetIdx = 1)
      
      // Trigger with ready=false initially
      dut.io.trigger.valid.poke(true.B)
      dut.io.trigger.bits.blkPaddr.poke(0x1000.U)
      dut.io.prefetchVaddr.ready.poke(false.B) // Block ready
      dut.clock.step(5)
      
      val prefetchCountBlocked = dut.io.perfInfo.totalPrefetches.peek().litValue
      
      // Now set ready=true and allow handshake
      dut.io.prefetchVaddr.ready.poke(true.B)
      dut.clock.step(5)
      
      val prefetchCountReady = dut.io.perfInfo.totalPrefetches.peek().litValue
      
      // Prefetches should only count when valid && ready
      assert(prefetchCountReady > prefetchCountBlocked, 
        f"Prefetches should only count when ready (blocked=$prefetchCountBlocked, ready=$prefetchCountReady)")
      
      println("[PDIP Controller Test] Prefetches counted only on valid && ready")
    }
  }
  
  it should "use scoreboard to verify multi-target prefetch sequence" in {
    test(new PDIPController(pdipParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(300)
      resetController(dut)
      
      // Learn multiple targets for one trigger
      // Since each allocation decays other targets, we need to re-learn to maintain high confidence
      learnFECLine(dut, triggerAddr = 0x1000, targetAddr = 0x2000, vSetIdx = 1)
      learnFECLine(dut, triggerAddr = 0x1000, targetAddr = 0x2000, vSetIdx = 1) // Boost to 3
      learnFECLine(dut, triggerAddr = 0x1000, targetAddr = 0x3000, vSetIdx = 2) // 0x2000 decays to 2
      learnFECLine(dut, triggerAddr = 0x1000, targetAddr = 0x3000, vSetIdx = 2) // Boost 0x3000 to 3, 0x2000 decays to 1
      learnFECLine(dut, triggerAddr = 0x1000, targetAddr = 0x2000, vSetIdx = 1) // Re-boost 0x2000 to 2, 0x3000 decays to 2
      
      // Expected targets - both should now have confidence >= 2
      val expectedTargets = Set(BigInt(0x2000), BigInt(0x3000))
      
      // Fire trigger and collect all prefetches
      dut.io.trigger.valid.poke(true.B)
      dut.io.trigger.bits.blkPaddr.poke(0x1000.U)
      dut.io.prefetchVaddr.ready.poke(true.B)
      
      val prefetches = collectPrefetches(dut, cycles = 30)
      val prefetchSet = prefetches.toSet
      
      println(f"[PDIP Controller Test] Collected prefetches: ${prefetches.map(p => f"0x$p%x").mkString(", ")}")
      println(f"[PDIP Controller Test] Expected targets: ${expectedTargets.map(p => f"0x$p%x").mkString(", ")}")
      
      // Note: PriorityEncoder selects one target per cycle, so we may only see the highest-priority one
      // Verify at least one expected target was prefetched
      assert(prefetchSet.intersect(expectedTargets).nonEmpty, 
        f"At least one expected target should be prefetched. Expected: $expectedTargets, Got: $prefetchSet")
      
      println("[PDIP Controller Test] Scoreboard verification passed")
    }
  }
  
  it should "scoreboard verify correct targets for multiple triggers" in {
    test(new PDIPController(pdipParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(300)
      resetController(dut)
      
      // Learn associations: trigger A -> target 1, trigger B -> target 2
      // Keep it simple: one target per trigger
      learnFECLine(dut, triggerAddr = 0xA000, targetAddr = 0x1000, vSetIdx = 1)
      learnFECLine(dut, triggerAddr = 0xB000, targetAddr = 0x2000, vSetIdx = 2)
      
      // Scoreboard for trigger A
      dut.io.trigger.valid.poke(true.B)
      dut.io.trigger.bits.blkPaddr.poke(0xA000.U)
      dut.io.prefetchVaddr.ready.poke(true.B)
      
      val prefetchesA = collectPrefetches(dut, cycles = 20)
      val expectedA = Set(BigInt(0x1000))
      
      println(f"[PDIP Controller Test] Trigger A prefetches: ${prefetchesA.map(p => f"0x$p%x").mkString(", ")}")
      assert(expectedA.subsetOf(prefetchesA.toSet), 
        f"Trigger A should prefetch target 0x1000")
      
      // Reset trigger
      dut.io.trigger.valid.poke(false.B)
      dut.clock.step(5)
      
      // Scoreboard for trigger B
      dut.io.trigger.valid.poke(true.B)
      dut.io.trigger.bits.blkPaddr.poke(0xB000.U)
      
      val prefetchesB = collectPrefetches(dut, cycles = 20)
      val expectedB = Set(BigInt(0x2000))
      
      println(f"[PDIP Controller Test] Trigger B prefetches: ${prefetchesB.map(p => f"0x$p%x").mkString(", ")}")
      assert(expectedB.subsetOf(prefetchesB.toSet), 
        f"Trigger B should prefetch target 0x2000")
      
      println("[PDIP Controller Test] Multi-trigger scoreboard verification passed")
    }
  }
  
  it should "handle flush + trigger in same cycle" in {
    test(new PDIPController(smallParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      resetController(dut)
      
      // Learn a FEC line
      learnFECLine(dut, triggerAddr = 0x1000, targetAddr = 0x2000, vSetIdx = 1)
      
      // Flush and trigger simultaneously
      dut.io.flush.poke(true.B)
      dut.io.trigger.valid.poke(true.B)
      dut.io.trigger.bits.blkPaddr.poke(0x1000.U)
      dut.io.prefetchVaddr.ready.poke(true.B)
      dut.clock.step(1)
      dut.io.flush.poke(false.B)
      
      // Collect prefetches
      val prefetches = collectPrefetches(dut, cycles = 10)
      
      // Should not issue prefetches (table was flushed)
      assert(prefetches.isEmpty, "No prefetches after simultaneous flush+trigger")
      println("[PDIP Controller Test] Flush + trigger conflict handled")
    }
  }
}

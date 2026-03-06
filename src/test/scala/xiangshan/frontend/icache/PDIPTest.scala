/***************************************************************************************
* PDIP (Prefetch-Directed Instruction Prefetching) Test Suite
*
* Tests the PDIP prefetching mechanism that learns trigger-candidate associations
* from FEC line detections and issues targeted prefetches.
***************************************************************************************/

package xiangshan.frontend.icache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import top.DefaultConfig

class PDIPTest extends AnyFlatSpec with ChiselScalatestTester {

  // Use XiangShan's default configuration
  val defaultConfig = new DefaultConfig
  implicit val p: Parameters = defaultConfig.alterPartial({
    case XSCoreParamsKey => defaultConfig(XSTileKey).head
  })

  val blockOffBits = 6 // log2(64) for 64-byte blocks

  // Standard params used by most tests
  val pdipParams = PDIPParams(
    enabled            = true,
    numTableSets       = 8,
    numWaysPerSet      = 2,
    numTargetsPerEntry = 2,
    prefetchQueueSize  = 4
  )

  // Compact params for structural / edge-case tests (fewer sets and ways → faster sim)
  val smallParams = PDIPParams(
    enabled            = true,
    numTableSets       = 4,
    numWaysPerSet      = 2,
    numTargetsPerEntry = 2,
    prefetchQueueSize  = 4
  )
  
  behavior of "PDIP Table"
  
  it should "allocate new trigger-target associations" in {
    test(new PDIPTable(pdipParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      
      // Initialize
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
      
      // Check response
      assert(dut.io.lookup.resp.valid.peek().litToBoolean, "Lookup should hit")
      assert(dut.io.lookup.resp.bits(0).valid.peek().litToBoolean, "First target should be valid")
      assert(dut.io.lookup.resp.bits(0).blkPaddr.peek().litValue == 0x2000, "Target address should match")
      
      println("[PDIP Table Test] Trigger-target association successful")
    }
  }
  
  it should "support multiple targets per trigger" in {
    test(new PDIPTable(pdipParams)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      
      dut.io.flush.poke(false.B)
      dut.clock.step(1)
      
      // Allocate first target for trigger 0x1000
      dut.io.allocate.valid.poke(true.B)
      dut.io.allocate.bits.trigger.poke(0x1000.U)
      dut.io.allocate.bits.target.valid.poke(true.B)
      dut.io.allocate.bits.target.blkPaddr.poke(0x2000.U)
      dut.io.allocate.bits.target.vSetIdx.poke(0x10.U)
      dut.io.allocate.bits.target.confidence.poke(2.U)
      dut.clock.step(1)
      
      // Allocate second target for same trigger
      dut.io.allocate.bits.target.blkPaddr.poke(0x3000.U)
      dut.io.allocate.bits.target.vSetIdx.poke(0x20.U)
      dut.clock.step(1)
      
      dut.io.allocate.valid.poke(false.B)
      dut.clock.step(1)
      
      // Lookup should return both targets
      dut.io.lookup.req.valid.poke(true.B)
      dut.io.lookup.req.bits.trigger.poke(0x1000.U)
      dut.clock.step(1)
      
      assert(dut.io.lookup.resp.valid.peek().litToBoolean, "Lookup should hit")
      assert(dut.io.lookup.resp.bits(0).valid.peek().litToBoolean, "First target valid")
      assert(dut.io.lookup.resp.bits(1).valid.peek().litToBoolean, "Second target valid")
      
      println("[PDIP Table Test] Multiple targets per trigger successful")
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
      dut.io.flush.poke(false.B)
      dut.io.allocate.valid.poke(false.B)
      dut.io.lookup.req.valid.poke(true.B)
      dut.io.lookup.req.bits.trigger.poke(0xABCD.U)
      dut.clock.step(1)
      assert(!dut.io.lookup.resp.valid.peek().litToBoolean, "Cold-cache lookup must miss")
      println("[PDIP Table Test] Cold-cache miss confirmed")
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
      alloc(0x10, 0x20) // re-insert same target → must boost to 3

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
      dut.io.flush.poke(false.B)

      def alloc(trig: Int, tgt: Int): Unit = {
        dut.io.allocate.valid.poke(true.B)
        dut.io.allocate.bits.trigger.poke(trig.U)
        dut.io.allocate.bits.target.valid.poke(true.B)
        dut.io.allocate.bits.target.blkPaddr.poke(tgt.U)
        dut.io.allocate.bits.target.vSetIdx.poke(0.U)
        dut.io.allocate.bits.target.confidence.poke(3.U)
        dut.clock.step(1)
        dut.io.allocate.valid.poke(false.B)
      }

      // numWaysPerSet=2; triggers 0x00 and 0x10 share set index 0 (bits[1:0]=0b00).
      alloc(0x00, 0xAA) // fills way 0
      alloc(0x10, 0xBB) // fills way 1
      alloc(0x20, 0xCC) // evicts LRU; new entry must be retrievable

      dut.io.lookup.req.valid.poke(true.B)
      dut.io.lookup.req.bits.trigger.poke(0x20.U)
      dut.clock.step(1)

      assert(dut.io.lookup.resp.valid.peek().litToBoolean, "New entry must be found after eviction")
      assert(dut.io.lookup.resp.bits(0).blkPaddr.peek().litValue == 0xCC,
        "Evicted entry target must match the most recently inserted one")
      println("[PDIP Table Test] Way eviction verified")
    }
  }

  // ─────────────────────────────────────────────────────────────────────────
  behavior of "Prefetch Queue"
  
  it should "enqueue and dequeue prefetch requests" in {
    test(new PrefetchQueue(queueSize = 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      
      dut.io.flush.poke(false.B)
      dut.io.mshrAvailable.poke(true.B)
      dut.clock.step(1)
      
      assert(dut.io.empty.peek().litToBoolean, "Queue should start empty")
      
      // Enqueue a request
      dut.io.enq.valid.poke(true.B)
      dut.io.enq.bits.vSetIdx.poke(0x10.U)
      dut.clock.step(1)
      
      dut.io.enq.valid.poke(false.B)
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
    test(new PrefetchQueue(queueSize = 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(100)
      
      dut.io.flush.poke(false.B)
      dut.clock.step(1)
      
      // Enqueue a request
      dut.io.enq.valid.poke(true.B)
      dut.io.enq.bits.vSetIdx.poke(0x10.U)
      dut.io.mshrAvailable.poke(true.B)
      dut.clock.step(1)
      
      dut.io.enq.valid.poke(false.B)
      
      // With MSHR available, dequeue should be valid
      assert(dut.io.deq.valid.peek().litToBoolean, "Dequeue should be valid with MSHR available")
      
      // Block MSHR
      dut.io.mshrAvailable.poke(false.B)
      dut.clock.step(1)
      
      // Dequeue should not be valid
      assert(!dut.io.deq.valid.peek().litToBoolean, "Dequeue should be blocked when MSHR unavailable")
      
      println("[Prefetch Queue Test] MSHR availability check passed")
    }
  }

  it should "assert full when queue is saturated" in {
    test(new PrefetchQueue(queueSize = 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
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
    test(new PrefetchQueue(queueSize = 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
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
    test(new PrefetchQueue(queueSize = 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
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

  // ─────────────────────────────────────────────────────────────────────────
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

      // Trigger after flush – table was wiped
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

      // Trigger on cold table → miss; tableHits must stay 0
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

      // Trigger the known address → must hit
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
}

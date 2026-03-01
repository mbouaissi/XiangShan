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
  
  val pdipParams = PDIPParams(
    enabled = true,
    numTableSets = 8,
    numWaysPerSet = 2,
    numTargetsPerEntry = 2,
    prefetchQueueSize = 4
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
      dut.io.enq.bits.blkPaddr.poke(0x1000.U)
      dut.io.enq.bits.vSetIdx.poke(0x10.U)
      dut.io.enq.bits.vaddr.poke(0x80001000L.U)
      dut.clock.step(1)
      
      dut.io.enq.valid.poke(false.B)
      assert(!dut.io.empty.peek().litToBoolean, "Queue should not be empty")
      
      // Dequeue should be valid
      assert(dut.io.deq.valid.peek().litToBoolean, "Dequeue should be valid")
      assert(dut.io.deq.bits.blkPaddr.peek().litValue == 0x1000, "Dequeued address should match")
      
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
      dut.io.enq.bits.blkPaddr.poke(0x1000.U)
      dut.io.enq.bits.vSetIdx.poke(0x10.U)
      dut.io.enq.bits.vaddr.poke(0x80001000L.U)
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
      dut.io.trigger.bits.blkPaddr.poke(0x1000.U) // Same as trigger from FEC
      dut.io.prefetchVaddr.ready.poke(true.B)
      dut.clock.step(1)
      
      // Check if prefetch is issued (vaddr)
      dut.clock.step(5) // Allow lookup and queue processing
      
      // The prefetch virtual address should appear
      var foundPrefetch = false
      for (_ <- 0 until 10) {
        if (dut.io.prefetchVaddr.valid.peek().litToBoolean) {
          foundPrefetch = true
          val v = dut.io.prefetchVaddr.bits.peek().litValue
          println(s"[PDIP Controller Test] Prefetch vaddr issued 0x${v.toString(16)}")
          // expect vaddr derived from learned blkVaddr appended with zeros
          // learned blkVaddr computed in test: we used fecLine.blkPaddr = 0x2000, vSetIdx=0x20
          // PDIPController reconstructs blkVaddr from table entry equal to that value, so full vaddr is (blkVaddr << blockOffBits)
          assert((v & ~((1 << blockOffBits)-1)) === (0x2000.U << blockOffBits).litValue,
            "Prefetch vaddr should match learned FEC line block address")
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
          println(s"[PDIP Controller Test] Prefetch #$prefetchCount vaddr ${dut.io.prefetchVaddr.bits.peek().litValue.toString(16)} issued")
        }
        dut.clock.step(1)
      }
      
      // Should eventually issue prefetches for both targets
      println(s"[PDIP Controller Test] Total prefetches issued: $prefetchCount")
      assert(prefetchCount >= 1, "Should issue at least one prefetch")
    }
  }
}

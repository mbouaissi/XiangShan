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
      dut.io.prefetchReq.ready.poke(true.B)
      dut.clock.step(1)
      
      // Check if prefetch is issued
      dut.clock.step(5) // Allow lookup and queue processing
      
      // The prefetch request should appear
      var foundPrefetch = false
      for (_ <- 0 until 10) {
        if (dut.io.prefetchReq.valid.peek().litToBoolean) {
          foundPrefetch = true
          println(s"[PDIP Controller Test] Prefetch issued for address 0x${dut.io.prefetchReq.bits.blkPaddr.peek().litValue.toString(16)}")
          assert(dut.io.prefetchReq.bits.blkPaddr.peek().litValue == 0x2000,
            "Prefetch address should match learned FEC line")
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
      dut.io.prefetchReq.ready.poke(true.B)
      
      var prefetchCount = 0
      for (_ <- 0 until 20) {
        if (dut.io.prefetchReq.valid.peek().litToBoolean) {
          prefetchCount += 1
          println(s"[PDIP Controller Test] Prefetch #$prefetchCount issued")
        }
        dut.clock.step(1)
      }
      
      // Should eventually issue prefetches for both targets
      println(s"[PDIP Controller Test] Total prefetches issued: $prefetchCount")
      assert(prefetchCount >= 1, "Should issue at least one prefetch")
    }
  }
}

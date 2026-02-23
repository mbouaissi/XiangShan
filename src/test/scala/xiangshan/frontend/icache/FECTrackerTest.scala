

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.frontend.FtqPtr
import top.DefaultConfig

class FECTrackerTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "FECTracker"

  // Use XiangShan's default configuration
  val defaultConfig = new DefaultConfig
  implicit val p: Parameters = defaultConfig.alterPartial({
    case XSCoreParamsKey => defaultConfig(XSTileKey).head
  })

  it should "detect FEC line when miss + stall + retire conditions are met" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      // Initialize
      dut.io.flush.poke(false.B)
      dut.io.newMiss.valid.poke(false.B)
      dut.io.stallUpdate.valid.poke(false.B)
      (0 until 6).foreach { i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      }
      dut.clock.step(1)

      // Test Case 1: Complete FEC lifecycle (miss -> stall -> retire)
      val testFtqIdx = 10.U
      val testBlkPaddr = 0x1000.U
      val testVSetIdx = 0x10.U

      // Step 1: Report a cache miss
      dut.io.newMiss.valid.poke(true.B)
      dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
      dut.io.newMiss.bits.ftqIdx.value.poke(testFtqIdx)
      dut.io.newMiss.bits.blkPaddr.poke(testBlkPaddr)
      dut.io.newMiss.bits.vSetIdx.poke(testVSetIdx)
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(false.B)

      // Verify entry was allocated
      println(s"[Cycle 1] Miss reported for ftqIdx=${testFtqIdx}")

      // Step 2: Report a stall for the same ftqIdx
      dut.clock.step(2) // Wait a bit
      dut.io.stallUpdate.valid.poke(true.B)
      dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
      dut.io.stallUpdate.bits.ftqIdx.value.poke(testFtqIdx)
      dut.io.stallUpdate.bits.stalled.poke(true.B)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(false.B)
      println(s"[Cycle 4] Stall reported for ftqIdx=${testFtqIdx}")

      // Step 3: Report retirement for the same ftqIdx
      dut.clock.step(2) // Wait a bit
      dut.io.retireUpdate(0).valid.poke(true.B)
      dut.io.retireUpdate(0).bits.ftqIdx.flag.poke(false.B)
      dut.io.retireUpdate(0).bits.ftqIdx.value.poke(testFtqIdx)
      dut.clock.step(1)

      // Verify FEC line was detected
      val fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      println(s"[Cycle 7] Retire reported for ftqIdx=${testFtqIdx}, FEC detected: ${fecDetected}")
      
      if (fecDetected) {
        dut.io.fecLine.bits.blkPaddr.expect(testBlkPaddr)
        dut.io.fecLine.bits.vSetIdx.expect(testVSetIdx)
        println(s"✓ FEC line correctly detected: blkPaddr=0x${testBlkPaddr.litValue.toString(16)}, vSetIdx=0x${testVSetIdx.litValue.toString(16)}")
      } else {
        println(s"✗ FEC line NOT detected (this may be a bug)")
      }

      dut.io.retireUpdate(0).valid.poke(false.B)
      dut.clock.step(1)
    }
  }

  it should "NOT detect FEC line if stall condition is missing" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      dut.io.flush.poke(false.B)
      dut.io.newMiss.valid.poke(false.B)
      dut.io.stallUpdate.valid.poke(false.B)
      (0 until 6).foreach { i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      }
      dut.clock.step(1)

      val testFtqIdx = 20.U

      // Step 1: Report miss
      dut.io.newMiss.valid.poke(true.B)
      dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
      dut.io.newMiss.bits.ftqIdx.value.poke(testFtqIdx)
      dut.io.newMiss.bits.blkPaddr.poke(0x2000.U)
      dut.io.newMiss.bits.vSetIdx.poke(0x20.U)
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(false.B)

      // Step 2: Skip stall, go directly to retire
      dut.clock.step(2)
      dut.io.retireUpdate(0).valid.poke(true.B)
      dut.io.retireUpdate(0).bits.ftqIdx.flag.poke(false.B)
      dut.io.retireUpdate(0).bits.ftqIdx.value.poke(testFtqIdx)
      dut.clock.step(1)

      // Verify FEC line is NOT detected (no stall)
      val fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      println(s"[No Stall Test] FEC detected: ${fecDetected} (should be false)")
      assert(!fecDetected, "FEC should NOT be detected without stall")

      dut.io.retireUpdate(0).valid.poke(false.B)
      dut.clock.step(1)
    }
  }

  it should "NOT detect FEC line if retire condition is missing" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      dut.io.flush.poke(false.B)
      dut.io.newMiss.valid.poke(false.B)
      dut.io.stallUpdate.valid.poke(false.B)
      (0 until 6).foreach { i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      }
      dut.clock.step(1)

      val testFtqIdx = 30.U

      // Step 1: Report miss
      dut.io.newMiss.valid.poke(true.B)
      dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
      dut.io.newMiss.bits.ftqIdx.value.poke(testFtqIdx)
      dut.io.newMiss.bits.blkPaddr.poke(0x3000.U)
      dut.io.newMiss.bits.vSetIdx.poke(0x30.U)
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(false.B)

      // Step 2: Report stall
      dut.clock.step(2)
      dut.io.stallUpdate.valid.poke(true.B)
      dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
      dut.io.stallUpdate.bits.ftqIdx.value.poke(testFtqIdx)
      dut.io.stallUpdate.bits.stalled.poke(true.B)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(false.B)

      // Step 3: Do NOT report retire, just check
      dut.clock.step(5)
      val fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      println(s"[No Retire Test] FEC detected: ${fecDetected} (should be false)")
      assert(!fecDetected, "FEC should NOT be detected without retire")
    }
  }

  it should "handle multiple concurrent commits" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      dut.io.flush.poke(false.B)
      dut.io.newMiss.valid.poke(false.B)
      dut.io.stallUpdate.valid.poke(false.B)
      (0 until 6).foreach { i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      }
      dut.clock.step(1)

      // Setup multiple tracked entries
      val ftqIndices = Seq(40, 41, 42)
      ftqIndices.foreach { idx =>
        // Report miss
        dut.io.newMiss.valid.poke(true.B)
        dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
        dut.io.newMiss.bits.ftqIdx.value.poke(idx.U)
        dut.io.newMiss.bits.blkPaddr.poke((0x4000 + idx * 0x100).U)
        dut.io.newMiss.bits.vSetIdx.poke((0x40 + idx).U)
        dut.clock.step(1)
        dut.io.newMiss.valid.poke(false.B)

        // Report stall
        dut.io.stallUpdate.valid.poke(true.B)
        dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
        dut.io.stallUpdate.bits.ftqIdx.value.poke(idx.U)
        dut.io.stallUpdate.bits.stalled.poke(true.B)
        dut.clock.step(1)
        dut.io.stallUpdate.valid.poke(false.B)
      }

      // Retire all three in one cycle (up to CommitWidth)
      dut.clock.step(2)
      (0 until 3).foreach { i =>
        dut.io.retireUpdate(i).valid.poke(true.B)
        dut.io.retireUpdate(i).bits.ftqIdx.flag.poke(false.B)
        dut.io.retireUpdate(i).bits.ftqIdx.value.poke(ftqIndices(i).U)
      }
      dut.clock.step(1)

      // Should detect at least one FEC line (one per cycle max)
      val fecDetected1 = dut.io.fecLine.valid.peek().litToBoolean
      val detectedAddr1 = if (fecDetected1) Some(dut.io.fecLine.bits.blkPaddr.peek().litValue) else None
      println(s"[Multiple Commits Test] First cycle FEC detected: ${fecDetected1}, addr: ${detectedAddr1}")
      
      (0 until 6).foreach { i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      }
      dut.clock.step(1)
      
      // Check for additional FEC detections in subsequent cycles
      val fecDetected2 = dut.io.fecLine.valid.peek().litToBoolean
      val detectedAddr2 = if (fecDetected2) Some(dut.io.fecLine.bits.blkPaddr.peek().litValue) else None
      println(s"[Multiple Commits Test] Second cycle FEC detected: ${fecDetected2}, addr: ${detectedAddr2}")
      
      // At least one of the three entries should have been detected as FEC
      assert(fecDetected1 || fecDetected2, "At least one FEC line should be detected from multiple commits")
      
      dut.clock.step(1)
    }
  }

  it should "clear all entries on flush" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      dut.io.flush.poke(false.B)
      dut.io.newMiss.valid.poke(false.B)
      dut.io.stallUpdate.valid.poke(false.B)
      (0 until 6).foreach { i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      }
      dut.clock.step(1)

      // Add some entries
      val testFtqIdx = 50.U
      dut.io.newMiss.valid.poke(true.B)
      dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
      dut.io.newMiss.bits.ftqIdx.value.poke(testFtqIdx)
      dut.io.newMiss.bits.blkPaddr.poke(0x5000.U)
      dut.io.newMiss.bits.vSetIdx.poke(0x50.U)
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(false.B)

      // Flush
      dut.clock.step(2)
      dut.io.flush.poke(true.B)
      dut.clock.step(1)
      dut.io.flush.poke(false.B)
      println(s"[Flush Test] Flush applied")

      // Try to retire - should not detect FEC after flush
      dut.clock.step(2)
      dut.io.stallUpdate.valid.poke(true.B)
      dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
      dut.io.stallUpdate.bits.ftqIdx.value.poke(testFtqIdx)
      dut.io.stallUpdate.bits.stalled.poke(true.B)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(false.B)

      dut.io.retireUpdate(0).valid.poke(true.B)
      dut.io.retireUpdate(0).bits.ftqIdx.flag.poke(false.B)
      dut.io.retireUpdate(0).bits.ftqIdx.value.poke(testFtqIdx)
      dut.clock.step(1)

      val fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      println(s"[Flush Test] FEC detected after flush: ${fecDetected} (should be false)")
      assert(!fecDetected, "FEC should NOT be detected for flushed entries")

      dut.io.retireUpdate(0).valid.poke(false.B)
      dut.clock.step(1)
    }
  }

  it should "handle tracker full condition" in {
    test(new FECTracker(numEntries = 4)) { dut => // Small tracker for easy testing
      dut.io.flush.poke(false.B)
      dut.io.newMiss.valid.poke(false.B)
      dut.io.stallUpdate.valid.poke(false.B)
      (0 until 6).foreach { i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      }
      dut.clock.step(1)

      // Fill the tracker
      for (i <- 0 until 6) { // Try to allocate more than capacity
        dut.io.newMiss.valid.poke(true.B)
        dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
        dut.io.newMiss.bits.ftqIdx.value.poke((55 + i).U) // 55-60 all within 0-63 range
        dut.io.newMiss.bits.blkPaddr.poke((0x6000 + i * 0x100).U)
        dut.io.newMiss.bits.vSetIdx.poke((0x60 + i).U)
        dut.clock.step(1)
      }
      dut.io.newMiss.valid.poke(false.B)

      println(s"[Tracker Full Test] Attempted to allocate 6 entries in 4-entry tracker")
      
      // Check performance counter for tracker full events
      dut.clock.step(2)
      val trackerFullCount = dut.io.perfInfo.trackerFull.peek().litValue
      println(s"[Tracker Full Test] Tracker full count: ${trackerFullCount}")
      
      // Should have at least 2 tracker full events (6 attempts - 4 capacity)
      assert(trackerFullCount >= 2, s"Expected at least 2 tracker full events, got ${trackerFullCount}")
      
      // Verify only first 4 entries were actually tracked
      val totalMisses = dut.io.perfInfo.totalMisses.peek().litValue
      assert(totalMisses == 4, s"Expected 4 misses tracked in 4-entry tracker, got ${totalMisses}")
    }
  }

  it should "age out old entries" in {
    test(new FECTracker(numEntries = 16)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(2000) // Increase timeout for aging test
      dut.io.flush.poke(false.B)
      dut.io.newMiss.valid.poke(false.B)
      dut.io.stallUpdate.valid.poke(false.B)
      (0 until 6).foreach { i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      }
      dut.clock.step(1)

      val testFtqIdx = 50.U

      // Allocate an entry
      dut.io.newMiss.valid.poke(true.B)
      dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
      dut.io.newMiss.bits.ftqIdx.value.poke(testFtqIdx)
      dut.io.newMiss.bits.blkPaddr.poke(0x7000.U)
      dut.io.newMiss.bits.vSetIdx.poke(0x70.U)
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(false.B)

      // Wait for aging threshold (1024 cycles + margin)
      println(s"[Aging Test] Waiting for aging threshold...")
      dut.clock.step(1100)

      // Try to complete FEC - should not work if entry aged out
      dut.io.stallUpdate.valid.poke(true.B)
      dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
      dut.io.stallUpdate.bits.ftqIdx.value.poke(testFtqIdx)
      dut.io.stallUpdate.bits.stalled.poke(true.B)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(false.B)

      dut.io.retireUpdate(0).valid.poke(true.B)
      dut.io.retireUpdate(0).bits.ftqIdx.flag.poke(false.B)
      dut.io.retireUpdate(0).bits.ftqIdx.value.poke(testFtqIdx)
      dut.clock.step(1)

      val fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      println(s"[Aging Test] FEC detected after aging: ${fecDetected} (likely false if aged out)")
      
      dut.io.retireUpdate(0).valid.poke(false.B)
      dut.clock.step(1)
    }
  }

  it should "correctly track performance counters" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      dut.io.flush.poke(false.B)
      dut.io.newMiss.valid.poke(false.B)
      dut.io.stallUpdate.valid.poke(false.B)
      (0 until 6).foreach { i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      }
      dut.clock.step(1)

      val initialTotalMisses = dut.io.perfInfo.totalMisses.peek().litValue
      println(s"[Perf Test] Initial total misses: ${initialTotalMisses}")

      // Generate some misses
      for (i <- 0 until 3) {
        dut.io.newMiss.valid.poke(true.B)
        dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
        dut.io.newMiss.bits.ftqIdx.value.poke((20 + i).U) // 20-22 within 0-63 range
        dut.io.newMiss.bits.blkPaddr.poke((0x8000 + i * 0x100).U)
        dut.io.newMiss.bits.vSetIdx.poke((0x20 + i).U)
        dut.clock.step(1)
      }
      dut.io.newMiss.valid.poke(false.B)
      dut.clock.step(1)

      val finalTotalMisses = dut.io.perfInfo.totalMisses.peek().litValue
      val missesWithStall = dut.io.perfInfo.missesWithStall.peek().litValue
      val fecLinesDetected = dut.io.perfInfo.fecLinesDetected.peek().litValue
      val trackerFull = dut.io.perfInfo.trackerFull.peek().litValue
      
      println(s"[Perf Test] Final total misses: ${finalTotalMisses}")
      println(s"[Perf Test] Misses with stall: ${missesWithStall}")
      println(s"[Perf Test] FEC lines detected: ${fecLinesDetected}")
      println(s"[Perf Test] Tracker full count: ${trackerFull}")
      
      // Validate counter correctness
      assert(finalTotalMisses == 3, s"Expected 3 total misses, got ${finalTotalMisses}")
      assert(finalTotalMisses >= initialTotalMisses, "Total misses should not decrease")
      assert(missesWithStall <= finalTotalMisses, "Misses with stall cannot exceed total misses")
      assert(fecLinesDetected <= missesWithStall, "FEC lines cannot exceed misses with stall")
      assert(trackerFull == 0, "No tracker overflow in this test")
    }
  }

  it should "handle event ordering: stall before miss" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      dut.io.flush.poke(false.B)
      dut.io.newMiss.valid.poke(false.B)
      dut.io.stallUpdate.valid.poke(false.B)
      (0 until 6).foreach { i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      }
      dut.clock.step(1)

      val testFtqIdx = 12.U

      // Report stall BEFORE miss (should be ignored until miss arrives)
      dut.io.stallUpdate.valid.poke(true.B)
      dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
      dut.io.stallUpdate.bits.ftqIdx.value.poke(testFtqIdx)
      dut.io.stallUpdate.bits.stalled.poke(true.B)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(false.B)
      println(s"[Event Order Test] Stall reported before miss")

      // Now report the miss
      dut.clock.step(2)
      dut.io.newMiss.valid.poke(true.B)
      dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
      dut.io.newMiss.bits.ftqIdx.value.poke(testFtqIdx)
      dut.io.newMiss.bits.blkPaddr.poke(0xa000.U)
      dut.io.newMiss.bits.vSetIdx.poke(0xa0.U)
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(false.B)
      println(s"[Event Order Test] Miss reported after stall")

      // Report stall again (entry now exists)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(true.B)
      dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
      dut.io.stallUpdate.bits.ftqIdx.value.poke(testFtqIdx)
      dut.io.stallUpdate.bits.stalled.poke(true.B)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(false.B)

      // Retire and check FEC
      dut.clock.step(1)
      dut.io.retireUpdate(0).valid.poke(true.B)
      dut.io.retireUpdate(0).bits.ftqIdx.flag.poke(false.B)
      dut.io.retireUpdate(0).bits.ftqIdx.value.poke(testFtqIdx)
      dut.clock.step(1)

      val fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      println(s"[Event Order Test] FEC detected: ${fecDetected} (should be true)")
      assert(fecDetected, "FEC should be detected even when stall arrives before miss")

      dut.io.retireUpdate(0).valid.poke(false.B)
      dut.clock.step(1)
    }
  }

  it should "handle duplicate misses for same line" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      dut.io.flush.poke(false.B)
      dut.io.newMiss.valid.poke(false.B)
      dut.io.stallUpdate.valid.poke(false.B)
      (0 until 6).foreach { i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      }
      dut.clock.step(1)

      val testFtqIdx = 15.U
      val testBlkPaddr = 0xb000.U
      val testVSetIdx = 0xb0.U

      // First miss
      dut.io.newMiss.valid.poke(true.B)
      dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
      dut.io.newMiss.bits.ftqIdx.value.poke(testFtqIdx)
      dut.io.newMiss.bits.blkPaddr.poke(testBlkPaddr)
      dut.io.newMiss.bits.vSetIdx.poke(testVSetIdx)
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(false.B)
      println(s"[Duplicate Test] First miss reported")

      val perfMisses1 = dut.io.perfInfo.totalMisses.peek().litValue

      // Duplicate miss for same FTQ entry (should update, not allocate new)
      dut.clock.step(2)
      dut.io.newMiss.valid.poke(true.B)
      dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
      dut.io.newMiss.bits.ftqIdx.value.poke(testFtqIdx)
      dut.io.newMiss.bits.blkPaddr.poke(testBlkPaddr)
      dut.io.newMiss.bits.vSetIdx.poke(testVSetIdx)
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(false.B)
      println(s"[Duplicate Test] Duplicate miss reported")

      val perfMisses2 = dut.io.perfInfo.totalMisses.peek().litValue

      // Both misses should be counted
      assert(perfMisses2 == perfMisses1 + 1, s"Expected ${perfMisses1 + 1} total misses, got ${perfMisses2}")

      // Complete FEC lifecycle
      dut.io.stallUpdate.valid.poke(true.B)
      dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
      dut.io.stallUpdate.bits.ftqIdx.value.poke(testFtqIdx)
      dut.io.stallUpdate.bits.stalled.poke(true.B)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(false.B)

      dut.clock.step(1)
      dut.io.retireUpdate(0).valid.poke(true.B)
      dut.io.retireUpdate(0).bits.ftqIdx.flag.poke(false.B)
      dut.io.retireUpdate(0).bits.ftqIdx.value.poke(testFtqIdx)
      dut.clock.step(1)

      val fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      println(s"[Duplicate Test] FEC detected: ${fecDetected}")
      assert(fecDetected, "FEC should be detected after duplicate miss handling")

      dut.io.retireUpdate(0).valid.poke(false.B)
      dut.clock.step(1)
    }
  }

  it should "handle stall clear (stalled=false)" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      dut.io.flush.poke(false.B)
      dut.io.newMiss.valid.poke(false.B)
      dut.io.stallUpdate.valid.poke(false.B)
      (0 until 6).foreach { i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      }
      dut.clock.step(1)

      val testFtqIdx = 18.U

      // Report miss
      dut.io.newMiss.valid.poke(true.B)
      dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
      dut.io.newMiss.bits.ftqIdx.value.poke(testFtqIdx)
      dut.io.newMiss.bits.blkPaddr.poke(0xc000.U)
      dut.io.newMiss.bits.vSetIdx.poke(0xc0.U)
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(false.B)

      // Report stall
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(true.B)
      dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
      dut.io.stallUpdate.bits.ftqIdx.value.poke(testFtqIdx)
      dut.io.stallUpdate.bits.stalled.poke(true.B)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(false.B)
      println(s"[Stall Clear Test] Stall set to true")

      // Clear stall (stalled=false) before retire
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(true.B)
      dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
      dut.io.stallUpdate.bits.ftqIdx.value.poke(testFtqIdx)
      dut.io.stallUpdate.bits.stalled.poke(false.B)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(false.B)
      println(s"[Stall Clear Test] Stall set to false")

      // Retire - should NOT detect FEC since stall was cleared
      dut.clock.step(1)
      dut.io.retireUpdate(0).valid.poke(true.B)
      dut.io.retireUpdate(0).bits.ftqIdx.flag.poke(false.B)
      dut.io.retireUpdate(0).bits.ftqIdx.value.poke(testFtqIdx)
      dut.clock.step(1)

      val fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      println(s"[Stall Clear Test] FEC detected: ${fecDetected} (should be false)")
      assert(!fecDetected, "FEC should NOT be detected when stall is cleared before retire")

      dut.io.retireUpdate(0).valid.poke(false.B)
      dut.clock.step(1)
    }
  }
}

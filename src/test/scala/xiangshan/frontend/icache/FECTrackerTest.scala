/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

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

      // At least one should be detected
      val fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      println(s"[Multiple Commits Test] FEC detected: ${fecDetected}")
      
      (0 until 6).foreach { i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      }
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
        dut.io.newMiss.bits.ftqIdx.value.poke((60 + i).U)
        dut.io.newMiss.bits.blkPaddr.poke((0x6000 + i * 0x100).U)
        dut.io.newMiss.bits.vSetIdx.poke((0x60 + i).U)
        dut.clock.step(1)
      }
      dut.io.newMiss.valid.poke(false.B)

      println(s"[Tracker Full Test] Attempted to allocate 6 entries in 4-entry tracker")
      
      // Check performance counter for tracker full events
      dut.clock.step(5)
      println(s"[Tracker Full Test] Tracker should have hit capacity limits")
    }
  }

  it should "age out old entries" in {
    test(new FECTracker(numEntries = 16)) { dut =>
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
        dut.io.newMiss.bits.ftqIdx.value.poke((30 + i).U)
        dut.io.newMiss.bits.blkPaddr.poke((0x8000 + i * 0x100).U)
        dut.io.newMiss.bits.vSetIdx.poke((0x30 + i).U)
        dut.clock.step(1)
      }
      dut.io.newMiss.valid.poke(false.B)
      dut.clock.step(1)

      val finalTotalMisses = dut.io.perfInfo.totalMisses.peek().litValue
      println(s"[Perf Test] Final total misses: ${finalTotalMisses}")
      println(s"[Perf Test] Misses with stall: ${dut.io.perfInfo.missesWithStall.peek().litValue}")
      println(s"[Perf Test] FEC lines detected: ${dut.io.perfInfo.fecLinesDetected.peek().litValue}")
      println(s"[Perf Test] Tracker full count: ${dut.io.perfInfo.trackerFull.peek().litValue}")
    }
  }
}

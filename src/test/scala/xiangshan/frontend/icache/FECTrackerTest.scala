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

class FECTrackerTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  behavior of "FECTracker"

  val defaultConfig = new DefaultConfig
  implicit val p: Parameters = defaultConfig.alterPartial({
    case XSCoreParamsKey => defaultConfig(XSTileKey).head
  })

  val commitWidth = p(XSCoreParamsKey).CommitWidth

  def initDUT(dut: FECTracker): Unit = {
    dut.io.fencei.poke(false.B)
    dut.io.newMiss.valid.poke(false.B)
    dut.io.stallUpdate.valid.poke(false.B)
    (0 until commitWidth).foreach(i =>
      dut.io.retireUpdate(i).valid.poke(false.B)
    )
    dut.clock.step(1)
  }

  def pokeMiss(
      dut: FECTracker,
      ftqIdx: UInt,
      blkPaddr: UInt,
      vSetIdx: UInt
  ): Unit = {
    dut.io.newMiss.valid.poke(true.B)
    dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
    dut.io.newMiss.bits.ftqIdx.value.poke(ftqIdx)
    dut.io.newMiss.bits.blkPaddr.poke(blkPaddr)
    dut.io.newMiss.bits.vSetIdx.poke(vSetIdx)
  }

  def clearMiss(dut: FECTracker): Unit = {
    dut.io.newMiss.valid.poke(false.B)
  }

  def pokeStall(dut: FECTracker, ftqIdx: UInt, stalled: Boolean): Unit = {
    dut.io.stallUpdate.valid.poke(true.B)
    dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
    dut.io.stallUpdate.bits.ftqIdx.value.poke(ftqIdx)
    dut.io.stallUpdate.bits.stalled.poke(stalled.B)
  }

  def clearStall(dut: FECTracker): Unit = {
    dut.io.stallUpdate.valid.poke(false.B)
  }

  def pokeRetire(dut: FECTracker, port: Int, ftqIdx: UInt): Unit = {
    dut.io.retireUpdate(port).valid.poke(true.B)
    dut.io.retireUpdate(port).bits.ftqIdx.flag.poke(false.B)
    dut.io.retireUpdate(port).bits.ftqIdx.value.poke(ftqIdx)
  }

  def clearRetire(dut: FECTracker, port: Int): Unit = {
    dut.io.retireUpdate(port).valid.poke(false.B)
  }

  def expectPerfCounters(
      dut: FECTracker,
      totalMisses: Int,
      missesWithStall: Int,
      fecLinesDetected: Int
  ): Unit = {
    dut.io.perfInfo.totalMisses.expect(totalMisses.U)
    dut.io.perfInfo.missesWithStall.expect(missesWithStall.U)
    dut.io.perfInfo.fecLinesDetected.expect(fecLinesDetected.U)
  }

  it should "detect FEC line when miss -> stall -> retire conditions are met" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      initDUT(dut)

      val testFtqIdx = 10.U
      val testBlkPaddr = 0x1000.U
      val testVSetIdx = 0x10.U

      // Report a cache miss
      pokeMiss(dut, testFtqIdx, testBlkPaddr, testVSetIdx)
      dut.clock.step(1)
      clearMiss(dut)

      // Verify entry was allocated
      println(s"[Cycle 1] Miss reported for ftqIdx=${testFtqIdx}")
      // FEC should not be detected yet (missing stall and retire)
      var fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      println(s"[Cycle 1] FEC detected: ${fecDetected} (should be false)")
      assert(!fecDetected, "FEC should NOT be detected immediately after miss")
      expectPerfCounters(
        dut,
        totalMisses = 1,
        missesWithStall = 0,
        fecLinesDetected = 0
      )

      // Report a stall for the same ftqIdx
      dut.clock.step(2)
      pokeStall(dut, testFtqIdx, stalled = true)
      dut.clock.step(1)
      clearStall(dut)
      println(s"[Cycle 4] Stall reported for ftqIdx=${testFtqIdx}")

      // FEC should still not be detected
      fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      println(s"[Cycle 4] FEC detected: ${fecDetected} (should be false)")
      assert(
        !fecDetected,
        "FEC should NOT be detected after stall but before retire"
      )
      expectPerfCounters(
        dut,
        totalMisses = 1,
        missesWithStall = 1,
        fecLinesDetected = 0
      )

      // Report retirement for the same ftqIdx
      dut.clock.step(2)
      pokeRetire(dut, port = 0, testFtqIdx)
      dut.clock.step(1)

      // Verify FEC line was detected
      fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      assert(fecDetected, "FEC should be detected after retire")
      dut.io.fecLine.bits.blkPaddr.expect(testBlkPaddr)
      dut.io.fecLine.bits.vSetIdx.expect(testVSetIdx)
      expectPerfCounters(
        dut,
        totalMisses = 1,
        missesWithStall = 1,
        fecLinesDetected = 1
      )

      println(
        s"[Cycle 7] FEC detected: blkPaddr=0x${testBlkPaddr.litValue.toString(16)}, vSetIdx=0x${testVSetIdx.litValue.toString(16)}"
      )

      clearRetire(dut, port = 0)
      dut.clock.step(1)
    }
  }

  it should "detect FEC line when miss -> retire -> stall conditions are met" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      initDUT(dut)

      val testFtqIdx = 10.U
      val testBlkPaddr = 0x1000.U
      val testVSetIdx = 0x10.U

      // miss
      pokeMiss(dut, testFtqIdx, testBlkPaddr, testVSetIdx)
      dut.clock.step(1)
      clearMiss(dut)

      dut.io.fecLine.valid.expect(false.B)
      expectPerfCounters(
        dut,
        totalMisses = 1,
        missesWithStall = 0,
        fecLinesDetected = 0
      )

      //  retire (before stall)
      dut.clock.step(2)
      pokeRetire(dut, port = 0, testFtqIdx)
      dut.clock.step(1)
      clearRetire(dut, port = 0)

      // Still should NOT detect (stall missing)
      dut.io.fecLine.valid.expect(false.B)
      expectPerfCounters(
        dut,
        totalMisses = 1,
        missesWithStall = 0,
        fecLinesDetected = 0
      )

      //  stall arrives late
      dut.clock.step(2)
      pokeStall(dut, testFtqIdx, stalled = true)
      dut.clock.step(1)
      clearStall(dut)

      // Now should detect (stall completed the condition)
      dut.io.fecLine.valid.expect(true.B)
      dut.io.fecLine.bits.blkPaddr.expect(testBlkPaddr)
      dut.io.fecLine.bits.vSetIdx.expect(testVSetIdx)
      expectPerfCounters(
        dut,
        totalMisses = 1,
        missesWithStall = 1,
        fecLinesDetected = 1
      )

      dut.clock.step(1)
    }
  }

  it should "NOT detect FEC line if stall condition is missing" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      initDUT(dut)

      val testFtqIdx = 20.U

      // Report miss
      pokeMiss(dut, testFtqIdx, blkPaddr = 0x2000.U, vSetIdx = 0x20.U)
      dut.clock.step(1)
      clearMiss(dut)

      //  Skip stall, go directly to retire
      dut.clock.step(2)
      pokeRetire(dut, port = 0, testFtqIdx)
      dut.clock.step(1)

      // Verify FEC line is NOT detected (no stall)
      val fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      println(s"[No Stall Test] FEC detected: ${fecDetected} (should be false)")
      assert(!fecDetected, "FEC should NOT be detected without stall")

      clearRetire(dut, port = 0)
      dut.clock.step(1)
    }
  }

  it should "NOT detect FEC line if retire condition is missing" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      initDUT(dut)

      val testFtqIdx = 30.U

      // Report miss
      pokeMiss(dut, testFtqIdx, blkPaddr = 0x3000.U, vSetIdx = 0x30.U)
      dut.clock.step(1)
      clearMiss(dut)

      //  Report stall
      dut.clock.step(2)
      pokeStall(dut, testFtqIdx, stalled = true)
      dut.clock.step(1)
      clearStall(dut)

      //  Do NOT report retire, just check
      dut.clock.step(5)
      val fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      println(
        s"[No Retire Test] FEC detected: ${fecDetected} (should be false)"
      )
      assert(!fecDetected, "FEC should NOT be detected without retire")
    }
  }

  it should "NOT detect FEC line if miss condition is missing" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      initDUT(dut)

      val testFtqIdx = 40.U

      // Skip miss, go directly to stall
      dut.clock.step(2)
      pokeStall(dut, testFtqIdx, stalled = true)
      dut.clock.step(1)
      clearStall(dut)

      //  Report retire
      dut.clock.step(2)
      pokeRetire(dut, port = 0, testFtqIdx)
      dut.clock.step(1)

      // Verify FEC line is NOT detected (no miss)
      val fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      println(s"[No Miss Test] FEC detected: ${fecDetected} (should be false)")
      assert(!fecDetected, "FEC should NOT be detected without miss")

      clearRetire(dut, port = 0)
      dut.clock.step(1)
    }
  }

  it should "handle multiple concurrent commits" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      initDUT(dut)

      // Setup multiple tracked entries
      val ftqIndices = Seq(40, 41, 42)
      ftqIndices.foreach { idx =>
        // Report miss
        pokeMiss(
          dut,
          idx.U,
          blkPaddr = (0x4000 + idx * 0x100).U,
          vSetIdx = (0x40 + idx).U
        )
        dut.clock.step(1)
        clearMiss(dut)

        // Report stall
        pokeStall(dut, idx.U, stalled = true)
        dut.clock.step(1)
        clearStall(dut)
      }

      // Retire all three in one cycle (up to CommitWidth)
      dut.clock.step(2)
      (0 until 3).foreach { i =>
        pokeRetire(dut, port = i, ftqIndices(i).U)
      }
      dut.clock.step(1)

      // Should detect exactly one FEC line (one per cycle max)
      val fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      assert(fecDetected, "FEC line should be detected after multiple commits")

      val detectedAddr = dut.io.fecLine.bits.blkPaddr.peek().litValue
      val expectedAddrs = ftqIndices.map(idx => 0x4000 + idx * 0x100)
      assert(
        expectedAddrs.contains(detectedAddr.toInt),
        s"Detected address 0x${detectedAddr.toString(16)} should be one of ${expectedAddrs.map(a => s"0x${a.toHexString}").mkString(", ")}"
      )
      println(
        s"[Multiple Commits Test] FEC detected with addr: 0x${detectedAddr.toString(16)}"
      )

      (0 until commitWidth).foreach { i =>
        clearRetire(dut, port = i)
      }
      dut.clock.step(1)
    }
  }

  it should "clear all entries on flush" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      initDUT(dut)

      // Add some entries
      val testFtqIdx = 50.U
      pokeMiss(dut, testFtqIdx, blkPaddr = 0x5000.U, vSetIdx = 0x50.U)
      dut.clock.step(1)
      clearMiss(dut)

      // Flush (fence.i)
      dut.clock.step(2)
      dut.io.fencei.poke(true.B)
      dut.clock.step(1)
      dut.io.fencei.poke(false.B)
      println(s"[Flush Test] Fence.i applied")

      // Try to retire, should not detect FEC after flush
      dut.clock.step(2)
      pokeStall(dut, testFtqIdx, stalled = true)
      dut.clock.step(1)
      clearStall(dut)

      pokeRetire(dut, port = 0, testFtqIdx)
      dut.clock.step(1)

      val fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      println(
        s"[Flush Test] FEC detected after flush: ${fecDetected} (should be false)"
      )
      assert(!fecDetected, "FEC should NOT be detected for flushed entries")

      clearRetire(dut, port = 0)
      dut.clock.step(1)
    }
  }

  it should "handle tracker full condition" in {
    test(new FECTracker(numEntries = 4)) {
      dut => // Small tracker for easy testing
        initDUT(dut)

        // Fill the tracker
        for (i <- 0 until 6) { // Try to allocate more than capacity
          pokeMiss(
            dut,
            (55 + i).U,
            blkPaddr = (0x6000 + i * 0x100).U,
            vSetIdx = (0x60 + i).U
          )
          dut.clock.step(1)
        }
        clearMiss(dut)

        println(
          s"[Tracker Full Test] Attempted to allocate 6 entries in 4-entry tracker"
        )

        // Check performance counter for tracker full events
        dut.clock.step(2)
        val trackerFullCount = dut.io.perfInfo.trackerFull.peek().litValue
        println(s"[Tracker Full Test] Tracker full count: ${trackerFullCount}")

        // Should have at least 2 tracker full events (6 attempts - 4 capacity)
        assert(
          trackerFullCount >= 2,
          s"Expected at least 2 tracker full events, got ${trackerFullCount}"
        )

        // Verify all 6 miss events were counted (even those that couldn't be tracked)
        val totalMisses = dut.io.perfInfo.totalMisses.peek().litValue
        assert(
          totalMisses == 6,
          s"Expected 6 total miss events, got ${totalMisses}"
        )
    }
  }

  it should "age out old entries" in {
    test(new FECTracker(numEntries = 16))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        dut.clock.setTimeout(2000) // Increase timeout for aging test
        initDUT(dut)

        val testFtqIdx = 50.U

        // Allocate an entry
        pokeMiss(dut, testFtqIdx, blkPaddr = 0x7000.U, vSetIdx = 0x70.U)
        dut.clock.step(1)
        clearMiss(dut)

        // Wait for aging threshold (1024 cycles + margin)
        println(s"[Aging Test] Waiting for aging threshold...")
        dut.clock.step(1100)

        // Try to complete FEC - should not work if entry aged out
        pokeStall(dut, testFtqIdx, stalled = true)
        dut.clock.step(1)
        clearStall(dut)

        pokeRetire(dut, port = 0, testFtqIdx)
        dut.clock.step(1)

        val fecDetected = dut.io.fecLine.valid.peek().litToBoolean
        println(
          s"[Aging Test] FEC detected after aging: ${fecDetected} (likely false if aged out)"
        )

        clearRetire(dut, port = 0)
        dut.clock.step(1)
      }
  }

  it should "correctly track performance counters" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      initDUT(dut)

      val initialTotalMisses = dut.io.perfInfo.totalMisses.peek().litValue
      println(s"[Perf Test] Initial total misses: ${initialTotalMisses}")

      // Generate some misses
      for (i <- 0 until 3) {
        pokeMiss(
          dut,
          (20 + i).U,
          blkPaddr = (0x8000 + i * 0x100).U,
          vSetIdx = (0x20 + i).U
        )
        dut.clock.step(1)
      }
      clearMiss(dut)
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
      assert(
        finalTotalMisses == 3,
        s"Expected 3 total misses, got ${finalTotalMisses}"
      )
      assert(
        finalTotalMisses >= initialTotalMisses,
        "Total misses should not decrease"
      )
      assert(
        missesWithStall <= finalTotalMisses,
        "Misses with stall cannot exceed total misses"
      )
      assert(
        fecLinesDetected <= missesWithStall,
        "FEC lines cannot exceed misses with stall"
      )
      assert(trackerFull == 0, "No tracker overflow in this test")
    }
  }

  it should "handle event ordering: stall before miss" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      initDUT(dut)

      val testFtqIdx = 12.U

      // Report stall BEFORE miss (should be ignored until miss arrives)
      pokeStall(dut, testFtqIdx, stalled = true)
      dut.clock.step(1)
      clearStall(dut)
      println(s"[Event Order Test] Stall reported before miss")

      // Now report the miss
      dut.clock.step(2)
      pokeMiss(dut, testFtqIdx, blkPaddr = 0xa000.U, vSetIdx = 0xa0.U)
      dut.clock.step(1)
      clearMiss(dut)
      println(s"[Event Order Test] Miss reported after stall")

      // Report stall again (entry now exists)
      dut.clock.step(1)
      pokeStall(dut, testFtqIdx, stalled = true)
      dut.clock.step(1)
      clearStall(dut)

      // Retire and check FEC
      dut.clock.step(1)
      pokeRetire(dut, port = 0, testFtqIdx)
      dut.clock.step(1)

      val fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      println(
        s"[Event Order Test] FEC detected: ${fecDetected} (should be true)"
      )
      assert(
        fecDetected,
        "FEC should be detected even when stall arrives before miss"
      )

      clearRetire(dut, port = 0)
      dut.clock.step(1)
    }
  }

  it should "handle duplicate misses for same line" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      initDUT(dut)

      val testFtqIdx = 15.U
      val testBlkPaddr = 0xb000.U
      val testVSetIdx = 0xb0.U

      // First miss
      pokeMiss(dut, testFtqIdx, testBlkPaddr, testVSetIdx)
      dut.clock.step(1)
      clearMiss(dut)
      println(s"[Duplicate Test] First miss reported")

      val perfMisses1 = dut.io.perfInfo.totalMisses.peek().litValue

      // Duplicate miss for same FTQ entry (should update, not allocate new)
      dut.clock.step(2)
      pokeMiss(dut, testFtqIdx, testBlkPaddr, testVSetIdx)
      dut.clock.step(1)
      clearMiss(dut)
      println(s"[Duplicate Test] Duplicate miss reported")

      val perfMisses2 = dut.io.perfInfo.totalMisses.peek().litValue

      // Both misses should be counted
      assert(
        perfMisses2 == perfMisses1 + 1,
        s"Expected ${perfMisses1 + 1} total misses, got ${perfMisses2}"
      )

      // Complete FEC lifecycle
      pokeStall(dut, testFtqIdx, stalled = true)
      dut.clock.step(1)
      clearStall(dut)

      dut.clock.step(1)
      pokeRetire(dut, port = 0, testFtqIdx)
      dut.clock.step(1)

      val fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      println(s"[Duplicate Test] FEC detected: ${fecDetected}")
      assert(
        fecDetected,
        "FEC should be detected after duplicate miss handling"
      )

      clearRetire(dut, port = 0)
      dut.clock.step(1)
    }
  }

  it should "handle stall clear (stalled=false)" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      initDUT(dut)

      val testFtqIdx = 18.U

      // Report miss
      pokeMiss(dut, testFtqIdx, blkPaddr = 0xc000.U, vSetIdx = 0xc0.U)
      dut.clock.step(1)
      clearMiss(dut)

      // Report stall
      dut.clock.step(1)
      pokeStall(dut, testFtqIdx, stalled = true)
      dut.clock.step(1)
      clearStall(dut)
      println(s"[Stall Clear Test] Stall set to true")

      // Clear stall before retire
      dut.clock.step(1)
      pokeStall(dut, testFtqIdx, stalled = false)
      dut.clock.step(1)
      clearStall(dut)
      println(s"[Stall Clear Test] Stall set to false")

      // Retire should NOT detect FEC since stall was cleared
      dut.clock.step(1)
      pokeRetire(dut, port = 0, testFtqIdx)
      dut.clock.step(1)

      val fecDetected = dut.io.fecLine.valid.peek().litToBoolean
      println(
        s"[Stall Clear Test] FEC detected: ${fecDetected} (should be false)"
      )
      assert(
        !fecDetected,
        "FEC should NOT be detected when stall is cleared before retire"
      )

      clearRetire(dut, port = 0)
      dut.clock.step(1)
    }
  }
  it should "NOT detect FEC after entry ages out (asserted)" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      dut.clock.setTimeout(2000)
      initDUT(dut)

      val ftq = 9.U

      // Miss to allocate entry
      pokeMiss(dut, ftq, blkPaddr = 0x9000.U, vSetIdx = 0x90.U)
      dut.clock.step(1)
      clearMiss(dut)

      // Wait past agingThreshold (1024) with margin
      dut.clock.step(1100)

      // Now try to complete FEC (stall + retire)  should NOT detect because entry should be invalidated
      pokeStall(dut, ftq, stalled = true)
      dut.clock.step(1)
      clearStall(dut)

      pokeRetire(dut, port = 0, ftq)
      dut.clock.step(1)
      clearRetire(dut, port = 0)

      // Assert no detection
      dut.io.fecLine.valid.expect(false.B)
    }
  }

  it should "clear fecLine output state across flush" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      // Init
      dut.io.fencei.poke(false.B)
      dut.io.newMiss.valid.poke(false.B)
      dut.io.stallUpdate.valid.poke(false.B)
      (0 until commitWidth).foreach(i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      )
      dut.clock.step(1)

      val ftq = 11.U

      // Create a valid FEC detection: miss -> stall -> retire
      dut.io.newMiss.valid.poke(true.B)
      dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
      dut.io.newMiss.bits.ftqIdx.value.poke(ftq)
      dut.io.newMiss.bits.blkPaddr.poke(0x11000.U)
      dut.io.newMiss.bits.vSetIdx.poke(0x11.U)
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(false.B)

      dut.io.stallUpdate.valid.poke(true.B)
      dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
      dut.io.stallUpdate.bits.ftqIdx.value.poke(ftq)
      dut.io.stallUpdate.bits.stalled.poke(true.B)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(false.B)

      dut.io.retireUpdate(0).valid.poke(true.B)
      dut.io.retireUpdate(0).bits.ftqIdx.flag.poke(false.B)
      dut.io.retireUpdate(0).bits.ftqIdx.value.poke(ftq)
      dut.clock.step(1)
      dut.io.retireUpdate(0).valid.poke(false.B)

      // Must detect now
      dut.io.fecLine.valid.expect(true.B)

      // Next cycle: should clear (fecDetected reg is only 1 cycle)
      dut.clock.step(1)
      dut.io.fecLine.valid.expect(false.B)

      // Flush (fence.i) and ensure still false
      dut.io.fencei.poke(true.B)
      dut.clock.step(1)
      dut.io.fencei.poke(false.B)
      dut.io.fecLine.valid.expect(false.B)

      // Try to "finish" using same ftq (stall+retire) without a new miss: should not detect
      dut.io.stallUpdate.valid.poke(true.B)
      dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
      dut.io.stallUpdate.bits.ftqIdx.value.poke(ftq)
      dut.io.stallUpdate.bits.stalled.poke(true.B)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(false.B)

      dut.io.retireUpdate(0).valid.poke(true.B)
      dut.io.retireUpdate(0).bits.ftqIdx.flag.poke(false.B)
      dut.io.retireUpdate(0).bits.ftqIdx.value.poke(ftq)
      dut.clock.step(1)
      dut.io.retireUpdate(0).valid.poke(false.B)

      dut.io.fecLine.valid.expect(false.B)
    }
  }

  it should "count missesWithStall only on rising edge of stalledIFU" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      // Init
      dut.io.fencei.poke(false.B)
      dut.io.newMiss.valid.poke(false.B)
      dut.io.stallUpdate.valid.poke(false.B)
      (0 until commitWidth).foreach(i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      )
      dut.clock.step(1)

      val ftq = 13.U

      // Miss
      dut.io.newMiss.valid.poke(true.B)
      dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
      dut.io.newMiss.bits.ftqIdx.value.poke(ftq)
      dut.io.newMiss.bits.blkPaddr.poke(0x13000.U)
      dut.io.newMiss.bits.vSetIdx.poke(0x13.U)
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(false.B)

      dut.io.perfInfo.missesWithStall.expect(0.U)

      // Stall true (first time) -> increments
      dut.io.stallUpdate.valid.poke(true.B)
      dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
      dut.io.stallUpdate.bits.ftqIdx.value.poke(ftq)
      dut.io.stallUpdate.bits.stalled.poke(true.B)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(false.B)
      dut.io.perfInfo.missesWithStall.expect(1.U)

      // Stall true again -> should NOT increment (wasStalled already true)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(true.B)
      dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
      dut.io.stallUpdate.bits.ftqIdx.value.poke(ftq)
      dut.io.stallUpdate.bits.stalled.poke(true.B)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(false.B)

      dut.io.perfInfo.missesWithStall.expect(1.U)
    }
  }

  it should "increment missesWithStall again if stalled is cleared then re-asserted" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      // Init
      dut.io.fencei.poke(false.B)
      dut.io.newMiss.valid.poke(false.B)
      dut.io.stallUpdate.valid.poke(false.B)
      (0 until commitWidth).foreach(i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      )
      dut.clock.step(1)

      val ftq = 14.U

      // Miss
      dut.io.newMiss.valid.poke(true.B)
      dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
      dut.io.newMiss.bits.ftqIdx.value.poke(ftq)
      dut.io.newMiss.bits.blkPaddr.poke(0x14000.U)
      dut.io.newMiss.bits.vSetIdx.poke(0x14.U)
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(false.B)

      // Stall true -> +1
      dut.io.stallUpdate.valid.poke(true.B)
      dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
      dut.io.stallUpdate.bits.ftqIdx.value.poke(ftq)
      dut.io.stallUpdate.bits.stalled.poke(true.B)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(false.B)
      dut.io.perfInfo.missesWithStall.expect(1.U)

      // Stall false -> no counter change
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(true.B)
      dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
      dut.io.stallUpdate.bits.ftqIdx.value.poke(ftq)
      dut.io.stallUpdate.bits.stalled.poke(false.B)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(false.B)
      dut.io.perfInfo.missesWithStall.expect(1.U)

      // Stall true again -> +1 (current implementation)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(true.B)
      dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
      dut.io.stallUpdate.bits.ftqIdx.value.poke(ftq)
      dut.io.stallUpdate.bits.stalled.poke(true.B)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(false.B)

      dut.io.perfInfo.missesWithStall.expect(2.U)
    }
  }
  it should "expose one-per-cycle behavior when two candidates become FEC in same cycle" in {
    test(new FECTracker(numEntries = 16)) { dut =>
      // Init
      dut.io.fencei.poke(false.B)
      dut.io.newMiss.valid.poke(false.B)
      dut.io.stallUpdate.valid.poke(false.B)
      (0 until commitWidth).foreach(i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      )
      dut.clock.step(1)

      // Two different entries
      val ftqA = 21.U
      val ftqB = 22.U
      val addrA = 0x21000.U
      val addrB = 0x22000.U

      def miss(ftq: UInt, addr: UInt): Unit = {
        val addrScala: BigInt = addr.litValue
        val vset: BigInt = (addrScala >> 6) & 0xff // Mask to 8 bits for vSetIdx
        dut.io.newMiss.valid.poke(true.B)
        dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
        dut.io.newMiss.bits.ftqIdx.value.poke(ftq)
        dut.io.newMiss.bits.blkPaddr.poke(addr)
        dut.io.newMiss.bits.vSetIdx
          .poke(vset.U)
        dut.clock.step(1)
        dut.io.newMiss.valid.poke(false.B)
      }

      def stall(ftq: UInt): Unit = {
        dut.io.stallUpdate.valid.poke(true.B)
        dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
        dut.io.stallUpdate.bits.ftqIdx.value.poke(ftq)
        dut.io.stallUpdate.bits.stalled.poke(true.B)
        dut.clock.step(1)
        dut.io.stallUpdate.valid.poke(false.B)
      }

      // Miss + stall for both so both are "ready" once they retire
      miss(ftqA, addrA); stall(ftqA)
      miss(ftqB, addrB); stall(ftqB)

      // Retire both in same cycle (if commitWidth < 2, skip the second)
      require(commitWidth >= 2, "This test requires commitWidth >= 2")

      val before = dut.io.perfInfo.fecLinesDetected.peek().litValue

      dut.io.retireUpdate(0).valid.poke(true.B)
      dut.io.retireUpdate(0).bits.ftqIdx.flag.poke(false.B)
      dut.io.retireUpdate(0).bits.ftqIdx.value.poke(ftqA)

      dut.io.retireUpdate(1).valid.poke(true.B)
      dut.io.retireUpdate(1).bits.ftqIdx.flag.poke(false.B)
      dut.io.retireUpdate(1).bits.ftqIdx.value.poke(ftqB)

      dut.clock.step(1)

      dut.io.retireUpdate(0).valid.poke(false.B)
      dut.io.retireUpdate(1).valid.poke(false.B)

      // Exactly one output per cycle (visible behavior)
      dut.io.fecLine.valid.expect(true.B)

      val after = dut.io.perfInfo.fecLinesDetected.peek().litValue
      assert(
        after == before + 1,
        s"Expected fecLinesDetected to increment by 1, got before=$before after=$after"
      )

      // Next cycle fecLine should clear
      dut.clock.step(1)
      dut.io.fecLine.valid.expect(false.B)
    }
  }
  it should "not overflow on duplicate miss for same ftqIdx (numEntries=1)" in {
    test(new FECTracker(numEntries = 1)) { dut =>
      // Init
      dut.io.fencei.poke(false.B)
      dut.io.newMiss.valid.poke(false.B)
      dut.io.stallUpdate.valid.poke(false.B)
      (0 until commitWidth).foreach(i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      )
      dut.clock.step(1)

      val ftq1 = 1.U
      val ftq2 = 2.U

      // First miss takes the only slot
      dut.io.newMiss.valid.poke(true.B)
      dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
      dut.io.newMiss.bits.ftqIdx.value.poke(ftq1)
      dut.io.newMiss.bits.blkPaddr.poke(0x1000.U)
      dut.io.newMiss.bits.vSetIdx.poke(0x10.U)
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(false.B)

      dut.io.perfInfo.trackerFull.expect(0.U)

      // Duplicate miss for same ftqIdx should hit, not overflow
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(true.B)
      dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
      dut.io.newMiss.bits.ftqIdx.value.poke(ftq1)
      dut.io.newMiss.bits.blkPaddr.poke(0x1111.U)
      dut.io.newMiss.bits.vSetIdx.poke(0x11.U)
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(false.B)

      dut.io.perfInfo.trackerFull.expect(0.U) // key

      // New ftqIdx should overflow now
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(true.B)
      dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
      dut.io.newMiss.bits.ftqIdx.value.poke(ftq2)
      dut.io.newMiss.bits.blkPaddr.poke(0x2000.U)
      dut.io.newMiss.bits.vSetIdx.poke(0x20.U)
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(false.B)

      dut.io.perfInfo.trackerFull.expect(1.U)
    }
  }
  it should "use updated blkPaddr/vSetIdx from duplicate miss when detecting FEC" in {
    test(new FECTracker(numEntries = 4)) { dut =>
      // Init
      dut.io.fencei.poke(false.B)
      dut.io.newMiss.valid.poke(false.B)
      dut.io.stallUpdate.valid.poke(false.B)
      (0 until commitWidth).foreach(i =>
        dut.io.retireUpdate(i).valid.poke(false.B)
      )
      dut.clock.step(1)

      val ftq = 7.U
      val addrA = 0x3000.U
      val addrB = 0x3abc.U
      val setA = 0x30.U
      val setB = 0x3a.U

      // Miss A
      dut.io.newMiss.valid.poke(true.B)
      dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
      dut.io.newMiss.bits.ftqIdx.value.poke(ftq)
      dut.io.newMiss.bits.blkPaddr.poke(addrA)
      dut.io.newMiss.bits.vSetIdx.poke(setA)
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(false.B)

      // Duplicate miss B (should update tracked entry)
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(true.B)
      dut.io.newMiss.bits.ftqIdx.flag.poke(false.B)
      dut.io.newMiss.bits.ftqIdx.value.poke(ftq)
      dut.io.newMiss.bits.blkPaddr.poke(addrB)
      dut.io.newMiss.bits.vSetIdx.poke(setB)
      dut.clock.step(1)
      dut.io.newMiss.valid.poke(false.B)

      // Stall + retire -> should detect with UPDATED addrB/setB
      dut.io.stallUpdate.valid.poke(true.B)
      dut.io.stallUpdate.bits.ftqIdx.flag.poke(false.B)
      dut.io.stallUpdate.bits.ftqIdx.value.poke(ftq)
      dut.io.stallUpdate.bits.stalled.poke(true.B)
      dut.clock.step(1)
      dut.io.stallUpdate.valid.poke(false.B)

      dut.io.retireUpdate(0).valid.poke(true.B)
      dut.io.retireUpdate(0).bits.ftqIdx.flag.poke(false.B)
      dut.io.retireUpdate(0).bits.ftqIdx.value.poke(ftq)
      dut.clock.step(1)
      dut.io.retireUpdate(0).valid.poke(false.B)

      dut.io.fecLine.valid.expect(true.B)
      dut.io.fecLine.bits.blkPaddr.expect(addrB)
      dut.io.fecLine.bits.vSetIdx.expect(setB)
    }
  }
}

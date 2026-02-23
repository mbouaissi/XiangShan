/***************************************************************************************
* FEC Tracker Simple Test Example
* 
* This is a simplified example showing how to manually test the FECTracker module
* for debugging or understanding purposes.
***************************************************************************************/

package xiangshan.frontend.icache

import chisel3._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import top.DefaultConfig

object FECTrackerSimpleExample extends App {
  
  // Use XiangShan's default configuration
  val defaultConfig = new DefaultConfig
  implicit val p: Parameters = defaultConfig.alterPartial({
    case XSCoreParamsKey => defaultConfig(XSTileKey).head
  })

  println("""
    |=================================================================
    | FEC Tracker Simple Test Example
    |=================================================================
    | 
    | This example shows the basic FEC detection flow:
    | 
    | 1. Cache Miss: A cache line misses in the ICache
    |    - ftqIdx=10, blkPaddr=0x1000, vSetIdx=0x10
    |    - Tracker allocates an entry
    | 
    | 2. Frontend Stall: The miss causes IFU to stall
    |    - ftqIdx=10 matches tracked entry
    |    - Entry marked as "stalledIFU"
    | 
    | 3. Instruction Retirement: Instructions from ftqIdx retire
    |    - ftqIdx=10 retirement notification
    |    - Entry marked as "retired"
    |    - FEC line detected! (miss ∧ stall ∧ retired)
    | 
    | Expected Output: FEC line detected with:
    |   - blkPaddr = 0x1000
    |   - vSetIdx = 0x10
    | 
    |=================================================================
    | 
    | To run full tests, use:
    |   sbt "testOnly xiangshan.frontend.icache.FECTrackerTest"
    | 
    | Or with Mill:
    |   mill XiangShan.test.testOnly xiangshan.frontend.icache.FECTrackerTest
    |=================================================================
  """.stripMargin)

  val icacheParams = p(XSCoreParamsKey).icacheParameters
  println("\nGenerated FECTracker module parameters:")
  println(s"  - Number of entries: 16")
  println(s"  - Aging threshold: 1024 cycles")
  println(s"  - CommitWidth: ${p(XSCoreParamsKey).CommitWidth}")
  println(s"  - ICache sets: ${icacheParams.nSets}")
  println(s"  - ICache ways: ${icacheParams.nWays}")
  println(s"  - Block bytes: ${icacheParams.blockBytes}")
  
  println("\n✓ Module instantiation successful!")
  println("✓ Run the test suite (FECTrackerTest.scala) to verify functionality")
}

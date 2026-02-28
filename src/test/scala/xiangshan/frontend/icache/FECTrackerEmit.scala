package xiangshan.frontend.icache

import chisel3._
import chisel3.emitVerilog
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import top.DefaultConfig

/** Simple app to emit Verilog for FECTracker for manual coverage analysis */
object FECTrackerEmit extends App {
  val defaultConfig = new DefaultConfig
  implicit val p: Parameters = defaultConfig.alterPartial({
    case XSCoreParamsKey => defaultConfig(XSTileKey).head
  })

  println("Generating Verilog for FECTracker...")
  emitVerilog(
    new FECTracker(numEntries = 16),
    Array("--target-dir", "generated_sv")
  )
  println(s"Verilog generated in: generated_sv/FECTracker.sv")
}

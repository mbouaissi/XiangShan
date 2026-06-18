package xiangshan.cache

import chisel3._
import chisel3.experimental.ExtModule

// Xilinx ILA 6.2 blackbox — port names must match the Vivado-generated module exactly.
// Generate the IP with the block in xiangshan-fpga_26/27.tcl before synthesising.
class ila_prefetch_debug extends ExtModule {
  val clk    = IO(Input(Clock()))
  // prefetch quality signals
  val probe0 = IO(Input(UInt(1.W)))   // total_prefetch  : a prefetch fired this cycle
  val probe1 = IO(Input(UInt(1.W)))   // good_prefetch   : prefetch used before eviction
  val probe2 = IO(Input(UInt(1.W)))   // bad_prefetch    : prefetch evicted before use
  val probe3 = IO(Input(UInt(1.W)))   // late_hit_prefetch  : demand hit before prefetch arrived
  val probe4 = IO(Input(UInt(1.W)))   // late_miss_prefetch : demand missed, prefetch still in-flight
  val probe5 = IO(Input(UInt(2.W)))   // prefetch_hit    : # loads that hit a prefetched line (0-3)
  val probe6 = IO(Input(UInt(1.W)))   // pf_ctrl.enable  : prefetcher currently enabled by monitor
  // cache miss signals (1 bit per load unit, LoadPipelineWidth = 3)
  val probe7 = IO(Input(UInt(3.W)))   // ldu_miss_fire   : which load units sent a miss this cycle
}

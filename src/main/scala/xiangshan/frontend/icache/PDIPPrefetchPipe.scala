
package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.cache.mmu._
import xiangshan.frontend.ExceptionType
import xiangshan.backend.fu.PMPReqBundle

class PDIPPrefetchPipe(implicit p: Parameters) extends ICacheModule {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val flush  = Input(Bool())

    // PDIP provides virtual, block-aligned address
    val in = Flipped(DecoupledIO(UInt(VAddrBits.W)))

    // To missUnit as prefetch req
    val out = DecoupledIO(new ICacheMissReq)

    // iTLB port dedicated to PDIP prefetch
    val itlb = new TlbRequestIO(1)

    // PMP port dedicated to PDIP prefetch
    val pmp = new ICachePMPBundle
  })

  // ---------------------------
  // Defaults
  // ---------------------------
  io.out.valid := false.B
  io.out.bits  := 0.U.asTypeOf(new ICacheMissReq)

  io.in.ready := false.B

  io.itlb.req.valid := false.B
  io.itlb.req.bits  := 0.U.asTypeOf(new TlbReq)
  io.itlb.req_kill  := io.flush
  io.itlb.resp.ready := false.B

  io.pmp.req.valid := false.B
  io.pmp.req.bits  := 0.U.asTypeOf(new PMPReqBundle())
  // io.pmp.resp is Input — DO NOT DRIVE

  // ---------------------------
  // Simple FSM
  // ---------------------------
  private object State {
    val idle :: waitTlb :: emit :: Nil = Enum(3)
  }
  private val state = RegInit(State.idle)

  private val vaddrReg = RegInit(0.U(VAddrBits.W))
  private val paddrReg = RegInit(0.U(PAddrBits.W))
  private val pbmtReg  = RegInit(Pbmt.pma)

  private def isUncache(pbmt: UInt): Bool = Pbmt.isUncache(pbmt)

  // Convenience
  private val active = io.enable && !io.flush

  // ---------------------------
  // State machine
  // ---------------------------
  switch(state) {
    is(State.idle) {
      io.in.ready := active && io.itlb.req.ready
      when(io.in.fire) {
        vaddrReg := io.in.bits
        state := State.waitTlb
      }
    }

    is(State.waitTlb) {
      // Send iTLB req (Decoupled)
      io.itlb.req.valid := active
      io.itlb.req.bits.vaddr := vaddrReg
      io.itlb.req.bits.fullva := vaddrReg
      io.itlb.req.bits.checkfullva := false.B
      io.itlb.req.bits.cmd := TlbCmd.exec
      io.itlb.req.bits.hyperinst := false.B
      io.itlb.req.bits.hlvx := false.B
      io.itlb.req.bits.size := 3.U
      io.itlb.req.bits.kill := io.flush
      io.itlb.req.bits.memidx := 0.U.asTypeOf(new MemBlockidxBundle)
      io.itlb.req.bits.isPrefetch := true.B
      io.itlb.req.bits.no_translate := false.B
      io.itlb.req.bits.pmp_addr := 0.U // unused for normal translate path
      io.itlb.req.bits.debug.pc := 0.U
      io.itlb.req.bits.debug.robIdx := 0.U.asTypeOf(io.itlb.req.bits.debug.robIdx)
      io.itlb.req.bits.debug.isFirstIssue := false.B

      // Accept resp
      io.itlb.resp.ready := active

      when(io.itlb.resp.fire) {
        val r = io.itlb.resp.bits
        // If miss or instr PF/AF/etc, just drop this prefetch
        val hasExcp = r.miss ||
          r.excp(0).pf.instr ||
          r.excp(0).af.instr ||
          r.excp(0).gpf.instr
        when(hasExcp) {
          state := State.idle
        }.otherwise {
          paddrReg := r.paddr(0)
          pbmtReg  := r.pbmt(0)
          state := State.emit
        }
      }

      when(io.flush) {
        state := State.idle
      }
    }

    is(State.emit) {
      // PMP check looks combinational in mainPipe style:
      // drive req and read resp same cycle.
      io.pmp.req.valid := active
      io.pmp.req.bits.addr := paddrReg
      io.pmp.req.bits.size := 3.U
      io.pmp.req.bits.cmd  := TlbCmd.exec

      val pmpExcp = ExceptionType.fromPMPResp(io.pmp.resp)
      val pmpMmio = io.pmp.resp.mmio

      val drop = ExceptionType.hasException(pmpExcp) || pmpMmio || isUncache(pbmtReg)

      // Emit miss req if allowed
      io.out.valid := active && !drop
      io.out.bits.blkPaddr := getBlkAddr(paddrReg)
      io.out.bits.vSetIdx  := get_idx(vaddrReg) // same helper used in mainPipe

      when(io.out.fire || drop || io.flush) {
        state := State.idle
      }
    }
  }
}
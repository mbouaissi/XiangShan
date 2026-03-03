
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

    // PDIP provides prefetch entry with block address and virtual set index
    val in = Flipped(DecoupledIO(new PrefetchEntry))

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
  io.itlb.resp.ready := true.B  // non-blocking TLB port: must always be true

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

  private val vaddrReg    = RegInit(0.U(VAddrBits.W))
  private val vSetIdxReg  = RegInit(0.U(idxBits.W))
  // blkPaddrReg removed: the pre-TLB hint is not used; blkPaddr is derived from
  // the TLB-translated paddr directly in waitTlb.
  // paddrReg / pbmtReg removed: used combinationally from the ITLB response.

  private def isUncache(pbmt: UInt): Bool = Pbmt.isUncache(pbmt)

  // Convenience
  private val active = io.enable && !io.flush

  // ---------------------------
  // ITLB req helper — shared between idle (speculative) and waitResp (retry)
  // ---------------------------
  private def driveItlbReq(vaddr: UInt): Unit = {
    io.itlb.req.valid                    := active
    io.itlb.req.bits.vaddr               := vaddr
    io.itlb.req.bits.fullva              := vaddr
    io.itlb.req.bits.checkfullva         := false.B
    io.itlb.req.bits.cmd                 := TlbCmd.exec
    io.itlb.req.bits.hyperinst           := false.B
    io.itlb.req.bits.hlvx               := false.B
    io.itlb.req.bits.size                := 3.U
    io.itlb.req.bits.kill                := io.flush
    io.itlb.req.bits.memidx              := 0.U.asTypeOf(new MemBlockidxBundle)
    io.itlb.req.bits.isPrefetch          := true.B
    io.itlb.req.bits.no_translate        := false.B
    io.itlb.req.bits.pmp_addr            := 0.U
    io.itlb.req.bits.debug.pc            := 0.U
    io.itlb.req.bits.debug.robIdx        := 0.U.asTypeOf(io.itlb.req.bits.debug.robIdx)
    io.itlb.req.bits.debug.isFirstIssue  := false.B
  }

  // ---------------------------
  // 2-state FSM
  //
  //  idle      — accept input AND speculatively send ITLB req in the same cycle,
  //              saving the extra stall cycle of the old 3-state design.
  //  waitResp  — retry ITLB req until a non-miss response arrives, then do PMP
  //              combinationally and emit (or drop) in the same cycle.
  //              Dropping when the miss-unit is back-pressured is acceptable
  //              for a prefetcher, which avoids needing a separate emit state.
  // ---------------------------
  switch(state) {
    is(State.idle) {
      // Speculatively send the ITLB req using the incoming vaddr so we don't
      // waste a cycle latching before requesting.
      when(io.in.valid && active && io.itlb.req.ready) {
        driveItlbReq(io.in.bits.vaddr)
      }
      io.in.ready := active && io.itlb.req.ready
      when(io.in.fire) {
        vaddrReg   := io.in.bits.vaddr
        vSetIdxReg := io.in.bits.vSetIdx
        state      := State.waitTlb
      }
    }

    is(State.waitTlb) {
      // Keep driving ITLB req from the latched vaddr (retry on miss).
      driveItlbReq(vaddrReg)

      // resp.ready is wired true.B (non-blocking port); fire = valid.
      when(io.itlb.resp.fire) {
        val r = io.itlb.resp.bits
        val hasExcp =
          r.excp(0).pf.instr ||
          r.excp(0).af.instr ||
          r.excp(0).gpf.instr

        when(r.miss) {
          // TLB miss — stay here and retry next cycle
        }.elsewhen(hasExcp) {
          // Translate exception — silently drop this prefetch
          state := State.idle
        }.otherwise {
          // TLB hit: do PMP combinationally with the just-received paddr and
          // emit to the miss unit in the same cycle.
          val paddr = r.paddr(0)
          val pbmt  = r.pbmt(0)

          io.pmp.req.valid    := active
          io.pmp.req.bits.addr := paddr
          io.pmp.req.bits.size := 3.U
          io.pmp.req.bits.cmd  := TlbCmd.exec

          val pmpExcp = ExceptionType.fromPMPResp(io.pmp.resp)
          val drop    = ExceptionType.hasException(pmpExcp) ||
                        io.pmp.resp.mmio                    ||
                        isUncache(pbmt)

          // Emit — drop if miss unit is back-pressured (acceptable for prefetcher)
          io.out.valid          := active && !drop
          io.out.bits.blkPaddr  := paddr(PAddrBits - 1, blockOffBits)
          io.out.bits.vSetIdx   := vSetIdxReg

          // Always retire this entry: either it fired, was dropped by PMP/MMIO,
          // or the miss unit was full (prefetch dropped).
          state := State.idle
        }
      }

      when(io.flush) { state := State.idle }
    }

    // State.emit is now unused; kept in the Enum to avoid renumbering
    // but will be optimised away by the compiler.
    is(State.emit) { state := State.idle }
  }
}
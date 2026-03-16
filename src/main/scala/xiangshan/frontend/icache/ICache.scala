/** *************************************************************************************
  * Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC) Copyright
  * (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2. You can use this software
  * according to the terms and conditions of the Mulan PSL v2. You may obtain a
  * copy of Mulan PSL v2 at: http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY
  * KIND, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
  * NON-INFRINGEMENT, MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  *
  * Acknowledgement
  *
  * This implementation is inspired by several key papers: [1] Glenn Reinman,
  * Brad Calder, and Todd Austin. "[Fetch directed instruction prefetching.]
  * (https://doi.org/10.1109/MICRO.1999.809439)" 32nd Annual ACM/IEEE
  * International Symposium on Microarchitecture (MICRO). 1999.
  */

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.diplomacy.IdRange
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleFieldBase
import huancun.AliasField
import huancun.PrefetchField
import org.chipsalliance.cde.config.Parameters
import utility._
import utility.mbist.MbistPipeline
import utility.sram.SplittedSRAMTemplate
import utility.sram.SRAMReadBus
import utility.sram.SRAMTemplate
import utility.sram.SRAMWriteBus
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.frontend._
import xiangshan.backend.fu.PMPReqBundle
import xiangshan.cache.mmu.TlbReq

case class ICacheParameters(
    nSets: Int = 256,
    nWays: Int = 4,
    rowBits: Int = 64,
    nTLBEntries: Int = 32,
    tagECC: Option[String] = None,
    dataECC: Option[String] = None,
    replacer: Option[String] = Some("random"),
    PortNumber: Int = 2,
    nFetchMshr: Int = 4,
    nPrefetchMshr: Int = 10,
    nWayLookupSize: Int = 32,
    DataCodeUnit: Int = 64,
    ICacheDataBanks: Int = 8,
    ICacheDataSRAMWidth: Int = 66,
    // TODO: hard code, need delete
    partWayNum: Int = 4,
    nMMIOs: Int = 1,
    blockBytes: Int = 64,
    cacheCtrlAddressOpt: Option[AddressSet] = None,
    // PDIP parameters
    pdipParams: PDIPParams = PDIPParams()
) extends L1CacheParameters {

  val setBytes: Int = nSets * blockBytes
  val aliasBitsOpt: Option[Int] =
    Option.when(setBytes > pageSize)(log2Ceil(setBytes / pageSize))
  val reqFields: Seq[BundleFieldBase] = Seq(
    PrefetchField(),
    ReqSourceField()
  ) ++ aliasBitsOpt.map(AliasField)
  val echoFields: Seq[BundleFieldBase] = Nil
  def tagCode: Code = Code.fromString(tagECC)
  def dataCode: Code = Code.fromString(dataECC)
  def replacement = ReplacementPolicy.fromString(replacer, nWays, nSets)
}

trait HasICacheParameters
    extends HasL1CacheParameters
    with HasInstrMMIOConst
    with HasIFUConst {
  val cacheParams: ICacheParameters = icacheParameters

  def ctrlUnitParamsOpt: Option[L1ICacheCtrlParams] = OptionWrapper(
    cacheParams.cacheCtrlAddressOpt.nonEmpty,
    L1ICacheCtrlParams(
      address = cacheParams.cacheCtrlAddressOpt.get,
      regWidth = XLEN
    )
  )

  def ICacheSets: Int = cacheParams.nSets
  def ICacheWays: Int = cacheParams.nWays
  def PortNumber: Int = cacheParams.PortNumber
  def nFetchMshr: Int = cacheParams.nFetchMshr
  def nPrefetchMshr: Int = cacheParams.nPrefetchMshr
  def nWayLookupSize: Int = cacheParams.nWayLookupSize
  def DataCodeUnit: Int = cacheParams.DataCodeUnit
  def ICacheDataBanks: Int = cacheParams.ICacheDataBanks
  def ICacheDataSRAMWidth: Int = cacheParams.ICacheDataSRAMWidth
  def partWayNum: Int = cacheParams.partWayNum

  def ICacheMetaBits: Int =
    tagBits // FIXME: unportable: maybe use somemethod to get width
  def ICacheMetaCodeBits: Int =
    1 // FIXME: unportable: maybe use cacheParams.tagCode.somemethod to get width
  def ICacheMetaEntryBits: Int = ICacheMetaBits + ICacheMetaCodeBits

  def ICacheDataBits: Int = blockBits / ICacheDataBanks
  def ICacheDataCodeSegs: Int =
    math
      .ceil(ICacheDataBits / DataCodeUnit)
      .toInt // split data to segments for ECC checking
  def ICacheDataCodeBits: Int =
    ICacheDataCodeSegs * 1 // FIXME: unportable: maybe use cacheParams.dataCode.somemethod to get width
  def ICacheDataEntryBits: Int = ICacheDataBits + ICacheDataCodeBits
  def ICacheBankVisitNum: Int = 32 * 8 / ICacheDataBits + 1
  def highestIdxBit: Int = log2Ceil(nSets) - 1

  require((ICacheDataBanks >= 2) && isPow2(ICacheDataBanks))
  require(ICacheDataSRAMWidth >= ICacheDataEntryBits)
  require(isPow2(ICacheSets), s"nSets($ICacheSets) must be pow2")
  require(isPow2(ICacheWays), s"nWays($ICacheWays) must be pow2")

  def generatePipeControl(
      lastFire: Bool,
      thisFire: Bool,
      thisFlush: Bool,
      lastFlush: Bool
  ): Bool = {
    val valid = RegInit(false.B)
    when(thisFlush)(valid := false.B)
      .elsewhen(lastFire && !lastFlush)(valid := true.B)
      .elsewhen(thisFire)(valid := false.B)
    valid
  }

  def ResultHoldBypass[T <: Data](data: T, valid: Bool): T =
    Mux(valid, data, RegEnable(data, valid))

  def ResultHoldBypass[T <: Data](data: T, init: T, valid: Bool): T =
    Mux(valid, data, RegEnable(data, init, valid))

  def holdReleaseLatch(valid: Bool, release: Bool, flush: Bool): Bool = {
    val bit = RegInit(false.B)
    when(flush)(bit := false.B)
      .elsewhen(valid && !release)(bit := true.B)
      .elsewhen(release)(bit := false.B)
    bit || valid
  }

  def blockCounter(block: Bool, flush: Bool, threshold: Int): Bool = {
    val counter = RegInit(0.U(log2Up(threshold + 1).W))
    when(block)(counter := counter + 1.U)
    when(flush)(counter := 0.U)
    counter > threshold.U
  }

  def InitQueue[T <: Data](entry: T, size: Int): Vec[T] =
    RegInit(VecInit(Seq.fill(size)(0.U.asTypeOf(entry.cloneType))))

  def getBankSel(blkOffset: UInt, valid: Bool = true.B): Vec[UInt] = {
    val bankIdxLow = (Cat(0.U(1.W), blkOffset) >> log2Ceil(
      blockBytes / ICacheDataBanks
    )).asUInt
    val bankIdxHigh = ((Cat(0.U(1.W), blkOffset) + 32.U) >> log2Ceil(
      blockBytes / ICacheDataBanks
    )).asUInt
    val bankSel = VecInit(
      (0 until ICacheDataBanks * 2).map(i =>
        (i.U >= bankIdxLow) && (i.U <= bankIdxHigh)
      )
    )
    assert(
      !valid || PopCount(bankSel) === ICacheBankVisitNum.U,
      "The number of bank visits must be %d, but bankSel=0x%x",
      ICacheBankVisitNum.U,
      bankSel.asUInt
    )
    bankSel
      .asTypeOf(UInt((ICacheDataBanks * 2).W))
      .asTypeOf(Vec(2, UInt(ICacheDataBanks.W)))
  }

  def getLineSel(blkOffset: UInt): Vec[Bool] = {
    val bankIdxLow =
      (blkOffset >> log2Ceil(blockBytes / ICacheDataBanks)).asUInt
    val lineSel = VecInit((0 until ICacheDataBanks).map(i => i.U < bankIdxLow))
    lineSel
  }

  def getBlkAddr(addr: UInt): UInt = (addr >> blockOffBits).asUInt
  def getPhyTagFromBlk(addr: UInt): UInt =
    (addr >> (pgUntagBits - blockOffBits)).asUInt
  def getIdxFromBlk(addr: UInt): UInt = addr(idxBits - 1, 0)
  def getPaddrFromPtag(vaddr: UInt, ptag: UInt): UInt =
    Cat(ptag, vaddr(pgUntagBits - 1, 0))
  def getPaddrFromPtag(vaddrVec: Vec[UInt], ptagVec: Vec[UInt]): Vec[UInt] =
    VecInit((vaddrVec zip ptagVec).map { case (vaddr, ptag) =>
      getPaddrFromPtag(vaddr, ptag)
    })
}

trait HasICacheECCHelper extends HasICacheParameters {
  def encodeMetaECC(meta: UInt, poison: Bool = false.B): UInt = {
    require(meta.getWidth == ICacheMetaBits)
    val code = cacheParams.tagCode.encode(meta, poison) >> ICacheMetaBits
    code.asTypeOf(UInt(ICacheMetaCodeBits.W))
  }

  def encodeDataECC(data: UInt, poison: Bool = false.B): UInt = {
    require(data.getWidth == ICacheDataBits)
    val datas = data.asTypeOf(
      Vec(ICacheDataCodeSegs, UInt((ICacheDataBits / ICacheDataCodeSegs).W))
    )
    val codes = VecInit(
      datas.map(
        cacheParams.dataCode
          .encode(_, poison) >> (ICacheDataBits / ICacheDataCodeSegs)
      )
    )
    codes.asTypeOf(UInt(ICacheDataCodeBits.W))
  }
}

abstract class ICacheBundle(implicit p: Parameters)
    extends XSBundle
    with HasICacheParameters

abstract class ICacheModule(implicit p: Parameters)
    extends XSModule
    with HasICacheParameters

abstract class ICacheArray(implicit p: Parameters)
    extends XSModule
    with HasICacheParameters

class ICacheMetadata(implicit p: Parameters) extends ICacheBundle {
  val tag: UInt = UInt(tagBits.W)
}

object ICacheMetadata {
  def apply(tag: Bits)(implicit p: Parameters): ICacheMetadata = {
    val meta = Wire(new ICacheMetadata)
    meta.tag := tag
    meta
  }
}

class ICacheMetaArrayIO(implicit p: Parameters) extends ICacheBundle {
  val write: DecoupledIO[ICacheMetaWriteBundle] = Flipped(
    DecoupledIO(new ICacheMetaWriteBundle)
  )
  val read: DecoupledIO[ICacheReadBundle] = Flipped(
    DecoupledIO(new ICacheReadBundle)
  )
  val readResp: ICacheMetaRespBundle = Output(new ICacheMetaRespBundle)
  val flush: Vec[Valid[ICacheMetaFlushBundle]] =
    Vec(PortNumber, Flipped(ValidIO(new ICacheMetaFlushBundle)))
  val flushAll: Bool = Input(Bool())
}

class ICacheMetaArray(implicit p: Parameters)
    extends ICacheArray
    with HasICacheECCHelper {
  class ICacheMetaEntry(implicit p: Parameters) extends ICacheBundle {
    val meta: ICacheMetadata = new ICacheMetadata
    val code: UInt = UInt(ICacheMetaCodeBits.W)
  }

  private object ICacheMetaEntry {
    def apply(meta: ICacheMetadata, poison: Bool)(implicit
        p: Parameters
    ): ICacheMetaEntry = {
      val entry = Wire(new ICacheMetaEntry)
      entry.meta := meta
      entry.code := encodeMetaECC(meta.asUInt, poison)
      entry
    }
  }

  // sanity check
  require(ICacheMetaEntryBits == (new ICacheMetaEntry).getWidth)

  val io: ICacheMetaArrayIO = IO(new ICacheMetaArrayIO)

  private val port_0_read_0 = io.read.valid && !io.read.bits.vSetIdx(0)(0)
  private val port_0_read_1 = io.read.valid && io.read.bits.vSetIdx(0)(0)
  private val port_1_read_1 =
    io.read.valid && io.read.bits.vSetIdx(1)(0) && io.read.bits.isDoubleLine
  private val port_1_read_0 =
    io.read.valid && !io.read.bits.vSetIdx(1)(0) && io.read.bits.isDoubleLine

  private val port_0_read_0_reg =
    RegEnable(port_0_read_0, 0.U.asTypeOf(port_0_read_0), io.read.fire)
  private val port_0_read_1_reg =
    RegEnable(port_0_read_1, 0.U.asTypeOf(port_0_read_1), io.read.fire)
  private val port_1_read_1_reg =
    RegEnable(port_1_read_1, 0.U.asTypeOf(port_1_read_1), io.read.fire)
  private val port_1_read_0_reg =
    RegEnable(port_1_read_0, 0.U.asTypeOf(port_1_read_0), io.read.fire)

  private val bank_0_idx =
    Mux(port_0_read_0, io.read.bits.vSetIdx(0), io.read.bits.vSetIdx(1))
  private val bank_1_idx =
    Mux(port_0_read_1, io.read.bits.vSetIdx(0), io.read.bits.vSetIdx(1))

  private val write_bank_0 = io.write.valid && !io.write.bits.bankIdx
  private val write_bank_1 = io.write.valid && io.write.bits.bankIdx

  private val write_meta_bits = ICacheMetaEntry(
    meta = ICacheMetadata(
      tag = io.write.bits.phyTag
    ),
    poison = io.write.bits.poison
  )


  private val tagArrays = (0 until PortNumber) map { bank =>
    val tagArray = Module(
      new SplittedSRAMTemplate(
        new ICacheMetaEntry(),
        set = nSets / PortNumber,
        way = nWays,
        waySplit = 2,
        dataSplit = 1,
        shouldReset = true,
        holdRead = true,
        singlePort = true,
        withClockGate = true,
        hasMbist = hasMbist
      )
    )

    // meta connection
    if (bank == 0) {
      tagArray.io.r.req.valid := port_0_read_0 || port_1_read_0
      tagArray.io.r.req.bits.apply(setIdx = bank_0_idx(highestIdxBit, 1))
      tagArray.io.w.req.valid := write_bank_0
      tagArray.io.w.req.bits.apply(
        data = write_meta_bits,
        setIdx = io.write.bits.virIdx(highestIdxBit, 1),
        waymask = io.write.bits.waymask
      )
    } else {
      tagArray.io.r.req.valid := port_0_read_1 || port_1_read_1
      tagArray.io.r.req.bits.apply(setIdx = bank_1_idx(highestIdxBit, 1))
      tagArray.io.w.req.valid := write_bank_1
      tagArray.io.w.req.bits.apply(
        data = write_meta_bits,
        setIdx = io.write.bits.virIdx(highestIdxBit, 1),
        waymask = io.write.bits.waymask
      )
    }

    tagArray
  }
  private val mbistPl =
    MbistPipeline.PlaceMbistPipeline(1, "MbistPipeIcacheTag", hasMbist)

  private val read_set_idx_next = RegEnable(
    io.read.bits.vSetIdx,
    0.U.asTypeOf(io.read.bits.vSetIdx),
    io.read.fire
  )
  private val valid_array = RegInit(VecInit(Seq.fill(nWays)(0.U(nSets.W))))
  private val valid_metas = Wire(Vec(PortNumber, Vec(nWays, Bool())))
  // valid read
  (0 until PortNumber).foreach(i =>
    (0 until nWays).foreach(way =>
      valid_metas(i)(way) := valid_array(way)(read_set_idx_next(i))
    )
  )
  io.readResp.entryValid := valid_metas

  io.read.ready := !io.write.valid && !io.flush
    .map(_.valid)
    .reduce(_ || _) && !io.flushAll &&
    tagArrays.map(_.io.r.req.ready).reduce(_ && _)

  // valid write
  private val way_num = OHToUInt(io.write.bits.waymask)
  when(io.write.valid) {
    valid_array(way_num) := valid_array(way_num).bitSet(
      io.write.bits.virIdx,
      true.B
    )
  }

  XSPerfAccumulate("meta_refill_num", io.write.valid)

  io.readResp.metas <> DontCare
  io.readResp.codes <> DontCare
  private val readMetaEntries = tagArrays.map(port =>
    port.io.r.resp.asTypeOf(Vec(nWays, new ICacheMetaEntry()))
  )
  private val readMetas = readMetaEntries.map(_.map(_.meta))
  private val readCodes = readMetaEntries.map(_.map(_.code))

  // TEST: force ECC to fail by setting readCodes to 0
  if (ICacheForceMetaECCError) {
    readCodes.foreach(_.foreach(_ := 0.U))
  }

  when(port_0_read_0_reg) {
    io.readResp.metas(0) := readMetas(0)
    io.readResp.codes(0) := readCodes(0)
  }.elsewhen(port_0_read_1_reg) {
    io.readResp.metas(0) := readMetas(1)
    io.readResp.codes(0) := readCodes(1)
  }

  when(port_1_read_0_reg) {
    io.readResp.metas(1) := readMetas(0)
    io.readResp.codes(1) := readCodes(0)
  }.elsewhen(port_1_read_1_reg) {
    io.readResp.metas(1) := readMetas(1)
    io.readResp.codes(1) := readCodes(1)
  }

  io.write.ready := true.B // TODO : has bug ? should be !io.cacheOp.req.valid

  /*
   * flush logic
   */
  // flush standalone set (e.g. flushed by mainPipe before doing re-fetch)
  when(io.flush.map(_.valid).reduce(_ || _)) {
    (0 until nWays).foreach { w =>
      valid_array(w) := (0 until PortNumber)
        .map { i =>
          Mux(
            // check if set `virIdx` in way `w` is requested to be flushed by port `i`
            io.flush(i).valid && io.flush(i).bits.waymask(w),
            valid_array(w).bitSet(io.flush(i).bits.virIdx, false.B),
            valid_array(w)
          )
        }
        .reduce(_ & _)
    }
  }

  // flush all (e.g. fence.i)
  when(io.flushAll) {
    (0 until nWays).foreach(w => valid_array(w) := 0.U)
  }

  // PERF: flush counter
  XSPerfAccumulate("flush", io.flush.map(_.valid).reduce(_ || _))
  XSPerfAccumulate("flush_all", io.flushAll)
}

class ICacheDataArrayIO(implicit p: Parameters) extends ICacheBundle {
  val write: DecoupledIO[ICacheDataWriteBundle] = Flipped(
    DecoupledIO(new ICacheDataWriteBundle)
  )
  val read: Vec[DecoupledIO[ICacheReadBundle]] = Flipped(
    Vec(partWayNum, DecoupledIO(new ICacheReadBundle))
  )
  val readResp: ICacheDataRespBundle = Output(new ICacheDataRespBundle)
}

class ICacheDataArray(implicit p: Parameters)
    extends ICacheArray
    with HasICacheECCHelper {
  class ICacheDataEntry(implicit p: Parameters) extends ICacheBundle {
    val data: UInt = UInt(ICacheDataBits.W)
    val code: UInt = UInt(ICacheDataCodeBits.W)
  }

  private object ICacheDataEntry {
    def apply(data: UInt, poison: Bool)(implicit
        p: Parameters
    ): ICacheDataEntry = {
      val entry = Wire(new ICacheDataEntry)
      entry.data := data
      entry.code := encodeDataECC(data, poison)
      entry
    }
  }

  val io: ICacheDataArrayIO = IO(new ICacheDataArrayIO)

  /** data array
    */
  private val writeDatas =
    io.write.bits.data.asTypeOf(Vec(ICacheDataBanks, UInt(ICacheDataBits.W)))
  private val writeEntries =
    writeDatas.map(ICacheDataEntry(_, io.write.bits.poison).asUInt)

  // io.read() are copies to control fan-out, we can simply use .head here
  private val bankSel =
    getBankSel(io.read.head.bits.blkOffset, io.read.head.valid)
  private val lineSel = getLineSel(io.read.head.bits.blkOffset)
  private val waymasks = io.read.head.bits.waymask
  private val masks = Wire(Vec(nWays, Vec(ICacheDataBanks, Bool())))
  (0 until nWays).foreach { way =>
    (0 until ICacheDataBanks).foreach { bank =>
      masks(way)(bank) := Mux(
        lineSel(bank),
        waymasks(1)(way) && bankSel(1)(bank).asBool,
        waymasks(0)(way) && bankSel(0)(bank).asBool
      )
    }
  }

  private val dataArrays = (0 until nWays).map { way =>
    val banks = (0 until ICacheDataBanks).map { bank =>
      val sramBank = Module(
        new SRAMTemplateWithFixedWidth(
          UInt(ICacheDataEntryBits.W),
          set = nSets,
          width = ICacheDataSRAMWidth,
          shouldReset = true,
          holdRead = true,
          singlePort = true,
          withClockGate = false, // enable signal timing is bad, no gating here
          hasMbist = hasMbist
        )
      )

      // read
      sramBank.io.r.req.valid := io.read(bank % 4).valid && masks(way)(bank)
      sramBank.io.r.req.bits.apply(setIdx =
        Mux(
          lineSel(bank),
          io.read(bank % 4).bits.vSetIdx(1),
          io.read(bank % 4).bits.vSetIdx(0)
        )
      )
      // write
      sramBank.io.w.req.valid := io.write.valid && io.write.bits
        .waymask(way)
        .asBool
      sramBank.io.w.req.bits.apply(
        data = writeEntries(bank),
        setIdx = io.write.bits.virIdx,
        // waymask is invalid when way of SRAMTemplate <= 1
        waymask = 0.U
      )
      sramBank
    }
    MbistPipeline.PlaceMbistPipeline(
      1,
      s"MbistPipeIcacheDataWay${way}",
      hasMbist
    )
    banks
  }

  /** read logic
    */
  private val masksReg = RegEnable(masks, 0.U.asTypeOf(masks), io.read(0).valid)
  private val readDataWithCode = (0 until ICacheDataBanks).map { bank =>
    Mux1H(
      VecInit(masksReg.map(_(bank))).asTypeOf(UInt(nWays.W)),
      dataArrays.map(_(bank).io.r.resp.asUInt)
    )
  }
  private val readEntries =
    readDataWithCode.map(_.asTypeOf(new ICacheDataEntry()))
  private val readDatas = VecInit(readEntries.map(_.data))
  private val readCodes = VecInit(readEntries.map(_.code))

  // TEST: force ECC to fail by setting readCodes to 0
  if (ICacheForceDataECCError) {
    readCodes.foreach(_ := 0.U)
  }

  /** IO
    */
  io.readResp.datas := readDatas
  io.readResp.codes := readCodes
  io.write.ready := true.B
  io.read.foreach(_.ready := !io.write.valid)
}

class ICacheReplacerIO(implicit p: Parameters) extends ICacheBundle {
  val touch: Vec[Valid[ReplacerTouch]] =
    Vec(PortNumber, Flipped(ValidIO(new ReplacerTouch)))
  val victim: ReplacerVictim = Flipped(new ReplacerVictim)
}

class ICacheReplacer(implicit p: Parameters) extends ICacheModule {
  val io: ICacheReplacerIO = IO(new ICacheReplacerIO)

  private val replacers =
    Seq.fill(PortNumber)(
      ReplacementPolicy.fromString(
        cacheParams.replacer,
        nWays,
        nSets / PortNumber
      )
    )

  // touch
  private val touch_sets = Seq.fill(PortNumber)(
    Wire(Vec(PortNumber, UInt(log2Ceil(nSets / PortNumber).W)))
  )
  private val touch_ways =
    Seq.fill(PortNumber)(Wire(Vec(PortNumber, Valid(UInt(wayBits.W)))))
  (0 until PortNumber).foreach { i =>
    touch_sets(i)(0) := Mux(
      io.touch(i).bits.vSetIdx(0),
      io.touch(1).bits.vSetIdx(highestIdxBit, 1),
      io.touch(0).bits.vSetIdx(highestIdxBit, 1)
    )
    touch_ways(i)(0).bits := Mux(
      io.touch(i).bits.vSetIdx(0),
      io.touch(1).bits.way,
      io.touch(0).bits.way
    )
    touch_ways(i)(0).valid := Mux(
      io.touch(i).bits.vSetIdx(0),
      io.touch(1).valid,
      io.touch(0).valid
    )
  }

  // victim
  io.victim.way := Mux(
    io.victim.vSetIdx.bits(0),
    replacers(1).way(io.victim.vSetIdx.bits(highestIdxBit, 1)),
    replacers(0).way(io.victim.vSetIdx.bits(highestIdxBit, 1))
  )

  // touch the victim in next cycle
  private val victim_vSetIdx_reg =
    RegEnable(
      io.victim.vSetIdx.bits,
      0.U.asTypeOf(io.victim.vSetIdx.bits),
      io.victim.vSetIdx.valid
    )
  private val victim_way_reg = RegEnable(
    io.victim.way,
    0.U.asTypeOf(io.victim.way),
    io.victim.vSetIdx.valid
  )
  (0 until PortNumber).foreach { i =>
    touch_sets(i)(1) := victim_vSetIdx_reg(highestIdxBit, 1)
    touch_ways(i)(1).bits := victim_way_reg
    touch_ways(i)(1).valid := RegNext(
      io.victim.vSetIdx.valid
    ) && (victim_vSetIdx_reg(0) === i.U)
  }

  ((replacers zip touch_sets) zip touch_ways).foreach { case ((r, s), w) =>
    r.access(s, w)
  }
}

class ICacheIO(implicit p: Parameters) extends ICacheBundle {
  val hartId: UInt = Input(UInt(hartIdLen.W))
  // FTQ
  val fetch: ICacheMainPipeBundle = new ICacheMainPipeBundle
  val ftqPrefetch: FtqToPrefetchIO = Flipped(new FtqToPrefetchIO)
  // memblock
  val softPrefetch: Vec[Valid[SoftIfetchPrefetchBundle]] =
    Vec(backendParams.LduCnt, Flipped(Valid(new SoftIfetchPrefetchBundle)))
  // IFU - ROB commits for FEC tracking
  val rob_commits: Vec[Valid[RobCommitInfo]] = Input(
    Vec(CommitWidth, Valid(new RobCommitInfo))
  )
  // IFU
  val stop: Bool = Input(Bool())
  val toIFU: Bool = Output(Bool())
  // PMP: mainPipe & prefetchPipe need PortNumber each
  val pmp: Vec[ICachePMPBundle] = Vec(2 * PortNumber, new ICachePMPBundle)
  // iTLB
  val itlb: Vec[TlbRequestIO] = Vec(PortNumber, new TlbRequestIO)
  val itlbFlushPipe: Bool = Bool()
  // backend/BEU
  val error: Valid[L1CacheErrorInfo] = ValidIO(new L1CacheErrorInfo)
  // backend/CSR
  val csr_pf_enable: Bool = Input(Bool())
  // flush
  val fencei: Bool = Input(Bool())
  val flush: Bool = Input(Bool())

  // perf
  val perfInfo: ICachePerfInfo = Output(new ICachePerfInfo)
}

class ICache()(implicit p: Parameters)
    extends LazyModule
    with HasICacheParameters {
  override def shouldBeInlined: Boolean = false

  val clientParameters: TLMasterPortParameters = TLMasterPortParameters.v1(
    Seq(
      TLMasterParameters.v1(
        name = "icache",
        sourceId =
          IdRange(0, cacheParams.nFetchMshr + cacheParams.nPrefetchMshr + 1)
      )
    ),
    requestFields = cacheParams.reqFields,
    echoFields = cacheParams.echoFields
  )

  val clientNode: TLClientNode = TLClientNode(Seq(clientParameters))

  val ctrlUnitOpt: Option[ICacheCtrlUnit] =
    ctrlUnitParamsOpt.map(params => LazyModule(new ICacheCtrlUnit(params)))

  lazy val module: ICacheImp = new ICacheImp(this)
}

class ICacheImp(outer: ICache)
    extends LazyModuleImp(outer)
    with HasICacheParameters
    with HasPerfEvents {
  val io: ICacheIO = IO(new ICacheIO)

  println("ICache:")
  println("  TagECC: " + cacheParams.tagECC)
  println("  DataECC: " + cacheParams.dataECC)
  println("  ICacheSets: " + cacheParams.nSets)
  println("  ICacheWays: " + cacheParams.nWays)
  println("  PortNumber: " + cacheParams.PortNumber)
  println("  nFetchMshr: " + cacheParams.nFetchMshr)
  println("  nPrefetchMshr: " + cacheParams.nPrefetchMshr)
  println("  nWayLookupSize: " + cacheParams.nWayLookupSize)
  println("  DataCodeUnit: " + cacheParams.DataCodeUnit)
  println("  ICacheDataBanks: " + cacheParams.ICacheDataBanks)
  println("  ICacheDataSRAMWidth: " + cacheParams.ICacheDataSRAMWidth)

  val (bus, edge) = outer.clientNode.out.head

  private val metaArray = Module(new ICacheMetaArray)
  private val dataArray = Module(new ICacheDataArray)
  private val mainPipe = Module(new ICacheMainPipe)
  private val missUnit = Module(new ICacheMissUnit(edge))
  private val replacer = Module(new ICacheReplacer)
  private val prefetcher = Module(new IPrefetchPipe)
  private val wayLookup = Module(new WayLookup)
  private val fecTracker = Module(new FECTracker(numEntries = 32))  // Increased from 16 to reduce "tracker full" events
  private val pdipController = Module(
    new PDIPController(cacheParams.pdipParams)
  )

  println("  PDIP Enabled: " + cacheParams.pdipParams.enabled)
  if (cacheParams.pdipParams.enabled) {
    println("  PDIP Table Sets: " + cacheParams.pdipParams.numTableSets)
    println("  PDIP Ways Per Set: " + cacheParams.pdipParams.numWaysPerSet)
    println(
      "  PDIP Prefetch Queue Size: " + cacheParams.pdipParams.prefetchQueueSize
    )
  }

  private val ecc_enable =
    if (outer.ctrlUnitOpt.nonEmpty) outer.ctrlUnitOpt.get.module.io.ecc_enable
    else true.B

  // Miss write buffer with tag de-dup for prefetch fills (used when no ctrlUnit)
  private val missWriteBufValid = RegInit(false.B)
  private val missWriteBufNeedDedup = RegInit(false.B)
  private val missMetaBuf = Reg(new ICacheMetaWriteBundle)
  private val missDataBuf = Reg(new ICacheDataWriteBundle)
  private val missWaymaskBuf = RegInit(0.U(nWays.W))

  // dataArray io
  if (outer.ctrlUnitOpt.nonEmpty) {
    val ctrlUnit = outer.ctrlUnitOpt.get.module
    // Use Wires to help Chisel properly initialize all signal paths
    val data_write_valid = Wire(Bool())
    val data_write_bits = Wire(new ICacheDataWriteBundle())

    data_write_valid := Mux(
      ctrlUnit.io.injecting,
      ctrlUnit.io.dataWrite.valid,
      missUnit.io.data_write.valid
    )
    data_write_bits := Mux(
      ctrlUnit.io.injecting,
      ctrlUnit.io.dataWrite.bits,
      missUnit.io.data_write.bits
    )
    dataArray.io.write.valid := data_write_valid
    dataArray.io.write.bits := data_write_bits

    ctrlUnit.io.dataWrite.ready := Mux(
      ctrlUnit.io.injecting,
      dataArray.io.write.ready,
      false.B
    )
    missUnit.io.data_write.ready := Mux(
      ctrlUnit.io.injecting,
      false.B,
      dataArray.io.write.ready
    )
  } else {
    val missWriteInFire = missUnit.io.meta_write.valid && missUnit.io.data_write.valid && !missWriteBufValid
    when(missWriteInFire) {
      missMetaBuf := missUnit.io.meta_write.bits
      missDataBuf := missUnit.io.data_write.bits
      missWaymaskBuf := missUnit.io.meta_write.bits.waymask
      missWriteBufValid := true.B
      missWriteBufNeedDedup := true.B
    }

    missUnit.io.meta_write.ready := !missWriteBufValid
    missUnit.io.data_write.ready := !missWriteBufValid

    // Use signal-level assignments instead of <> to maintain consistency
    dataArray.io.write.valid := missWriteBufValid && !missWriteBufNeedDedup
    dataArray.io.write.bits := missDataBuf
    dataArray.io.write.bits.waymask := missWaymaskBuf
  }
  dataArray.io.read <> mainPipe.io.dataArray.toIData
  mainPipe.io.dataArray.fromIData := dataArray.io.readResp

  // metaArray io
  metaArray.io.flushAll := io.fencei
  val metaArrayFlush = Wire(Vec(PortNumber, ValidIO(new ICacheMetaFlushBundle)))
  metaArrayFlush := mainPipe.io.metaArrayFlush
  metaArray.io.flush <> metaArrayFlush

  // metaArray read path (IPrefetchPipe reads meta; ctrlUnit overrides when injecting)
  if (outer.ctrlUnitOpt.nonEmpty) {
    val ctrlUnit = outer.ctrlUnitOpt.get.module
    // Use Wires for write connections
    val meta_write_valid = Wire(Bool())
    val meta_write_bits = Wire(new ICacheMetaWriteBundle())

    meta_write_valid := Mux(
      ctrlUnit.io.injecting,
      ctrlUnit.io.metaWrite.valid,
      missUnit.io.meta_write.valid
    )
    meta_write_bits := Mux(
      ctrlUnit.io.injecting,
      ctrlUnit.io.metaWrite.bits,
      missUnit.io.meta_write.bits
    )
    metaArray.io.write.valid := meta_write_valid
    metaArray.io.write.bits := meta_write_bits

    ctrlUnit.io.metaWrite.ready := Mux(
      ctrlUnit.io.injecting,
      metaArray.io.write.ready,
      false.B
    )
    missUnit.io.meta_write.ready := Mux(
      ctrlUnit.io.injecting,
      false.B,
      metaArray.io.write.ready
    )

    when(ctrlUnit.io.injecting) {
      metaArray.io.read <> ctrlUnit.io.metaRead
      prefetcher.io.metaRead.toIMeta.ready := false.B
    }.otherwise {
      ctrlUnit.io.metaRead.ready := false.B
      metaArray.io.read <> prefetcher.io.metaRead.toIMeta
    }
    ctrlUnit.io.metaReadResp := metaArray.io.readResp
  } else {
    metaArray.io.read <> prefetcher.io.metaRead.toIMeta
    metaArray.io.write.valid := missWriteBufValid && !missWriteBufNeedDedup
    metaArray.io.write.bits := missMetaBuf
    metaArray.io.write.bits.waymask := missWaymaskBuf
  }
  prefetcher.io.metaRead.fromIMeta := metaArray.io.readResp

  // IPrefetchPipe: FTQ-driven prefetch — does TLB + meta lookup, writes WayLookup
  prefetcher.io.flush := io.flush
  prefetcher.io.csr_pf_enable := io.csr_pf_enable
  prefetcher.io.ecc_enable := ecc_enable
  prefetcher.io.MSHRResp := missUnit.io.fetch_resp
  prefetcher.io.flushFromBpu := io.ftqPrefetch.flushFromBpu

  // cache softPrefetch
  private val softPrefetchValid = RegInit(false.B)
  private val softPrefetch = RegInit(0.U.asTypeOf(new IPrefetchReq))
  when(io.softPrefetch.map(_.valid).reduce(_ || _)) {
    softPrefetchValid := true.B
    softPrefetch.fromSoftPrefetch(
      MuxCase(
        0.U.asTypeOf(new SoftIfetchPrefetchBundle),
        io.softPrefetch.map(req => req.valid -> req.bits)
      )
    )
  }.elsewhen(prefetcher.io.req.fire) {
    softPrefetchValid := false.B
  }
  // pass ftqPrefetch to IPrefetchPipe
  private val ftqPrefetch = WireInit(0.U.asTypeOf(new IPrefetchReq))
  ftqPrefetch.fromFtqICacheInfo(io.ftqPrefetch.req.bits)
  prefetcher.io.req.valid := softPrefetchValid || io.ftqPrefetch.req.valid
  prefetcher.io.req.bits := Mux(softPrefetchValid, softPrefetch, ftqPrefetch)
  prefetcher.io.req.bits.backendException := io.ftqPrefetch.backendException
  io.ftqPrefetch.req.ready := prefetcher.io.req.ready && !softPrefetchValid

  missUnit.io.hartId := io.hartId
  missUnit.io.fencei := io.fencei
  missUnit.io.flush := io.flush
  missUnit.io.fetch_req <> mainPipe.io.mshr.req
  // Shadow SRAM: mirror tag array for PDIP hit check.
  private val mirrorTagGen = new Bundle {
    val valid: Bool = Bool()
    val tag: UInt = UInt(tagBits.W)
    val epoch: Bool = Bool()
  }
  private val mirrorEpoch = RegInit(false.B)
  when(io.fencei) {
    mirrorEpoch := ~mirrorEpoch
  }

  private val mirrorTagArray = Module(
    new SRAMTemplate(
      mirrorTagGen,
      set = ICacheSets,
      way = ICacheWays,
      shouldReset = true,
      holdRead = true,
      singlePort = false
    )
  )

  // One-entry PDIP mirror lookup pipeline (SRAM read latency = 1)
  private val pdipReq = pdipController.io.prefetchVaddr
  private val pdipS0Pending = RegInit(false.B)
  private val pdipS0Fire = pdipReq.valid && pdipReq.ready
  when(pdipS0Fire) {
    pdipS0Pending := true.B
  }

  private val pdipS1Valid = RegInit(false.B)
  private val pdipS1Bits = Reg(new PrefetchEntry)
  private val pdipS1Hit = RegInit(false.B)

  private val pdipS1En = RegNext(pdipS0Fire, false.B)
  when(pdipS1En) {
    pdipS0Pending := false.B
  }

  val missDedupReq = missWriteBufValid && missWriteBufNeedDedup

  // Accept PDIP request only when no lookup is pending and stage1 is empty
  pdipReq.ready := !pdipS0Pending && !pdipS1Valid && !missDedupReq

  // SRAM read for PDIP mirror hit or miss-write de-dup
  mirrorTagArray.io.r.req.valid := missDedupReq || pdipS0Fire
  mirrorTagArray.io.r.req.bits.setIdx := Mux(
    missDedupReq,
    missMetaBuf.virIdx.asTypeOf(mirrorTagArray.io.r.req.bits.setIdx),
    pdipReq.bits.vSetIdx
  )
  val pdipS0BitsReg = RegEnable(pdipReq.bits, pdipS0Fire)

  private val pdipS1HitCalc = VecInit((0 until ICacheWays).map { w =>
    val entry = mirrorTagArray.io.r.resp.data(w)
    entry.valid && (entry.epoch === mirrorEpoch) &&
      (entry.tag === getPhyTagFromBlk(pdipS0BitsReg.blkPaddr))
  }).reduce((x, y) => x || y)

  when(pdipS1En) {
    pdipS1Valid := true.B
    pdipS1Bits := pdipS0BitsReg
    pdipS1Hit := pdipS1HitCalc
  }

  // De-dup miss writes: if the tag already exists in the set, reuse that way
  private val missDedupResp = RegNext(missDedupReq, false.B)
  private val dedupFlushValid = WireInit(false.B)
  private val dedupFlushBits = Wire(new ICacheMetaFlushBundle)
  dedupFlushBits := 0.U.asTypeOf(new ICacheMetaFlushBundle)
  when(missDedupResp) {
    val matchWays = VecInit((0 until ICacheWays).map { w =>
      val entry = mirrorTagArray.io.r.resp.data(w)
      entry.valid && (entry.epoch === mirrorEpoch) && (entry.tag === missMetaBuf.phyTag)
    }).asUInt
    val matchWayOneHot = PriorityEncoderOH(matchWays)
    val extraWays = matchWays & ~matchWayOneHot
    missWaymaskBuf := Mux(matchWays.orR, matchWayOneHot, missMetaBuf.waymask)
    when(extraWays.orR) {
      dedupFlushValid := true.B
      dedupFlushBits.virIdx := missMetaBuf.virIdx
      dedupFlushBits.waymask := extraWays
    }
    missWriteBufNeedDedup := false.B
  }

  when(missWriteBufValid && !missWriteBufNeedDedup) {
    missWriteBufValid := false.B
  }

  when(dedupFlushValid && !mainPipe.io.metaArrayFlush(0).valid) {
    metaArrayFlush(0).valid := true.B
    metaArrayFlush(0).bits := dedupFlushBits
  }

  private val pdipActive = cacheParams.pdipParams.enabled.B && io.csr_pf_enable
  private val pdipMirrorHit = pdipS1Valid && pdipS1Hit
  private val ftqReqBlkPaddr = io.ftqPrefetch.req.bits.startAddr(PAddrBits - 1, blockOffBits)
  private val pdipDupWithFtqReq = pdipS1Valid && io.ftqPrefetch.req.valid &&
    (pdipS1Bits.blkPaddr === ftqReqBlkPaddr)
  private val pdipReqBits = Wire(new ICacheMissReq)
  pdipReqBits.blkPaddr := pdipS1Bits.blkPaddr
  pdipReqBits.vSetIdx := pdipS1Bits.vSetIdx
  private val pdipDupWithFdipReq = pdipS1Valid && prefetcher.io.MSHRReq.valid &&
    (pdipS1Bits.blkPaddr === prefetcher.io.MSHRReq.bits.blkPaddr)
  private val pdipIssueValid = pdipS1Valid && pdipActive && !pdipMirrorHit &&
    !pdipDupWithFtqReq && !pdipDupWithFdipReq
  private val pdipS1Consume = pdipS1Valid &&
    (pdipMirrorHit || pdipDupWithFtqReq || pdipDupWithFdipReq || !pdipActive)
  when(pdipDupWithFtqReq) {
    printf(p"[PDIP] drop duplicate with FTQ blkPaddr=0x${Hexadecimal(pdipS1Bits.blkPaddr)}\n")
  }
  when(pdipDupWithFdipReq) {
    printf(p"[PDIP] drop duplicate with FDIP blkPaddr=0x${Hexadecimal(pdipS1Bits.blkPaddr)}\n")
  }
  when(pdipS1Consume) {
    pdipS1Valid := false.B
  }

  // Mirror tag SRAM write (single write port, priority: flush > meta write)
  val flushValids = Wire(Vec(PortNumber, Bool()))
  val flushIdxs = Wire(Vec(PortNumber, UInt(log2Ceil(ICacheSets).W)))
  val flushMasks = Wire(Vec(PortNumber, UInt(ICacheWays.W)))
  (0 until PortNumber).foreach { i =>
    flushValids(i) := mainPipe.io.metaArrayFlush(i).valid
    flushIdxs(i) := mainPipe.io.metaArrayFlush(i).bits.virIdx
    flushMasks(i) := mainPipe.io.metaArrayFlush(i).bits.waymask
  }

  val hasFlush = flushValids.asUInt.orR
  val flushOH = PriorityEncoderOH(flushValids)
  val flushIdxSel = Mux1H(flushOH, flushIdxs)
  val flushMaskSel = Mux1H(flushOH, flushMasks)

  val metaWriteValid = metaArray.io.write.fire
  val metaWriteIdx = metaArray.io.write.bits.virIdx
  val metaWriteMask = metaArray.io.write.bits.waymask
  val metaWriteTag = metaArray.io.write.bits.phyTag
  val metaWriteValidBit = !metaArray.io.write.bits.poison

  val mirrorWriteValid = hasFlush || metaWriteValid
  val mirrorWriteIdx = Mux(hasFlush, flushIdxSel, metaWriteIdx)
  val mirrorWriteMask = Mux(hasFlush, flushMaskSel, metaWriteMask)
  val mirrorWriteData = WireInit(
    VecInit(Seq.fill(ICacheWays)(0.U.asTypeOf(mirrorTagGen)))
  )
  when(hasFlush) {
    (0 until ICacheWays).foreach { w =>
      mirrorWriteData(w).valid := false.B
      mirrorWriteData(w).tag := 0.U
      mirrorWriteData(w).epoch := mirrorEpoch
    }
  }.elsewhen(metaWriteValid) {
    (0 until ICacheWays).foreach { w =>
      mirrorWriteData(w).valid := metaWriteValidBit
      mirrorWriteData(w).tag := metaWriteTag
      mirrorWriteData(w).epoch := mirrorEpoch
    }
  }

  mirrorTagArray.io.w.req.valid := mirrorWriteValid
  mirrorTagArray.io.w.req.bits.apply(
    data = mirrorWriteData,
    setIdx = mirrorWriteIdx,
    waymask = mirrorWriteMask
  )

  // PDIP cache-hit gate (SRAM-backed mirror) and shared FDIP/PDIP arbitration.
  // The paper keeps FDIP as the baseline prefetch source and adds PDIP alongside it,
  // so we avoid hard-wiring PDIP priority over the FTQ-driven prefetcher here.
  private val prefetchReqArb = Module(new RRArbiter(new ICacheMissReq, 2))
  prefetchReqArb.io.in(0).valid := prefetcher.io.MSHRReq.valid
  prefetchReqArb.io.in(0).bits := prefetcher.io.MSHRReq.bits
  prefetchReqArb.io.in(1).valid := pdipIssueValid
  prefetchReqArb.io.in(1).bits := pdipReqBits

  missUnit.io.prefetch_req <> prefetchReqArb.io.out
  prefetcher.io.MSHRReq.ready := prefetchReqArb.io.in(0).ready

  when(prefetchReqArb.io.in(1).fire) {
    pdipS1Valid := false.B
  }

  // TLB ports: both used by IPrefetchPipe
  io.itlb(0) <> prefetcher.io.itlb(0)
  io.itlb(1) <> prefetcher.io.itlb(1)
  io.itlbFlushPipe := prefetcher.io.itlbFlushPipe

  // PMP ports: mainPipe(0,1), IPrefetchPipe(2,3)
  io.pmp(2) <> prefetcher.io.pmp(0)
  io.pmp(3) <> prefetcher.io.pmp(1)

  missUnit.io.mem_grant.valid := bus.d.valid
  missUnit.io.mem_grant.bits := bus.d.bits
  bus.d.ready := missUnit.io.mem_grant.ready

  mainPipe.io.flush := io.flush
  mainPipe.io.respStall := io.stop
  mainPipe.io.ecc_enable := ecc_enable
  mainPipe.io.hartId := io.hartId
  mainPipe.io.mshr.resp := missUnit.io.fetch_resp
  mainPipe.io.fetch.req <> io.fetch.req
  mainPipe.io.wayLookupRead <> wayLookup.io.read

  wayLookup.io.flush := io.flush
  wayLookup.io.write <> prefetcher.io.wayLookupWrite
  wayLookup.io.update := missUnit.io.fetch_resp

  replacer.io.touch <> mainPipe.io.touch
  replacer.io.victim <> missUnit.io.victim

  io.pmp(0) <> mainPipe.io.pmp(0)
  io.pmp(1) <> mainPipe.io.pmp(1)

  // FEC Tracker connections
  fecTracker.io.newMiss <> mainPipe.io.fecNewMiss
  private val anyStall =
    mainPipe.io.fetch.stallInfo.map(x => x.valid).reduce((x, y) => x || y)
  fecTracker.io.stallUpdate.valid := anyStall
  fecTracker.io.stallUpdate.bits.ftqIdx := Mux(
    mainPipe.io.fetch.stallInfo(0).valid,
    mainPipe.io.fetch.stallInfo(0).bits.ftqIdx,
    mainPipe.io.fetch.stallInfo(1).bits.ftqIdx
  )
  fecTracker.io.stallUpdate.bits.stalled := true.B
  // Forward ROB commits to the FECC
  (0 until CommitWidth).foreach { i =>
    fecTracker.io.retireUpdate(i).valid := io.rob_commits(i).valid
    fecTracker.io.retireUpdate(i).bits.ftqIdx := io.rob_commits(i).bits.ftqIdx
  }
  // Provide trigger address from the ITLB response of IPrefetchPipe (block-accurate paddr)
  private val currentFetchBlkPaddr = Cat(
    prefetcher.io.itlb(0).resp.bits.paddr(0)(PAddrBits - 1, blockOffBits),
    0.U(blockOffBits.W)
  )(PAddrBits - 1, blockOffBits)
  private val pdipTriggerHintValid =
    prefetcher.io.req.fire && !prefetcher.io.req.bits.isSoftPrefetch
  private val pdipTriggerIsDemand =
    RegEnable(!prefetcher.io.req.bits.isSoftPrefetch, false.B, prefetcher.io.req.fire)
  private val pdipTriggerHighCostHint =
    RegEnable(prefetcher.io.req.bits.pdipHighCostHint, false.B, pdipTriggerHintValid)
  private val pdipTriggerControlHint =
    RegEnable(prefetcher.io.req.bits.pdipControlTrigger, false.B, pdipTriggerHintValid)
  private val pdipTriggerBtbMissHint =
    RegEnable(prefetcher.io.req.bits.pdipBtbMissTrigger, false.B, pdipTriggerHintValid)
  private val currentFetchBlkPaddrValid =
    prefetcher.io.itlb(0).resp.valid && pdipTriggerIsDemand
  fecTracker.io.triggerAddr := currentFetchBlkPaddr
  // Only flush FEC tracker on fence.i, NOT on regular pipeline flushes
  // Regular flushes (branch mispredictions) happen ~5000 times in CoreMark
  // but FEC tracking should survive across flushes to learn patterns
  fecTracker.io.fencei := io.fencei

  pdipController.io.enable := cacheParams.pdipParams.enabled.B && io.csr_pf_enable
  // Keep PDIP learning across regular pipeline flushes; only fence.i should clear it.
  pdipController.io.flush := io.fencei

  // Trigger from current instruction fetch
  // PDIP uses the translated fetch block so lookup and learning share the same address domain.
  pdipController.io.trigger.valid := currentFetchBlkPaddrValid && pdipController.io.enable
  pdipController.io.trigger.bits.blkPaddr := currentFetchBlkPaddr
  pdipController.io.trigger.bits.highCostHint := pdipTriggerHighCostHint
  pdipController.io.trigger.bits.controlTrigger := pdipTriggerControlHint
  pdipController.io.trigger.bits.btbMissTrigger := pdipTriggerBtbMissHint

  // Learn from FEC line detections
  // When FECTracker detects a FEC (Front-End Critical) line, PDIP learns this association
  pdipController.io.fecLine <> fecTracker.io.fecLine
  pdipController.io.dropDupWithFtq := pdipDupWithFtqReq

  // MSHR availability check — use RegNext to break the combinational cycle:
  // pdipReq.valid -> prefetch_req.bits -> prefetch_req.ready -> mshrAvailable -> pdipReq.valid
  pdipController.io.mshrAvailable := RegNext(
    missUnit.io.prefetch_req.ready,
    true.B
  )

  // notify IFU that Icache pipeline is available
  io.toIFU := mainPipe.io.fetch.req.ready
  io.perfInfo := mainPipe.io.perfInfo

  io.fetch.resp := mainPipe.io.fetch.resp
  // forward stall info from main pipe to external IFU/FTQ
  io.fetch.stallInfo := mainPipe.io.fetch.stallInfo
  io.fetch.topdownIcacheMiss := mainPipe.io.fetch.topdownIcacheMiss
  io.fetch.topdownItlbMiss := mainPipe.io.fetch.topdownItlbMiss

  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits := DontCare
  bus.e.valid := false.B
  bus.e.bits := DontCare

  bus.a <> missUnit.io.mem_acquire

  // Parity error port
  private val errors = mainPipe.io.errors
  private val errors_valid = errors.map(e => e.valid).reduce(_ | _)
  io.error.bits <> RegEnable(
    PriorityMux(errors.map(e => e.valid -> e.bits)),
    0.U.asTypeOf(errors(0).bits),
    errors_valid
  )
  io.error.valid := RegNext(errors_valid, false.B)

  XSPerfAccumulate(
    "softPrefetch_drop_not_ready",
    io.softPrefetch.map(_.valid).reduce(_ || _) && softPrefetchValid
  )
  XSPerfAccumulate(
    "softPrefetch_drop_multi_req",
    PopCount(io.softPrefetch.map(_.valid)) > 1.U
  )
  XSPerfAccumulate(
    "softPrefetch_block_ftq",
    softPrefetchValid && io.ftqPrefetch.req.valid
  )

  val perfEvents: Seq[(String, Bool)] = Seq(
    ("icache_miss_cnt  ", false.B),
    (
      "icache_miss_penalty",
      BoolStopWatch(
        start = false.B,
        stop = false.B || false.B,
        startHighPriority = true
      )
    )
  )
  generatePerfEvent()
}

//class ICachePartWayReadBundle[T <: Data](gen: T, pWay: Int)(implicit p: Parameters)
//    extends ICacheBundle {
//  val req = Flipped(Vec(
//    PortNumber,
//    Decoupled(new Bundle {
//      val ridx = UInt((log2Ceil(nSets) - 1).W)
//    })
//  ))
//  val resp = Output(new Bundle {
//    val rdata = Vec(PortNumber, Vec(pWay, gen))
//  })
//}

//class ICacheWriteBundle[T <: Data](gen: T, pWay: Int)(implicit p: Parameters)
//    extends ICacheBundle {
//  val wdata    = gen
//  val widx     = UInt((log2Ceil(nSets) - 1).W)
//  val wbankidx = Bool()
//  val wmask    = Vec(pWay, Bool())
//}

//class ICachePartWayArray[T <: Data](gen: T, pWay: Int)(implicit p: Parameters) extends ICacheArray {
//
//  // including part way data
//  val io = IO {
//    new Bundle {
//      val read  = new ICachePartWayReadBundle(gen, pWay)
//      val write = Flipped(ValidIO(new ICacheWriteBundle(gen, pWay)))
//    }
//  }
//
//  io.read.req.map(_.ready := !io.write.valid)
//
//  val srams = (0 until PortNumber) map { bank =>
//    val sramBank = Module(new SRAMTemplate(
//      gen,
//      set = nSets / 2,
//      way = pWay,
//      shouldReset = true,
//      holdRead = true,
//      singlePort = true,
//      withClockGate = true
//    ))
//
//    sramBank.io.r.req.valid := io.read.req(bank).valid
//    sramBank.io.r.req.bits.apply(setIdx = io.read.req(bank).bits.ridx)
//
//    if (bank == 0) sramBank.io.w.req.valid := io.write.valid && !io.write.bits.wbankidx
//    else sramBank.io.w.req.valid           := io.write.valid && io.write.bits.wbankidx
//    sramBank.io.w.req.bits.apply(
//      data = io.write.bits.wdata,
//      setIdx = io.write.bits.widx,
//      waymask = io.write.bits.wmask.asUInt
//    )
//
//    sramBank
//  }
//
//  io.read.req.map(_.ready := !io.write.valid && srams.map(_.io.r.req.ready).reduce(_ && _))
//
//  io.read.resp.rdata := VecInit(srams.map(bank => bank.io.r.resp.asTypeOf(Vec(pWay, gen))))
//
//}

class SRAMTemplateWithFixedWidthIO[T <: Data](gen: T, set: Int, way: Int)
    extends Bundle {
  val r: SRAMReadBus[T] = Flipped(new SRAMReadBus(gen, set, way))
  val w: SRAMWriteBus[T] = Flipped(new SRAMWriteBus(gen, set, way))
}

// Automatically partition the SRAM based on the width of the data and the desired width.
// final SRAM width = width * way
class SRAMTemplateWithFixedWidth[T <: Data](
    gen: T,
    set: Int,
    width: Int,
    way: Int = 1,
    shouldReset: Boolean = false,
    holdRead: Boolean = false,
    singlePort: Boolean = false,
    bypassWrite: Boolean = false,
    withClockGate: Boolean = false,
    hasMbist: Boolean = false
) extends Module {

  private val dataBits = gen.getWidth
  private val bankNum = math.ceil(dataBits.toDouble / width.toDouble).toInt
  private val totalBits = bankNum * width

  val io: SRAMTemplateWithFixedWidthIO[T] = IO(
    new SRAMTemplateWithFixedWidthIO(gen, set, way)
  )

  private val wordType = UInt(width.W)
  private val writeDatas = (0 until bankNum).map { bank =>
    VecInit((0 until way).map { i =>
      io.w.req.bits
        .data(i)
        .asTypeOf(UInt(totalBits.W))
        .asTypeOf(Vec(bankNum, wordType))(bank)
    })
  }

  private val srams = (0 until bankNum) map { bank =>
    val sramBank = Module(
      new SRAMTemplate(
        wordType,
        set = set,
        way = way,
        shouldReset = shouldReset,
        holdRead = holdRead,
        singlePort = singlePort,
        bypassWrite = bypassWrite,
        withClockGate = withClockGate,
        hasMbist = hasMbist
      )
    )
    // read req
    sramBank.io.r.req.valid := io.r.req.valid
    sramBank.io.r.req.bits.setIdx := io.r.req.bits.setIdx

    // write req
    sramBank.io.w.req.valid := io.w.req.valid
    sramBank.io.w.req.bits.setIdx := io.w.req.bits.setIdx
    sramBank.io.w.req.bits.data := writeDatas(bank)
    sramBank.io.w.req.bits.waymask.foreach(_ := io.w.req.bits.waymask.get)

    sramBank
  }

  io.r.req.ready := !io.w.req.valid
  (0 until way).foreach { i =>
    io.r.resp.data(i) := VecInit(
      (0 until bankNum).map(bank => srams(bank).io.r.resp.data(i))
    ).asTypeOf(UInt(totalBits.W))(dataBits - 1, 0).asTypeOf(gen.cloneType)
  }

  io.r.req.ready := srams.head.io.r.req.ready
  io.w.req.ready := srams.head.io.w.req.ready
}

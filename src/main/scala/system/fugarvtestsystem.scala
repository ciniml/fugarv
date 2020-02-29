package system

import chisel3._
import chisel3.util._
import axi._
import fugarv.FugaRV
import fugarv.FugaRVDebugIO
import java.io.FileInputStream
import scala.collection.mutable

class FugaRVTestSystem(programPath: String) extends Module {
    val io = IO(new Bundle{
      val dbg = new FugaRVDebugIO
      val passed = Output(Bool())
    })

    val rv = Module(new FugaRV(0))
    io.dbg <> rv.io.dbg

    var programData = mutable.ArrayBuffer.empty[UInt]
    val in = Some(new FileInputStream(programPath))
    try {
      val buffer = Array[Byte](0, 0, 0, 0)
      while(in.get.read(buffer) > 0) {
        val value = BigInt((buffer(0).toLong & 0xff) | ((buffer(1).toLong & 0xff) << 8) | ((buffer(2).toLong & 0xff) << 16) | ((buffer(3).toLong & 0xff) << 24))
        programData += value.U(32.W)
      }
    } finally {
      if( in.isDefined ) in.get.close()
    }
    val instMem = Module(new RomReader(32, 32, programData, 0x00000000))
    val dataMem = Module(new RamReaderWriter(32, 32, 0x00000000, 0x10000))

    rv.io.instReader <> instMem.io.reader
    rv.io.memReader <> dataMem.io.reader
    rv.io.memWriter <> dataMem.io.writer

    val maxPc = programData.size * 4
    val failInst = 0x00119193.U // sli TEST_NUM, TEST_NUM, 1
    val testStarted = WireInit(rv.io.dbg.regs(3) >= 2.U)
    val failed = RegInit(false.B)
    when(testStarted && (rv.io.dbg.inst === failInst)) {
      failed := true.B
    }
    io.passed := rv.io.dbg.regs(3) === 1.U && !failed
}

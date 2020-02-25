package system

import chisel3._
import chisel3.util._
import axi._
import fugarv.FugaRV
import fugarv.FugaRVDebugIO

class FugaRvSystem extends RawModule {
    val clock = IO(Input(Clock()))
    val reset_n = IO(Input(Bool()))
    
    val instMM_araddr  = IO(Output(UInt(32.W)))
    val instMM_arvalid = IO(Output(Bool()))
    val instMM_arready = IO(Input(Bool()))
    val instMM_awaddr  = IO(Output(UInt(32.W)))
    val instMM_awvalid = IO(Output(Bool()))
    val instMM_awready = IO(Input(Bool()))
    val instMM_rdata   = IO(Input(UInt(32.W)))
    val instMM_rvalid  = IO(Input(Bool()))
    val instMM_rready  = IO(Output(Bool()))
    val instMM_rresp   = IO(Input(UInt(2.W)))
    val instMM_wdata   = IO(Output(UInt(32.W)))
    val instMM_wvalid  = IO(Output(Bool()))
    val instMM_wready  = IO(Input(Bool()))
    val instMM_wstrb   = IO(Output(UInt(4.W)))
    val instMM_bvalid  = IO(Input(Bool()))
    val instMM_bready  = IO(Output(Bool()))
    val instMM_bresp   = IO(Input(UInt(2.W)))
    
    val dataMM_araddr  = IO(Output(UInt(32.W)))
    val dataMM_arvalid = IO(Output(Bool()))
    val dataMM_arready = IO(Input(Bool()))
    val dataMM_awaddr  = IO(Output(UInt(32.W)))
    val dataMM_awvalid = IO(Output(Bool()))
    val dataMM_awready = IO(Input(Bool()))
    val dataMM_rdata   = IO(Input(UInt(32.W)))
    val dataMM_rvalid  = IO(Input(Bool()))
    val dataMM_rready  = IO(Output(Bool()))
    val dataMM_rresp   = IO(Input(UInt(2.W)))
    val dataMM_wdata   = IO(Output(UInt(32.W)))
    val dataMM_wvalid  = IO(Output(Bool()))
    val dataMM_wready  = IO(Input(Bool()))
    val dataMM_wstrb   = IO(Output(UInt(4.W)))
    val dataMM_bvalid  = IO(Input(Bool()))
    val dataMM_bready  = IO(Output(Bool()))
    val dataMM_bresp   = IO(Input(UInt(2.W)))

    val dbg = IO(new FugaRVDebugIO)

    val instReader = withClockAndReset(clock, !reset_n) { Module(new AXI4MemoryReader(32, 32)) }
    val dataReader = withClockAndReset(clock, !reset_n) { Module(new AXI4MemoryReader(32, 32)) }
    val dataWriter = withClockAndReset(clock, !reset_n) { Module(new AXI4MemoryWriter(32, 32)) }

    instMM_araddr  <> instReader.io.axi4lite.ar.get.araddr
    instMM_arvalid <> instReader.io.axi4lite.ar.get.arvalid
    instMM_arready <> instReader.io.axi4lite.ar.get.arready
    instMM_rdata   <> instReader.io.axi4lite. r.get.rdata
    instMM_rvalid  <> instReader.io.axi4lite. r.get.rvalid
    instMM_rready  <> instReader.io.axi4lite. r.get.rready
    instMM_rresp   <> instReader.io.axi4lite. r.get.rresp
    
    instMM_awaddr  := 0.U
    instMM_awvalid := false.B
    instMM_wdata   := 0.U
    instMM_wvalid  := false.B
    instMM_wstrb   := 0.U
    instMM_bready  := false.B

    dataMM_araddr  <> dataReader.io.axi4lite.ar.get.araddr 
    dataMM_arvalid <> dataReader.io.axi4lite.ar.get.arvalid
    dataMM_arready <> dataReader.io.axi4lite.ar.get.arready
    dataMM_rdata   <> dataReader.io.axi4lite. r.get.rdata  
    dataMM_rvalid  <> dataReader.io.axi4lite. r.get.rvalid 
    dataMM_rready  <> dataReader.io.axi4lite. r.get.rready 
    dataMM_rresp   <> dataReader.io.axi4lite. r.get.rresp  

    dataMM_awaddr  <> dataWriter.io.axi4lite.aw.get.awaddr 
    dataMM_awvalid <> dataWriter.io.axi4lite.aw.get.awvalid
    dataMM_awready <> dataWriter.io.axi4lite.aw.get.awready
    dataMM_wdata   <> dataWriter.io.axi4lite. w.get.wdata  
    dataMM_wvalid  <> dataWriter.io.axi4lite. w.get.wvalid 
    dataMM_wready  <> dataWriter.io.axi4lite. w.get.wready 
    dataMM_wstrb   <> dataWriter.io.axi4lite. w.get.wstrb  
    dataMM_bvalid  <> dataWriter.io.axi4lite. b.get.bvalid 
    dataMM_bready  <> dataWriter.io.axi4lite. b.get.bready 
    dataMM_bresp   <> dataWriter.io.axi4lite. b.get.bresp  

    val rv = withClockAndReset(clock, !reset_n) { Module(new FugaRV) }
    rv.io.instReader <> instReader.io.reader
    rv.io.memReader <> dataReader.io.reader
    rv.io.memWriter <> dataWriter.io.writer

    dbg <> rv.io.dbg
}

object Elaborate extends App {
  
  Driver.execute(
    Array("-tn=fugarv", "-td=rtl/fugarv"),
    () => new FugaRvSystem)
}

package axi


import chisel3._
import chisel3.util._

sealed trait AXI4LiteMode
case object AXI4LiteReadWrite extends AXI4LiteMode
case object AXI4LiteReadOnly  extends AXI4LiteMode
case object AXI4LiteWriteOnly extends AXI4LiteMode

case class AXI4LiteParams
(
    addressBits: Int,
    dataBits: Int,
    mode: AXI4LiteMode
) {
    val strbBits = dataBits / 8
}

class AXI4LiteARChannel(params: AXI4LiteParams) extends Bundle {
    val araddr = Output(UInt(params.addressBits.W))
    val arvalid = Output(Bool())
    val arready = Input(Bool())
}
class AXI4LiteAWChannel(params: AXI4LiteParams) extends Bundle {
    val awaddr = Output(UInt(params.addressBits.W))
    val awvalid = Output(Bool())
    val awready = Input(Bool())
}
class AXI4LiteRChannel(params: AXI4LiteParams) extends Bundle {
    val rdata  = Input(UInt(params.addressBits.W))
    val rresp  = Input(UInt(2.W))
    val rvalid = Input(Bool())
    val rready = Output(Bool())
}
class AXI4LiteWChannel(params: AXI4LiteParams) extends Bundle {
    val wdata  = Output(UInt(params.addressBits.W))
    val wvalid = Output(Bool())
    val wready = Input(Bool())
    val wstrb = Output(UInt(params.strbBits.W))
}
class AXI4LiteBChannel(params: AXI4LiteParams) extends Bundle {
    val bresp  = Input(UInt(2.W))
    val bvalid = Input(Bool())
    val bready = Output(Bool())
}

class AXI4LiteIO(params: AXI4LiteParams) extends Bundle {
    val ar = params.mode match {
        case AXI4LiteWriteOnly => None
        case _ => Some(new AXI4LiteARChannel(params))
    }
    val r = params.mode match {
        case AXI4LiteWriteOnly => None
        case _ => Some(new AXI4LiteRChannel(params))
    }
    val aw = params.mode match {
        case AXI4LiteReadOnly => None
        case _ => Some(new AXI4LiteAWChannel(params))
    }
    val w = params.mode match {
        case AXI4LiteReadOnly => None
        case _ => Some(new AXI4LiteWChannel(params))
    }
    val b = params.mode match {
        case AXI4LiteReadOnly => None
        case _ => Some(new AXI4LiteBChannel(params))
    }

    override def cloneType: AXI4LiteIO.this.type = new AXI4LiteIO(params).asInstanceOf[this.type]
}

class MemoryReaderIO(addressBits: Int, dataBits: Int) extends Bundle {
    val address = Output(UInt(addressBits.W))
    val data = Input(UInt(addressBits.W))
    val request = Output(Bool())
    val response = Input(Bool())
    override def cloneType: MemoryReaderIO.this.type = new MemoryReaderIO(addressBits, dataBits).asInstanceOf[this.type]
}


class MemoryWriterIO(addressBits: Int, dataBits: Int) extends Bundle {
    val strbBits = dataBits/8
    val address = Output(UInt(addressBits.W))
    val data = Output(UInt(addressBits.W))
    val strobe = Output(UInt(strbBits.W))
    val request = Output(Bool())
    val ready = Input(Bool())
    override def cloneType: MemoryWriterIO.this.type = new MemoryWriterIO(addressBits, dataBits).asInstanceOf[this.type]
}


class AXI4MemoryReader(addressBits: Int, dataBits: Int) extends Module {
    val axi4liteParams = AXI4LiteParams(addressBits, dataBits, AXI4LiteReadOnly)
    val io = IO(new Bundle {
        val axi4lite = new AXI4LiteIO(axi4liteParams)
        val reader = Flipped(new MemoryReaderIO(addressBits, dataBits))
    })

    val sIdle :: sAddress :: sData :: Nil = Enum(3)
    val state = RegInit(sIdle)

    val address = RegInit(0.U(addressBits.W))
    val data = RegInit(0.U(dataBits.W))
    val response = RegInit(false.B)

    io.axi4lite.ar.get.arvalid := state === sAddress
    io.axi4lite.ar.get.araddr := address
    io.axi4lite.r.get.rready := state === sData
    io.reader.response := response
    io.reader.data := data

    response := false.B
    switch(state) {
        is(sIdle) {
            when(io.reader.request) {
                address := io.reader.address
                state := sAddress
            }
        }
        is(sAddress) {
            when(io.axi4lite.ar.get.arready) {
                state := sData
            }
        }
        is(sData) {
            when(io.axi4lite.r.get.rvalid) {
                data := io.axi4lite.r.get.rdata
                response := true.B

                when(io.reader.request) {
                    address := io.reader.address
                    state := sAddress
                } otherwise {
                    state := sIdle
                }
            }
        }
    }
}

class AXI4MemoryWriter(addressBits: Int, dataBits: Int) extends Module {
    val strobeBits = dataBits/8
    val axi4liteParams = AXI4LiteParams(addressBits, dataBits, AXI4LiteWriteOnly)
    val io = IO(new Bundle {
        val axi4lite = new AXI4LiteIO(axi4liteParams)
        val writer = Flipped(new MemoryWriterIO(addressBits, dataBits))
    })

    val sIdle :: sAddress :: sData :: sResp :: Nil = Enum(4)
    val state = RegInit(sIdle)

    val address = RegInit(0.U(addressBits.W))
    val data = RegInit(0.U(dataBits.W))
    val ready = RegInit(false.B)
    val strobe = RegInit(0.U(strobeBits.W))

    io.axi4lite.aw.get.awvalid := state === sAddress
    io.axi4lite.aw.get.awaddr := address
    io.axi4lite.w.get.wvalid := state === sData
    io.axi4lite.w.get.wdata := data
    io.axi4lite.w.get.wstrb := strobe
    io.axi4lite.b.get.bready := state === sResp
    io.writer.ready := state === sIdle && ready
    
    ready := true.B
    switch(state) {
        is(sIdle) {
            when(io.writer.request && ready) {
                address := io.writer.address
                data := io.writer.data
                strobe := io.writer.strobe
                state := sAddress
            }
        }
        is(sAddress) {
            when(io.axi4lite.aw.get.awready) {
                state := sData
            }
        }
        is(sData) {
            when(io.axi4lite.w.get.wready) {
                state := sResp
            }
        }
        is(sResp) {
            when(io.axi4lite.b.get.bvalid) {
                state := sIdle
            }
        }
    }
}
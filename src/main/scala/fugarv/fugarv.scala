package fugarv

import chisel3._
import chisel3.util._
import axi._
import chisel3.internal.firrtl.Width

object Opcodes {
    val load    = "b0000011".U
    val loadFp  = "b0000111".U
    val custom0 = "b0001011".U
    val miscMem = "b0001111".U
    val opImm   = "b0010011".U
    val auipc   = "b0010111".U
    val opImm32 = "b0011011".U
    
    val store   = "b0100011".U
    val storeFp = "b0100111".U
    val custom1 = "b0101011".U
    val amo     = "b0101111".U
    val op      = "b0110011".U
    val lui     = "b0110111".U
    val op32    = "b0111011".U
    
    val madd      = "b1000011".U
    val msub      = "b1000111".U
    val nmsub     = "b1001011".U
    val nmadd     = "b1001111".U
    val opFp      = "b1010011".U
    val reserved0 = "b1010111".U
    val custom2   = "b1011011".U

    val branch    = "b1100011".U
    val jalr      = "b1100111".U
    val reserved1 = "b1101011".U
    val jal       = "b1101111".U
    val system    = "b1110011".U
    val reserved2 = "b1110111".U
    val custom3   = "b1111011".U
}

class FugaRVDebugIO extends Bundle {
    val pc = Output(UInt(32.W))
    val state = Output(UInt(4.W))
    val result = Output(UInt(32.W))
    val rs1 = Output(UInt(5.W))
    val rs2 = Output(UInt(5.W))
    val rd  = Output(UInt(5.W))
    val regRs1 = Output(UInt(32.W))
    val regRs2 = Output(UInt(32.W))
    val inst = Output(UInt(32.W))
    val regs = Output(Vec(32, UInt(32.W)))
}
class FugaRV(resetAddress: BigInt = 0x200) extends Module {
    val io = IO(new Bundle {
        val instReader = new MemoryReaderIO(32, 32)
        val memReader = new MemoryReaderIO(32, 32)
        val memWriter = new MemoryWriterIO(32, 32)
        val dbg = new FugaRVDebugIO
    })

    val inst = RegInit(0.U(32.W))
    val opcode = WireInit(inst(6, 0))
    val rd = WireInit(inst(11, 7))
    val rs1 = WireInit(inst(19, 15))
    val rs2 = WireInit(inst(24, 20))
    val funct3 = WireInit(inst(14, 12))
    val funct7 = WireInit(inst(31, 25))
    val immI = WireInit(inst(31, 20))
    val immU = WireInit(inst(31, 12))
    val jalOffset = WireInit(Cat(inst(31), inst(19, 12),  inst(20), inst(30, 21), 0.U(1.W)))
    val branchOffset = WireInit(Cat(inst(31), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W)))
    val storeOffset = WireInit(Cat(inst(31, 25), inst(11, 7)))
    val csr = WireInit(immI)
    val shamt = WireInit(rs2)
    val arithmetic = WireInit(funct7 === "b0100000".U)

    val regs = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
    val cycleCounter = RegInit(0.U(32.W))
    cycleCounter := cycleCounter + 1.U

    val sFetch :: sDecode :: sExecute :: sMemAccess :: sWriteBack :: Nil = Enum(5)
    val state = RegInit(sFetch)

    val pc = RegInit(resetAddress.U(32.W))
    val instRequest = RegInit(true.B)
    io.instReader.address := pc
    io.instReader.request := state === sFetch && instRequest
    

    val regRs1 = RegInit(0.U(32.W))
    val regRs2 = RegInit(0.U(32.W))
    val result = RegInit(0.U(32.W))
    val loadReg = RegInit(false.B)

    
    val readRequest = RegInit(false.B)
    io.memReader.address := result
    io.memReader.request := readRequest
    
    val writeRequest = RegInit(false.B)
    val writeStrobe = RegInit(0.U(4.W))
    io.memWriter.address := result
    io.memWriter.strobe := writeStrobe
    io.memWriter.data := regRs2
    io.memWriter.request := writeRequest
    
    def csrRead(csr: UInt): UInt = MuxCase(0.U, Seq(
        (csr === 0xc00.U) -> cycleCounter,
        (csr === 0xc01.U) -> cycleCounter,
        (csr === 0xf14.U) -> 0.U,   // mhartid
    ))
    def csrWrite(csr: UInt, value: UInt): Unit = {
        // Nothing to do
    }
    def signExtend(value: UInt, w: Width) = {
        Fill(32 - w.get, value(w.get - 1)) ## value
    }

    when(state =/= sFetch) {
        instRequest := true.B
    } otherwise {
        instRequest := false.B
    }

    switch(state) {
        is(sFetch) {
            instRequest := false.B
            loadReg := false.B
            when(io.instReader.response) {
                inst := io.instReader.data
                state := sDecode
            }
        }
        is(sDecode) {
            regRs1 := regs(rs1)
            regRs2 := regs(rs2)
            state := sExecute
        }
        is(sExecute) {
            state := sWriteBack // Go to WriteBack stage by default
            switch(opcode) {
                is(Opcodes.opImm) {
                    loadReg := true.B
                    switch(funct3)  {
                        is("b000".U) { result := (regRs1.asSInt + immI.asSInt).asUInt } // addi
                        is("b010".U) { result := Mux(regRs1.asSInt < immI.asSInt, 1.U, 0.U) } // slti
                        is("b011".U) { result := Mux(regRs1 < signExtend(immI, 12.W), 1.U, 0.U) } // sltiu
                        is("b100".U) { result := regRs1 ^ signExtend(immI, 12.W) } // xori
                        is("b110".U) { result := regRs1 | signExtend(immI, 12.W) } // ori
                        is("b111".U) { result := regRs1 & signExtend(immI, 12.W) } // andi
                        
                        is("b001".U) { result := regRs1 << shamt } // slli
                        is("b101".U) { result := Mux(arithmetic, (regRs1.asSInt >> shamt).asUInt, regRs1 >> shamt) } // srai, srli
                    }
                }
                is(Opcodes.op) {
                    loadReg := true.B
                    switch(funct3)  {
                        is("b000".U) { result := Mux(arithmetic, regRs1.asSInt - regRs2.asSInt, regRs1.asSInt + regRs2.asSInt).asUInt } // sub, add
                        is("b010".U) { result := Mux(regRs1.asSInt < regRs2.asSInt, 1.U, 0.U) } // slt
                        is("b011".U) { result := Mux(regRs1 < regRs2, 1.U, 0.U) } // sltu
                        is("b100".U) { result := regRs1 ^ regRs2 } // xor
                        is("b110".U) { result := regRs1 | regRs2 } // or
                        is("b111".U) { result := regRs1 & regRs2 } // and
                        
                        is("b001".U) { result := regRs1 << regRs2(4, 0) } // sll
                        is("b101".U) { result := Mux(arithmetic, (regRs1.asSInt >> regRs2(4, 0)).asUInt, regRs1 >> regRs2(4, 0)) } // sra, srl
                    }
                }
                is(Opcodes.lui) {
                    loadReg := true.B
                    result := Cat(immU, Fill(12, 0.U(1.W)))
                }
                is(Opcodes.auipc) {
                    loadReg := true.B
                    result := Cat(immU, Fill(12, 0.U(1.W))) + pc
                }
                is(Opcodes.jal) {
                    loadReg := true.B
                    result := pc + 4.U
                }
                is(Opcodes.jalr) {
                    loadReg := true.B
                    result := pc + 4.U
                }
                is(Opcodes.branch) {
                    val condition = MuxCase(false.B, Seq(
                        (funct3 === "b000".U) -> (regRs1 === regRs2),               // beq
                        (funct3 === "b001".U) -> (regRs1 =/= regRs2),               // bne
                        (funct3 === "b100".U) -> (regRs1.asSInt < regRs2.asSInt),   // blt
                        (funct3 === "b110".U) -> (regRs1 < regRs2),                 // bltu
                        (funct3 === "b101".U) -> (regRs1.asSInt >= regRs2.asSInt),  // bge
                        (funct3 === "b111".U) -> (regRs1 >= regRs2),                // bgeu
                    ))
                    result := Mux(condition, 1.U, 0.U)
                }
                is(Opcodes.load) {
                    result := (regRs1.asSInt + immI.asSInt).asUInt & ~3.U(32.W)
                    readRequest := true.B
                    loadReg := true.B
                    state := sMemAccess
                }
                is(Opcodes.store) {
                    result := (regRs1.asSInt + storeOffset.asSInt).asUInt & ~3.U(32.W)
                    writeRequest := true.B
                    state := sMemAccess
                    writeStrobe := MuxCase("b1111".U, Seq(
                        (funct3 === "b000".U) -> "b0001".U, // stb
                        (funct3 === "b001".U) -> "b0011".U, // sth
                        (funct3 === "b010".U) -> "b1111".U, // stw
                    ))
                }
                is(Opcodes.system) {
                    switch(funct3) {
                        is("b010".U) {  // csrrs
                            loadReg := true.B
                            result := csrRead(csr) 
                        }
                        is("b011".U) {  // csrrc
                            loadReg := true.B
                            result := csrRead(csr) 
                        }
                    }
                }
            }
        }
        is(sMemAccess) {
            switch(opcode) {
                is(Opcodes.load) {
                    readRequest := false.B
                    when(io.memReader.response) {
                        switch(funct3) {
                            is("b000".U) { result := io.memReader.data(7,0).asSInt.asUInt }
                            is("b001".U) { result := io.memReader.data(15,0).asSInt.asUInt }
                            is("b010".U) { result := io.memReader.data }
                            is("b100".U) { result := 0.U(24.W) ## io.memReader.data(7,0)}
                            is("b101".U) { result := 0.U(16.W) ## io.memReader.data(15,0) }
                        }
                        state := sWriteBack
                    }
                }
                is(Opcodes.store) {
                    writeRequest := false.B
                    when(io.memWriter.ready) {
                        state := sWriteBack
                    }
                }
            }
        }
        is(sWriteBack) {
            state := sFetch
            pc := pc + 4.U
            when(loadReg && rd > 0.U) {
                regs(rd) := result
            }
            switch(opcode) {
                is(Opcodes.jal) {
                    pc := (pc.asSInt + jalOffset.asSInt).asUInt
                }
                is(Opcodes.jalr) {
                    pc := (regRs1.asSInt + immI.asSInt).asUInt & ~1.U(32.W)
                }
                is(Opcodes.branch) {
                    when(result(0)) {
                        pc := (pc.asSInt + branchOffset.asSInt).asUInt
                    }
                }
                is(Opcodes.system) {
                    switch(funct3) {
                        is("b010".U) {  // csrrs
                            when(rs1 =/= 0.U) {
                                csrWrite(csr, result | rs1)
                            }
                        }
                        is("b011".U) {  // csrrc
                            when(rs1 =/= 0.U) {
                                csrWrite(csr, result & ~rs1)
                            }
                        }
                    }
                }
            }
        }
    }

    io.dbg.pc := pc
    io.dbg.state := state
    io.dbg.inst := inst
    io.dbg.result := result
    io.dbg.rs1 := rs1
    io.dbg.rs2 := rs2
    io.dbg.rd := rd
    io.dbg.regRs1 := regRs1
    io.dbg.regRs2 := regRs2
    io.dbg.regs := regs
}
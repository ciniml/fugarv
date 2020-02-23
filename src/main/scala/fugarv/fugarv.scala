package fugarv

import chisel3._
import chisel3.util._
import axi._

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

class FugaRV extends Module {
    val io = IO(new Bundle {
        val instReader = new MemoryReaderIO(32, 32)
        val memReader = new MemoryReaderIO(32, 32)
        val memWriter = new MemoryWriterIO(32, 32)
    })

    val inst = RegInit(0.U(32.W))
    val opcode = WireInit(inst(6, 0))
    val rd = WireInit(inst(11, 7))
    val rs1 = WireInit(inst(19, 15))
    val rs2 = WireInit(inst(24, 20))
    val funct3 = WireInit(inst(14, 12))
    val funct7 = WireInit(inst(31, 25))
    val immI = WireInit(inst(31, 20))
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

    val pc = RegInit(0x200.U(32.W))
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
    ))
    def csrWrite(csr: UInt, value: UInt): Unit = {
        // Nothing to do
    }

    when(state =/= sFetch) {
        instRequest := true.B
    } otherwise {
        instRequest := false.B
    }

    switch(state) {
        is(sFetch) {
            instRequest := false.B
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
                        is("b000".U) { result := regRs1.asSInt + immI.asSInt } // addi
                        is("b010".U) { result := Mux(regRs1.asSInt < immI.asSInt, 1.U, 0.U) } // slti
                        is("b011".U) { result := Mux(regRs1 < immI, 1.U, 0.U) } // sltiu
                        is("b100".U) { result := regRs1 ^ immI } // xori
                        is("b110".U) { result := regRs1 | immI } // ori
                        is("b111".U) { result := regRs1 & immI } // andi
                        
                        is("b001".U) { result := regRs1 << shamt } // slli
                        is("b101".U) { result := Mux(arithmetic, regRs1.asSInt >> shamt, regRs1 >> shamt) } // srai, srli
                    }
                }
                is(Opcodes.op) {
                    loadReg := true.B
                    switch(funct3)  {
                        is("b000".U) { result := Mux(arithmetic, regRs1.asSInt - regRs2.asSInt, regRs1.asSInt + regRs2.asSInt) } // sub, add
                        is("b010".U) { result := Mux(regRs1.asSInt < regRs2.asSInt, 1.U, 0.U) } // slt
                        is("b011".U) { result := Mux(regRs1 < regRs2, 1.U, 0.U) } // sltu
                        is("b100".U) { result := regRs1 ^ regRs2 } // xor
                        is("b110".U) { result := regRs1 | regRs2 } // or
                        is("b111".U) { result := regRs1 & regRs2 } // and
                        
                        is("b001".U) { result := regRs1 << regRs2 } // sll
                        is("b101".U) { result := Mux(arithmetic, regRs1.asSInt >> regRs2, regRs1 >> regRs2) } // sra, srl
                    }
                }
                is(Opcodes.lui) {
                    loadReg := true.B
                    result := Cat(immI, Fill(12, 0.U(1.W)))
                }
                is(Opcodes.auipc) {
                    loadReg := true.B
                    result := Cat(immI, Fill(12, 0.U(1.W))) + pc
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
                        (funct3 === "b001".U) -> (regRs1 =/= regRs2),               // bnz
                        (funct3 === "b100".U) -> (regRs1.asSInt < regRs2.asSInt),   // blt
                        (funct3 === "b110".U) -> (regRs1 < regRs2),                 // bltu
                        (funct3 === "b101".U) -> (regRs1.asSInt >= regRs2.asSInt),  // bge
                        (funct3 === "b111".U) -> (regRs1 >= regRs2),                // bgeu
                    ))
                    result := Mux(condition, 1.U, 0.U)
                }
                is(Opcodes.load) {
                    result := (regRs1.asSInt + immI.asSInt).asInstanceOf[UInt] & 0xfffffffc.U
                    state := sMemAccess
                }
                is(Opcodes.store) {
                    result := (regRs1.asSInt + storeOffset.asSInt).asInstanceOf[UInt] & 0xfffffffc.U
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
                        result := io.memReader.data
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
            when(loadReg) {
                regs(rd) := result
            }
            switch(opcode) {
                is(Opcodes.jal) {
                    pc := pc.asSInt + jalOffset.asSInt
                }
                is(Opcodes.jalr) {
                    pc := (rs1.asSInt + immI.asSInt).asInstanceOf[UInt] & 0xfffffffe.U(32.W)
                }
                is(Opcodes.branch) {
                    when(result(0)) {
                        pc := pc.asSInt + branchOffset.asSInt
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
}
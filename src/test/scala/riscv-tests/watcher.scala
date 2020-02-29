package test

import chisel3.iotesters._
import system.FugaRVTestSystem
import scala.util.control.Breaks

class FugaRVTestWatcher(c: FugaRVTestSystem, timeoutCycles: Int = 10000, debugOut: Boolean = false, maxPc: Int = -1) extends PeekPokeTester(c) {

    val b = new Breaks
    import b.{breakable, break}

    val _maxPc = if( maxPc < 0 ) c.maxPc else maxPc

    reset()
    step(1)

    breakable {
        for (_ <- 0 until timeoutCycles) {
            val pc = peek(c.io.dbg.pc)
            if (peek(c.io.passed) == 0x1) {
                break
            }
            if( pc > _maxPc) {
                break
            }
            if (debugOut) {
                val inst = peek(c.io.dbg.inst)
                val state = peek(c.io.dbg.state)
                val result = peek(c.io.dbg.result)
                val rs1 = peek(c.io.dbg.rs1)
                val rs2 = peek(c.io.dbg.rs2)
                val regRs1 = peek(c.io.dbg.regRs1)
                val regRs2 = peek(c.io.dbg.regRs2)
                val regDump = (0 to 31).map {regIndex => f"${regIndex}%02d:${peek(c.io.dbg.regs(regIndex))}%08x" }.reduce(_ + " | " + _)
                println(f"pc: ${pc}%08x | state: ${state}%x | inst: ${inst}%08x | rs1(${rs1}%02d): ${regRs1}%08x | rs2(${rs2}%02d): ${regRs2}%08x | result: ${result}%08x | " + regDump)
            }
            step(1)
        }
    }
    expect(c.io.passed, true)
    step(10)
}


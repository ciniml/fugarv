package test

import chisel3.iotesters._
import system.FugaRVTestSystem
import scala.util.control.Breaks


class FugaRVTester extends ChiselFlatSpec {
    val debugOut = false
    def runTestCode(testCodeName: String, timeoutCycles: Int = 1000, maxPc: Int = -1) = {
        Driver.execute(args, () => new FugaRVTestSystem(f"src/test/riscv-tests/isa/${testCodeName}.bin")) {
            c => new FugaRVTestWatcher(c, timeoutCycles, debugOut, maxPc)
        }
    }
    val dutName = "FugaRV"
    behavior of dutName

    val args = Array(
        s"-tn=$dutName",
        s"-td=test_run_dir/$dutName",
        s"-tgvo=on",
    )

    val tests = Array(
        //("rv32ui-p-add", 5000, -1),
        //("rv32ui-p-addi", 3000, -1),
        //("rv32ui-p-and", 5000, -1),
        //("rv32ui-p-andi", 5000, 0x304),
        //("rv32ui-p-auipc", 5000, -1),
        //("rv32ui-p-beq", 5000, -1),
        //("rv32ui-p-bge", 5000, -1),
        //("rv32ui-p-bgeu", 5000, -1),
        //("rv32ui-p-blt", 5000, -1),
        //("rv32ui-p-bltu", 5000, -1),
        //("rv32ui-p-bne", 5000, -1),
        ("rv32ui-p-fence_i", 5000, -1),
        //("rv32ui-p-jal", 5000, -1),
        //("rv32ui-p-jalr", 5000, -1),
        ("rv32ui-p-lb", 5000, -1),
        ("rv32ui-p-lbu", 5000, -1),
        ("rv32ui-p-lh", 5000, -1),
        ("rv32ui-p-lhu", 5000, -1),
        //("rv32ui-p-lui", 5000, -1),
        ("rv32ui-p-lw", 5000, -1),
        //("rv32ui-p-or", 5000, -1),
        //("rv32ui-p-ori", 5000, -1),
        ("rv32ui-p-sb", 5000, -1),
        ("rv32ui-p-sh", 5000, -1),
        //("rv32ui-p-simple", 5000, -1),
        //("rv32ui-p-sll", 5000, -1),
        //("rv32ui-p-slli", 5000, -1),
        //("rv32ui-p-slt", 5000, -1),
        //("rv32ui-p-slti", 5000, -1),
        //("rv32ui-p-sltiu", 5000, -1),
        //("rv32ui-p-sltu", 5000, -1),
        //("rv32ui-p-sra", 5000, -1),
        //("rv32ui-p-srai", 5000, -1),
        //("rv32ui-p-srl", 5000, -1),
        //("rv32ui-p-srli", 5000, -1),
        //("rv32ui-p-sub", 5000, -1),
        ("rv32ui-p-sw", 5000, -1),
        //("rv32ui-p-xor", 5000, -1),
        //("rv32ui-p-xori", 5000, -1),
    )
    for( (code, timeOut, maxPc) <- tests ) {
        it must f"runs ${code}"   in { runTestCode(code, timeOut, maxPc) should be (true) }
    }
}


package test

import chisel3.iotesters._
import system.FugaRVTestSystem
import scala.util.control.Breaks

class FugaRVTester extends ChiselFlatSpec {

    val dutName = "FugaRV"
    behavior of dutName

    val args = Array(
        s"-tn=$dutName ",
        s"-td=test_run_dir/$dutName "
    )

    it must "runs add" in {
        Driver.execute(args, () => new FugaRVTestSystem("src/test/riscv-tests/isa/rv32mi-p-breakpoint.bin")) {
            c => new FugaRVTestWatcher(c)
        } should be (true)
    } 
}


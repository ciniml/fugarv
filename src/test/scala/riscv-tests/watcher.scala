package test

import chisel3.iotesters._
import system.FugaRVTestSystem
import scala.util.control.Breaks

class FugaRVTestWatcher(c: FugaRVTestSystem) extends PeekPokeTester(c) {

    val timeoutCycles = 1000
    val b = new Breaks
    import b.{breakable, break}

    reset()
    step(1)

    breakable {
        for (_ <- 0 until timeoutCycles) {
            if (peek(c.io.passed) == 0x1) {
                break
            }
            step(1)
            break
        }
    }
    expect(c.io.passed, true)
    step(10)
}


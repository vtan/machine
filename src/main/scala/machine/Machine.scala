package machine

import scala.scalajs.js.typedarray.Uint8Array

class Machine(val memory: Uint8Array) {
  var error: Option[String] = None

  private var a: Int = 0
  private var ip: Int = 0

  private var fetched: Short = 0

  def registers: Map[String, Int] = Map(
    "a" -> a,
    "ip" -> ip
  )

  def step(): Unit =
    if (error.isEmpty) {
      val opcodeHead = nextInstructionByte()
      opcodes.get(opcodeHead.toInt) match {
        case Some(Operation(fetch, execute)) =>
          fetch()
          execute()
        case None =>
          error = Some(s"Invalid opcode: $opcodeHead")
      }
    }

  private val opcodes: Map[Int, Operation] = Map(
    0x01 -> Operation(fetchImmediate, add),
    0x02 -> Operation(fetchAddress, add)
  )

  private def fetchImmediate(): Unit =
    fetched = nextInstructionByte()

  private def fetchAddress(): Unit = {
    val lo = nextInstructionByte()
    val hi = nextInstructionByte()
    fetched = memory(lo | hi << 8)
  }

  private def add(): Unit = {
    a = (a + fetched) & 0xFF
  }

  private def nextInstructionByte(): Short = {
    val result = memory(ip)
    ip = (ip + 1) & 0xFFFF
    result
  }
}

object Machine {
  def apply(memory: Seq[Int]): Machine =
    new Machine(Uint8Array.of(memory.map(_.toShort): _*))
}

private final case class Operation(
  fetch: () => Unit,
  execute: () => Unit
)

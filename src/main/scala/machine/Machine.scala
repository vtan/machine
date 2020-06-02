package machine

import scala.scalajs.js.typedarray.Uint8Array

class Machine(val memory: Uint8Array) {
  var error: Option[String] = None

  private var a: Int = 0
  private var x: Int = 0
  private var y: Int = 0
  private var ip: Int = 0
  private var flags: Int = 0

  private val flagZero: Int = 1 << 0
  private val flagCarry: Int = 1 << 1

  private var fetched: Int = 0

  def registers: Map[String, Int] = Map(
    "a" -> a,
    "x" -> x,
    "y" -> y,
    "ip" -> ip,
    "flags" -> flags
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
    0x01 -> Operation(fetchImmediate, adc),
    0x02 -> Operation(fetchAddress, adc),

    0x11 -> Operation(fetchImmediate, movToA),
    0x12 -> Operation(fetchAddress, movToA),
    0x13 -> Operation(fetchX, movToA),
    0x14 -> Operation(fetchY, movToA),

    0x15 -> Operation(fetchImmediate, movToX),
    0x16 -> Operation(fetchAddress, movToX),
    0x17 -> Operation(fetchA, movToX),
    0x18 -> Operation(fetchY, movToX),

    0x19 -> Operation(fetchImmediate, movToY),
    0x1A -> Operation(fetchAddress, movToY),
    0x1B -> Operation(fetchA, movToY),
    0x1C -> Operation(fetchX, movToY),

    0x1D -> Operation(fetchImmediateWord, movFromA),
    0x1E -> Operation(fetchImmediateWord, movFromX),
    0x1F -> Operation(fetchImmediateWord, movFromY)
  )

  private def fetchImmediate(): Unit =
    fetched = nextInstructionByte()

  private def fetchImmediateWord(): Unit =  {
    val lo = nextInstructionByte()
    val hi = nextInstructionByte()
    fetched = lo | hi << 8
  }

  private def fetchAddress(): Unit = {
    fetchImmediateWord()
    fetched = memory(fetched).toInt
  }

  private def fetchA(): Unit = fetched = a
  private def fetchX(): Unit = fetched = x
  private def fetchY(): Unit = fetched = y

  private def adc(): Unit = {
    val sum = a + fetched + (if ((flags & flagCarry) == 0) 0 else 1)
    val result = sum & 0xFF
    flags = 0
    if (result == 0) {
      flags |= flagZero
    }
    if ((sum & 0x100) != 0) {
      flags |= flagCarry
    }
    a = result
  }

  private def movToA(): Unit = a = fetched
  private def movToX(): Unit = x = fetched
  private def movToY(): Unit = y = fetched

  private def movFromA(): Unit = memory(fetched) = a.toShort
  private def movFromX(): Unit = memory(fetched) = x.toShort
  private def movFromY(): Unit = memory(fetched) = y.toShort

  private def nextInstructionByte(): Int = {
    val result = memory(ip)
    ip = (ip + 1) & 0xFFFF
    result.toInt
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

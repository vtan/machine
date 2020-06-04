package machine

class Cpu(bus: Bus) {
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

  def stepMany(steps: Int): Unit =
    (1 to steps).foreach(_ => step())

  // TODO use an array?
  private val opcodes: Map[Int, Operation] = Map(
    0x01 -> Operation(fetchImmediate, adc),
    0x02 -> Operation(fetchFromAddress, adc),

    0x09 -> Operation(fetchReg(() => a), inc(a = _)),
    0x0A -> Operation(fetchReg(() => x), inc(x = _)),
    0x0B -> Operation(fetchReg(() => y), inc(y = _)),

    0x10 -> Operation(fetchImmediate, movToReg(a = _)),
    0x11 -> Operation(fetchFromAddress, movToReg(a = _)),
    0x12 -> Operation(fetchFromIndexedAddress(() => x), movToReg(a = _)),
    0x13 -> Operation(fetchFromIndexedAddress(() => y), movToReg(a = _)),
    0x14 -> Operation(fetchFromIndirectAddress, movToReg(a = _)),

    0x16 -> Operation(fetchImmediate, movToReg(x = _)),
    0x17 -> Operation(fetchFromAddress, movToReg(x = _)),
    0x18 -> Operation(fetchFromIndexedAddress(() => y), movToReg(x = _)),

    0x19 -> Operation(fetchImmediate, movToReg(y = _)),
    0x1A -> Operation(fetchFromAddress, movToReg(y = _)),
    0x1B -> Operation(fetchFromIndexedAddress(() => x), movToReg(y = _)),

    0x20 -> Operation(fetchAddress, movFromReg(() => a)),
    0x21 -> Operation(fetchIndexedAddress(() => x), movFromReg(() => a)),
    0x22 -> Operation(fetchIndexedAddress(() => y), movFromReg(() => a)),
    0x23 -> Operation(fetchIndirectAddress, movFromReg(() => a)),

    0x25 -> Operation(fetchAddress, movFromReg(() => x)),
    0x26 -> Operation(fetchIndexedAddress(() => y), movFromReg(() => x)),

    0x27 -> Operation(fetchAddress, movFromReg(() => y)),
    0x28 -> Operation(fetchIndexedAddress(() => x), movFromReg(() => y)),

    0x30 -> Operation(fetchReg(() => a), movToReg(x = _)),
    0x31 -> Operation(fetchReg(() => a), movToReg(y = _)),
    0x32 -> Operation(fetchReg(() => x), movToReg(a = _)),
    0x33 -> Operation(fetchReg(() => y), movToReg(a = _)),

    0x40 -> Operation(fetchImmediate, jmp),
    0x41 -> Operation(fetchImmediate, jz),
    0x42 -> Operation(fetchImmediate, jnz),
    0x43 -> Operation(fetchImmediate, jc),
    0x44 -> Operation(fetchImmediate, jnc),
  )

  private def fetchImmediate(): Unit =
    fetched = nextInstructionByte()

  private def fetchAddress(): Unit =  {
    val lo = nextInstructionByte()
    val hi = nextInstructionByte()
    fetched = lo | hi << 8
  }

  private def fetchFromAddress(): Unit = {
    fetchAddress()
    fetched = bus.read(fetched).toInt
  }

  private def fetchIndexedAddress(index: () => Int)(): Unit = {
    fetchAddress()
    fetched = (fetched + index()) & 0xFFFF
  }

  private def fetchFromIndexedAddress(index: () => Int)(): Unit = {
    fetchIndexedAddress(index)
    fetched = bus.read(fetched).toInt
  }

  private def fetchIndirectAddress(): Unit = {
    fetchAddress()
    val lo = bus.read(fetched).toInt
    val hi = bus.read(fetched + 1).toInt
    fetched = (lo | hi << 8)
  }

  private def fetchFromIndirectAddress(): Unit = {
    fetchIndirectAddress()
    fetched = bus.read(fetched).toInt
  }

  private def fetchReg(get: () => Int)(): Unit =
    fetched = get()

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

  private def inc(set: Int => ())(): Unit = {
    val result = (fetched + 1) & 0xFF
    set(result)
    setFlag(flagZero, result == 0)
  }

  private def movToReg(set: Int => ())(): Unit =
    set(fetched)

  private def movFromReg(get: () => Int)(): Unit =
    bus.write(fetched, get().toShort)

  private def jmp(): Unit = {
    val signed = if ((fetched & 0x80) == 0) {
      fetched
    } else {
      -((~fetched & 0xFF) + 1)
    }
    ip = (ip + signed) & 0xFFFF
  }

  private def jz(): Unit =
    if ((flags & flagZero) != 0) jmp()

  private def jnz(): Unit =
    if ((flags & flagZero) == 0) jmp()

  private def jc(): Unit =
    if ((flags & flagCarry) != 0) jmp()

  private def jnc(): Unit =
    if ((flags & flagCarry) == 0) jmp()

  private def nextInstructionByte(): Int = {
    val result = bus.read(ip)
    ip = (ip + 1) & 0xFFFF
    result.toInt
  }

  private def setFlag(flag: Int, value: Boolean): Unit =
    if (value) {
      flags |= flag
    } else {
      flags &= ~flag
    }
}

private final case class Operation(
  fetch: () => Unit,
  execute: () => Unit
)

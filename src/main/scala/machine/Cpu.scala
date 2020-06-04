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
        case Some(Operation(runWithAddress, getAddress)) =>
          runWithAddress(getAddress())
        case None =>
          error = Some(s"Invalid opcode: $opcodeHead")
      }
    }

  def stepMany(steps: Int): Unit =
    (1 to steps).foreach(_ => step())

  private def stepInstructionPointer(): Unit =
    ip = (ip + 1) & 0xFFFF

  private def nextInstructionByte(): Int = {
    val result = bus.read(ip)
    stepInstructionPointer()
    result.toInt
  }

  private def setFlag(flag: Int, value: Boolean): Unit =
    if (value) {
      flags |= flag
    } else {
      flags &= ~flag
    }

  private def setZeroFlag(value: Int): Unit =
    setFlag(flagZero, value == 0)

  // TODO use an array?
  private val opcodes: Map[Int, Operation] = {
    import Addressing._, Instructions._
    Map(
      0x01 -> Operation(adc, immediate),
      0x02 -> Operation(adc, absolute),

      0x09 -> Operation(incReg(() => a, a = _), implied),
      0x0A -> Operation(incReg(() => x, x = _), implied),
      0x0B -> Operation(incReg(() => y, y = _), implied),
      0x0C -> Operation(incMem, absolute),
      0x0D -> Operation(incMem, absoluteIndexed(() => x)),

      0x10 -> Operation(movToReg(a = _), immediate),
      0x11 -> Operation(movToReg(a = _), absolute),
      0x12 -> Operation(movToReg(a = _), absoluteIndexed(() => x)),
      0x13 -> Operation(movToReg(a = _), absoluteIndexed(() => y)),
      0x14 -> Operation(movToReg(a = _), indirect),

      0x16 -> Operation(movToReg(x = _), immediate),
      0x17 -> Operation(movToReg(x = _), absolute),
      0x18 -> Operation(movToReg(x = _), absoluteIndexed(() => y)),

      0x19 -> Operation(movToReg(y = _), immediate),
      0x1A -> Operation(movToReg(y = _), absolute),
      0x1B -> Operation(movToReg(y = _), absoluteIndexed(() => x)),

      0x20 -> Operation(movFromReg(() => a), absolute),
      0x21 -> Operation(movFromReg(() => a), absoluteIndexed(() => x)),
      0x22 -> Operation(movFromReg(() => a), absoluteIndexed(() => y)),
      0x23 -> Operation(movFromReg(() => a), indirect),

      0x25 -> Operation(movFromReg(() => x), absolute),
      0x26 -> Operation(movFromReg(() => x), absoluteIndexed(() => y)),

      0x27 -> Operation(movFromReg(() => y), absolute),
      0x28 -> Operation(movFromReg(() => y), absoluteIndexed(() => x)),

      0x30 -> Operation(movBetweenRegs(() => a, x = _), implied),
      0x31 -> Operation(movBetweenRegs(() => a, y = _), implied),
      0x32 -> Operation(movBetweenRegs(() => x, a = _), implied),
      0x33 -> Operation(movBetweenRegs(() => y, a = _), implied),

      0x40 -> Operation(jmp, relative),
      0x41 -> Operation(jz, relative),
      0x42 -> Operation(jnz, relative),
      0x43 -> Operation(jc, relative),
      0x44 -> Operation(jnc, relative),
    )
  }

  private object Addressing {

    def implied(): Int = 0

    def immediate(): Int = {
      val addr = ip
      stepInstructionPointer()
      addr
    }

    def relative(): Int = {
      val unsigned = nextInstructionByte()
      if ((unsigned & 0x80) == 0) {
        unsigned
      } else {
        -((~unsigned & 0xFF) + 1)
      }
    }

    def absolute(): Int = {
      val lo = nextInstructionByte()
      val hi = nextInstructionByte()
      lo | hi << 8
    }

    def absoluteIndexed(reg: () => Int)(): Int = {
      val lo = nextInstructionByte()
      val hi = nextInstructionByte()
      ((lo | hi << 8) + reg()) & 0xFFFF
    }

    def indirect(): Int = {
      val addrLo = nextInstructionByte()
      val addrHi = nextInstructionByte()
      val addr = addrLo | addrHi << 8
      val lo = bus.read(addr)
      val hi = bus.read(addr + 1)
      lo | hi << 8
    }
  }

  private object Instructions {

    def adc(address: Int): Unit = {
      val sum = a + bus.read(address) + (if ((flags & flagCarry) == 0) 0 else 1)
      val result = sum & 0xFF
      setZeroFlag(result)
      setFlag(flagCarry, (sum & 0x100) != 0)
      a = result
    }

    def incReg(get: () => Int, set: Int => ())(address: Int): Unit = {
      val _ = address
      val result = (get() + 1) & 0xFF
      set(result)
      setZeroFlag(result)
    }

    def incMem(address: Int): Unit = {
      val result = (bus.read(address) + 1) & 0xFF
      bus.write(address, result.toShort)
      setZeroFlag(result)
    }

    def movToReg(set: Int => ())(address: Int): Unit =
      set(bus.read(address).toInt)

    def movFromReg(get: () => Int)(address: Int): Unit =
      bus.write(address, get().toShort)

    def movBetweenRegs(get: () => Int, set: Int => ())(address: Int): Unit = {
      val _ = address
      set(get())
    }

    def jmp(address: Int): Unit =
      ip = (ip + address) & 0xFFFF

    def jz(address: Int): Unit =
      if ((flags & flagZero) != 0) jmp(address)

    def jnz(address: Int): Unit =
      if ((flags & flagZero) == 0) jmp(address)

    def jc(address: Int): Unit =
      if ((flags & flagCarry) != 0) jmp(address)

    def jnc(address: Int): Unit =
      if ((flags & flagCarry) == 0) jmp(address)
  }
}

private final case class Operation(
  runWithAddress: Int => Unit,
  getAddress: () => Int
)

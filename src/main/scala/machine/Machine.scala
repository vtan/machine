package machine

final case class Machine(
  memory: Vector[Int],
  registers: Vector[Int],
  error: Option[String]
) {
  val ax: Int = registers(Machine.axAddr)
  val bx: Int = registers(Machine.bxAddr)
  val ip: Int = registers(Machine.ipAddr)

  def step: Option[Machine] =
    error match {
      case Some(_) => None
      case None =>
        Some {
          val opcodeHead = memory(ip)
          val instruction = Instruction.allByOpcodeHead.get(opcodeHead)
          instruction match {
            case Some(TwoOperandInstruction(operation)) =>
              val discriminator = memory(ip + 1)
              val (operands, opcodeSize) = discriminator & 3 match {
                case 0 =>
                  val dest = discriminator >> 2 & 3
                  val src = memory(ip + 2) | memory(ip + 3) << 8
                  TwoOperandInstruction.RegMem(dest, src) -> 4
                case 1 =>
                  val dest = memory(ip + 2) | memory(ip + 3) << 8
                  val src = discriminator >> 2 & 3
                  TwoOperandInstruction.MemReg(dest, src) -> 4
                case 2 =>
                  val dest = discriminator >> 2 & 3
                  val src = discriminator >> 4 & 3
                  TwoOperandInstruction.RegReg(dest, src) -> 2
                case 3 =>
                  val dest = discriminator >> 2 & 3
                  val src = memory(ip + 2) | memory(ip + 3) << 8
                  TwoOperandInstruction.RegImm(dest, src) -> 4
              }
              operation(
                operands,
                copy(registers = registers.updated(Machine.ipAddr, ip + opcodeSize))
              )
            case None =>
              copy(error = Some(s"Unknown opcode: $opcodeHead"))
          }
        }
    }
}

object Machine {

  def apply(memory: Vector[Int]): Machine = Machine(
    memory = memory,
    registers = Vector.fill(3)(0),
    error = None
  )

  val axAddr: Int = 0
  val bxAddr: Int = 1
  private val ipAddr: Int = 2
}

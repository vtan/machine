package machine.assembler

import machine.wordToBytes

private[assembler] object BytecodeBuilder {

  def build(instructions: Seq[Instruction]): Either[String, Vector[Int]] = {
    val (errors, bytes) = instructions.partitionMap { instruction =>
      buildInstruction(instruction).fold(
        err => Left(s"Error at ${instruction.position}: " ++ err),
        Right(_)
      )
    }
    errors match {
      case Seq() => Right(bytes.flatten.toVector)
      case firstError +: _ => Left(firstError)
    }
  }

  private def buildInstruction(instruction: Instruction): Either[String, Vector[Int]] = {
    import Operand._
    instruction.operation match {
      case "add" =>
        val opcode = 0x01
        instruction.operands match {
          case Seq(RegisterAsBits(reg), Address(addr)) =>
            val discriminator = reg << 2
            Right(opcode +: discriminator +: wordToBytes(addr))

          case Seq(Address(addr), RegisterAsBits(reg)) =>
            val discriminator = reg << 2 | 1
            Right(opcode +: discriminator +: wordToBytes(addr))

          case Seq(RegisterAsBits(reg1), RegisterAsBits(reg2)) =>
            val discriminator = reg2 << 4 | reg1 << 2 | 2
            Right(Vector(opcode, discriminator))

          case Seq(RegisterAsBits(reg), Immediate(imm)) =>
            val discriminator = reg << 2 | 3
            Right(opcode +: discriminator +: wordToBytes(imm))

          case Seq(_, _) => Left("Invalid operands for instruction")
          case _ => Left("Invalid number of operands, expected 2")
        }

      case unknown =>
        Left(s"Unknown operation: $unknown")
    }
  }

  private object RegisterAsBits {
    def unapply(register: Operand.Register): Option[Int] =
      register match {
        case Operand.AX => Some(0)
        case Operand.BX => Some(1)
      }
  }
}

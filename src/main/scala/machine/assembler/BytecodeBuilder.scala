package machine.assembler

import machine.wordToBytes

private[assembler] object BytecodeBuilder {

  def build(instructions: Seq[Instruction]): Either[String, Seq[Int]] = {
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

  private val instructions: Map[String, PartialFunction[Seq[Operand], Seq[Int]]] = {
    import Operand._
    Map(
      "adc" -> {
        case Seq(Immediate(imm)) => Seq(0x01, imm)
        case Seq(Address(addr)) => 0x02 +: wordToBytes(addr)
      },
      "mov" -> {
        case Seq(A, Immediate(imm)) => Seq(0x11, imm)
        case Seq(A, Address(addr)) => 0x12 +: wordToBytes(addr)
        case Seq(A, X) => Seq(0x13)
        case Seq(A, Y) => Seq(0x14)

        case Seq(X, Immediate(imm)) => Seq(0x15, imm)
        case Seq(X, Address(addr)) => 0x16 +: wordToBytes(addr)
        case Seq(X, A) => Seq(0x17)
        case Seq(X, Y) => Seq(0x18)

        case Seq(Y, Immediate(imm)) => Seq(0x19, imm)
        case Seq(Y, Address(addr)) => 0x1A +: wordToBytes(addr)
        case Seq(Y, A) => Seq(0x1B)
        case Seq(Y, Y) => Seq(0x1C)

        case Seq(Address(addr), A) => 0x1D +: wordToBytes(addr)
        case Seq(Address(addr), X) => 0x1E +: wordToBytes(addr)
        case Seq(Address(addr), Y) => 0x1F +: wordToBytes(addr)
      }
    )
  }

  private def buildInstruction(instruction: Instruction): Either[String, Seq[Int]] =
    instructions.get(instruction.operation) match {
      case None => Left(s"Invalid instruction: ${instruction.operation}")
      case Some(validOperands) =>
        instruction.operands match {
          case validOperands(bytes) => Right(bytes)
          case _ => Left("Invalid operands for instruction")
        }
    }
}

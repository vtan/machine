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
      "add" -> {
        case Seq(Immediate(imm)) => Seq(0x01, imm)
        case Seq(Address(addr)) => 0x02 +: wordToBytes(addr)
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

package machine.assembler

import machine.wordToBytes

private[assembler] object BytecodeBuilder {

  private final case class State(
    currentOffset: Int,
    labelOffsets: Map[String, Int],
    bytecode: Vector[Int]
  )

  def build(commands: Seq[Command]): Either[String, Seq[Int]] = {
    val initialState: Either[String, State] = Right(State(
      currentOffset = 0,
      labelOffsets = Map.empty,
      bytecode = Vector.empty
    ))
    for {
      result <- commands.foldLeft(initialState) { (errorOrState, command) =>
          errorOrState match {
            case Right(state) => processCommand(command, state)
            case Left(error) => Left(s"Error at ${command.position}: " ++ error)
          }
        }
    } yield {
      println(result.labelOffsets)
      result.bytecode
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

  private def processCommand(command: Command, state: State): Either[String, State] =
    command match {
      case Label(label, _) =>
        if (state.labelOffsets.contains(label)) {
          Left("Duplicate label")
        } else {
          Right(state.copy(labelOffsets = state.labelOffsets + (label -> state.currentOffset)))
        }

      case Instruction(operation, operands, _) =>
        instructions.get(operation) match {
          case None => Left(s"Invalid instruction: $operation")
          case Some(validOperands) =>
            operands match {
              case validOperands(bytes) =>
                // TODO check if it fits in memory
                val offset = state.currentOffset + bytes.length
                Right(state.copy(
                  currentOffset = offset,
                  bytecode = state.bytecode ++ bytes
                ))
              case _ =>
                Left("Invalid operands for instruction")
            }
        }
    }
}

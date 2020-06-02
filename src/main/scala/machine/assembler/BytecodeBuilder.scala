package machine.assembler

private[assembler] object BytecodeBuilder {

  private final case class State(
    currentOffset: Int,
    symbolOffsets: Map[String, Int],
    symbolReferencesWithOpcodeOffsets: Seq[(NeedsRelativeOffset, Int)],
    bytecode: Vector[Int]
  )

  def build(commands: Seq[Command]): Either[String, Seq[Int]] = {
    val initialState: Either[String, State] = Right(State(
      currentOffset = 0,
      symbolOffsets = Map.empty,
      symbolReferencesWithOpcodeOffsets = Seq.empty,
      bytecode = Vector.empty
    ))
    for {
      result <- commands.foldLeft(initialState) { (errorOrState, command) =>
        errorOrState match {
          case Right(state) => processCommand(command, state)
          case Left(error) => Left(s"Error at ${command.position}: " ++ error)
        }
      }
      bytecodeWithReferences <- fillReferences(
        result.symbolOffsets,
        result.symbolReferencesWithOpcodeOffsets,
        result.bytecode
      )
    } yield bytecodeWithReferences
  }

  private def fillReferences(
    offsets: Map[String, Int],
    refsWithOpcodeOffsets: Seq[(NeedsRelativeOffset, Int)],
    bytesWithoutRefs: Vector[Int]
  ): Either[String, Vector[Int]] = {
    val initialState: Either[String, Vector[Int]] = Right(bytesWithoutRefs)
    refsWithOpcodeOffsets.foldLeft(initialState) {
      case (Left(e), _) => Left(e)
      case (Right(bytes), (instruction, opcodeOffset)) =>
        offsets.get(instruction.symbol) match {
          case None => Left(s"Reference to undefined symbol: ${instruction.symbol}")
          case Some(offset) =>
            val ipAfterInstruction = opcodeOffset + 2
            val distance = offset - ipAfterInstruction
            if (distance >= -128 && distance <= 127) {
              // TODO check if the offset we're filling is present in the array
              Right(bytes.updated(opcodeOffset + 1, distance))
            } else {
              Left(s"Signed relative offset to ${instruction.symbol} does not fit a byte")
            }
        }
    }
  }

  private def processCommand(command: Command, state: State): Either[String, State] =
    command match {
      case Label(symbol, _) =>
        if (state.symbolOffsets.contains(symbol)) {
          Left("Duplicate label")
        } else {
          Right(state.copy(symbolOffsets = state.symbolOffsets + (symbol -> state.currentOffset)))
        }

      case Instruction(operation, operands, _) =>
        instructions.get(operation) match {
          case None => Left(s"Invalid instruction: $operation")
          case Some(validOperands) =>
            operands match {
              case validOperands(bytecode) =>
                // TODO check if it fits in memory
                val bytes = bytecode.toSeq
                val offset = state.currentOffset + bytes.length
                val symbolReferences = bytecode match {
                  case _: Final => Seq.empty
                  case ref: NeedsRelativeOffset => Seq((ref, state.currentOffset))
                }
                Right(state.copy(
                  currentOffset = offset,
                  bytecode = state.bytecode ++ bytes,
                  symbolReferencesWithOpcodeOffsets = state.symbolReferencesWithOpcodeOffsets ++ symbolReferences
                ))
              case _ =>
                Left("Invalid operands for instruction")
            }
        }
    }

  private val instructions: Map[String, PartialFunction[Seq[Operand], InstructionBytecode]] = {
    import Operand._
    Map(
      "adc" -> {
        case Seq(Immediate(imm)) => Final(0x01, imm)
        case Seq(Address(addr)) => Final.withAbsoluteAddress(0x02, addr)
      },
      "mov" -> {
        case Seq(A, Immediate(imm)) => Final(0x11, imm)
        case Seq(A, Address(addr)) => Final.withAbsoluteAddress(0x12, addr)
        case Seq(A, X) => Final(0x13)
        case Seq(A, Y) => Final(0x14)

        case Seq(X, Immediate(imm)) => Final(0x15, imm)
        case Seq(X, Address(addr)) => Final.withAbsoluteAddress(0x16, addr)
        case Seq(X, A) => Final(0x17)
        case Seq(X, Y) => Final(0x18)

        case Seq(Y, Immediate(imm)) => Final(0x19, imm)
        case Seq(Y, Address(addr)) => Final.withAbsoluteAddress(0x1A, addr)
        case Seq(Y, A) => Final(0x1B)
        case Seq(Y, Y) => Final(0x1C)

        case Seq(Address(addr), A) => Final.withAbsoluteAddress(0x1D, addr)
        case Seq(Address(addr), X) => Final.withAbsoluteAddress(0x1E, addr)
        case Seq(Address(addr), Y) => Final.withAbsoluteAddress(0x1F, addr)
      },
      "jmp" -> { case Seq(Symbol(sym)) => NeedsRelativeOffset(0x21, sym) },
      "jz" -> { case Seq(Symbol(sym)) => NeedsRelativeOffset(0x22, sym) },
      "jnz" -> { case Seq(Symbol(sym)) => NeedsRelativeOffset(0x23, sym) },
      "jc" -> { case Seq(Symbol(sym)) => NeedsRelativeOffset(0x24, sym) },
      "jnc" -> { case Seq(Symbol(sym)) => NeedsRelativeOffset(0x25, sym) }
    )
  }
}

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
      "inc" -> {
        case Seq(A) => Final(0x09)
        case Seq(X) => Final(0x0A)
        case Seq(Y) => Final(0x0B)
      },
      "mov" -> {
        case Seq(A, Immediate(imm)) => Final(0x10, imm)
        case Seq(A, Address(addr)) => Final.withAbsoluteAddress(0x11, addr)
        case Seq(A, IndexedAddress(addr, X)) => Final.withAbsoluteAddress(0x12, addr)
        case Seq(A, IndexedAddress(addr, Y)) => Final.withAbsoluteAddress(0x13, addr)
        case Seq(A, IndirectAddress(addr)) => Final.withAbsoluteAddress(0x14, addr)

        case Seq(X, Immediate(imm)) => Final(0x16, imm)
        case Seq(X, Address(addr)) => Final.withAbsoluteAddress(0x17, addr)
        case Seq(X, IndexedAddress(addr, Y)) => Final.withAbsoluteAddress(0x18, addr)

        case Seq(Y, Immediate(imm)) => Final(0x19, imm)
        case Seq(Y, Address(addr)) => Final.withAbsoluteAddress(0x1A, addr)
        case Seq(Y, IndexedAddress(addr, X)) => Final.withAbsoluteAddress(0x1B, addr)

        case Seq(Address(addr), A) => Final.withAbsoluteAddress(0x20, addr)
        case Seq(IndexedAddress(addr, X), A) => Final.withAbsoluteAddress(0x21, addr)
        case Seq(IndexedAddress(addr, Y), A) => Final.withAbsoluteAddress(0x22, addr)
        case Seq(IndirectAddress(addr), A) => Final.withAbsoluteAddress(0x23, addr)

        case Seq(Address(addr), X) => Final.withAbsoluteAddress(0x25, addr)
        case Seq(IndexedAddress(addr, Y), X) => Final.withAbsoluteAddress(0x26, addr)

        case Seq(Address(addr), Y) => Final.withAbsoluteAddress(0x27, addr)
        case Seq(IndexedAddress(addr, X), Y) => Final.withAbsoluteAddress(0x28, addr)

        case Seq(X, A) => Final(0x30)
        case Seq(Y, A) => Final(0x31)
        case Seq(A, X) => Final(0x32)
        case Seq(A, Y) => Final(0x33)
      },
      "jmp" -> { case Seq(Symbol(sym)) => NeedsRelativeOffset(0x40, sym) },
      "jz" -> { case Seq(Symbol(sym)) => NeedsRelativeOffset(0x41, sym) },
      "jnz" -> { case Seq(Symbol(sym)) => NeedsRelativeOffset(0x42, sym) },
      "jc" -> { case Seq(Symbol(sym)) => NeedsRelativeOffset(0x43, sym) },
      "jnc" -> { case Seq(Symbol(sym)) => NeedsRelativeOffset(0x44, sym) }
    )
  }
}

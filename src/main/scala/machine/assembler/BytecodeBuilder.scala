package machine.assembler

private[assembler] object BytecodeBuilder {

  private final case class State(
    currentOffset: Int,
    symbolOffsets: Map[String, Int],
    bytecode: Bytecode
  )

  def build(commands: Seq[Command]): Either[String, Seq[Int]] = {
    val initialState: Either[String, State] = Right(State(
      currentOffset = 0,
      symbolOffsets = Map.empty,
      bytecode = Bytecode.empty
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
        result.bytecode
      )
    } yield bytecodeWithReferences
  }

  private def fillReferences(
    offsets: Map[String, Int],
    bytecode: Bytecode
  ): Either[String, Seq[Int]] = {
    val initialState: Either[String, Seq[Int]] = Right(bytecode.bytes)
    bytecode.references.foldLeft(initialState) {
      case (Left(e), _) => Left(e)
      case (Right(bytes), reference) =>
        offsets.get(reference.symbol) match {
          case None =>
            Left(s"Reference to undefined symbol: ${reference.symbol}")
          case Some(offset) =>
            reference.patchWithSymbolOffset(offset)(bytes)
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
              case validOperands(bytecodeGenerator) =>
                bytecodeGenerator(state.currentOffset).map { bytecode =>
                  // TODO check if it fits in memory
                  val offset = state.currentOffset + bytecode.bytes.length
                  state.copy(
                    currentOffset = offset,
                    bytecode = state.bytecode.append(bytecode)
                  )
                }
              case _ =>
                Left("Invalid operands for instruction")
            }
        }
    }

  private val instructions: Map[String, PartialFunction[Seq[Operand], Int => Either[String, Bytecode]]] = {
    import Operand._
    import Function.const
    import Bytecode.{noOperand, knownOperandByte, knownOperandWord, relativeReference}
    Map(
      "adc" -> {
        case Seq(Immediate(imm)) => const(knownOperandByte(0x01, imm))
        case Seq(Address(addr)) => const(knownOperandWord(0x02, addr))
      },
      "inc" -> {
        case Seq(A) => const(noOperand(0x09))
        case Seq(X) => const(noOperand(0x0A))
        case Seq(Y) => const(noOperand(0x0B))
        case Seq(Address(addr)) => const(knownOperandWord(0x0C, addr))
        case Seq(IndexedAddress(addr, X)) => const(knownOperandWord(0x0D, addr))
      },
      "mov" -> {
        case Seq(A, Immediate(imm)) => const(knownOperandByte(0x10, imm))
        case Seq(A, Address(addr)) => const(knownOperandWord(0x11, addr))
        case Seq(A, IndexedAddress(addr, X)) => const(knownOperandWord(0x12, addr))
        case Seq(A, IndexedAddress(addr, Y)) => const(knownOperandWord(0x13, addr))
        case Seq(A, IndirectAddress(addr)) => const(knownOperandWord(0x14, addr))

        case Seq(X, Immediate(imm)) => const(knownOperandByte(0x16, imm))
        case Seq(X, Address(addr)) => const(knownOperandWord(0x17, addr))
        case Seq(X, IndexedAddress(addr, Y)) => const(knownOperandWord(0x18, addr))

        case Seq(Y, Immediate(imm)) => const(knownOperandByte(0x19, imm))
        case Seq(Y, Address(addr)) => const(knownOperandWord(0x1A, addr))
        case Seq(Y, IndexedAddress(addr, X)) => const(knownOperandWord(0x1B, addr))

        case Seq(Address(addr), A) => const(knownOperandWord(0x20, addr))
        case Seq(IndexedAddress(addr, X), A) => const(knownOperandWord(0x21, addr))
        case Seq(IndexedAddress(addr, Y), A) => const(knownOperandWord(0x22, addr))
        case Seq(IndirectAddress(addr), A) => const(knownOperandWord(0x23, addr))

        case Seq(Address(addr), X) => const(knownOperandWord(0x25, addr))
        case Seq(IndexedAddress(addr, Y), X) => const(knownOperandWord(0x26, addr))

        case Seq(Address(addr), Y) => const(knownOperandWord(0x27, addr))
        case Seq(IndexedAddress(addr, X), Y) => const(knownOperandWord(0x28, addr))

        case Seq(X, A) => const(noOperand(0x30))
        case Seq(Y, A) => const(noOperand(0x31))
        case Seq(A, X) => const(noOperand(0x32))
        case Seq(A, Y) => const(noOperand(0x33))
      },
      "jmp" -> { case Seq(Symbol(sym)) => relativeReference(0x40, sym, _) },
      "jz" -> { case Seq(Symbol(sym)) => relativeReference(0x41, sym, _) },
      "jnz" -> { case Seq(Symbol(sym)) => relativeReference(0x42, sym, _) },
      "jc" -> { case Seq(Symbol(sym)) => relativeReference(0x43, sym, _) },
      "jnc" -> { case Seq(Symbol(sym)) => relativeReference(0x44, sym, _) }
    )
  }
}

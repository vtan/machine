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

      case Directive("define", Seq(DirectiveArgument.StringArg(symbol), DirectiveArgument.IntArg(value)), _) =>
        if (state.symbolOffsets.contains(symbol)) {
          Left(s"Duplicate symbol: $symbol")
        } else {
          Right(state.copy(symbolOffsets = state.symbolOffsets + (symbol -> value)))
        }
      case Directive("define", _, _) => Left("Invalid arguments")

      case Directive("offset", Seq(DirectiveArgument.IntArg(offset)), _) =>
        if (offset < state.currentOffset) {
          Left(s"Current offset is already ${state.currentOffset}")
        } else if (offset < 0 && offset >= 0xA000) {
          Left(s"Offset must be below 0xA000")
        } else {
          Right(state.copy(
            currentOffset = offset,
            bytecode = state.bytecode.copy(
              bytes = state.bytecode.bytes.padTo(offset, 0)
            )
          ))
        }
      case Directive("offset", _, _) => Left("Invalid arguments")

      case Directive("byte", args, _) =>
        val (invalid, bytes) = args.partitionMap { case DirectiveArgument.IntArg(i) => Right(i); case _ => Left(()) }
        if (invalid.nonEmpty || bytes.exists(n => n < -128 || n > 255)) {
          Left("All arguments must be 1-byte integers")
        } else {
          Right(state.copy(
            currentOffset = state.currentOffset + bytes.length,
            bytecode = state.bytecode.copy(bytes = state.bytecode.bytes ++ bytes)
          ))
        }

      case Directive(directive, _, _) => Left(s"Invalid directive: $directive")
    }

  private val instructions: Map[String, PartialFunction[Seq[Operand], Int => Either[String, Bytecode]]] = {
    import Operand._
    import Function.const
    import Bytecode.{noOperand, fromValue8, fromValue16, fromValueRelative}
    Map(
      "adc" -> {
        case Seq(Immediate(imm)) => fromValue8(0x01, imm)
        case Seq(Address(addr)) => fromValue16(0x02, addr)
      },
      "inc" -> {
        case Seq(A) => const(noOperand(0x09))
        case Seq(X) => const(noOperand(0x0A))
        case Seq(Y) => const(noOperand(0x0B))
        case Seq(Address(addr)) => fromValue16(0x0C, addr)
        case Seq(IndexedAddress(addr, X)) => fromValue16(0x0D, addr)
      },
      "mov" -> {
        case Seq(A, Immediate(imm)) => fromValue8(0x10, imm)
        case Seq(A, Address(addr)) => fromValue16(0x11, addr)
        case Seq(A, IndexedAddress(addr, X)) => fromValue16(0x12, addr)
        case Seq(A, IndexedAddress(addr, Y)) => fromValue16(0x13, addr)
        case Seq(A, IndirectAddress(addr)) => fromValue16(0x14, addr)

        case Seq(X, Immediate(imm)) => fromValue8(0x16, imm)
        case Seq(X, Address(addr)) => fromValue16(0x17, addr)
        case Seq(X, IndexedAddress(addr, Y)) => fromValue16(0x18, addr)

        case Seq(Y, Immediate(imm)) => fromValue8(0x19, imm)
        case Seq(Y, Address(addr)) => fromValue16(0x1A, addr)
        case Seq(Y, IndexedAddress(addr, X)) => fromValue16(0x1B, addr)

        case Seq(Address(addr), A) => fromValue16(0x20, addr)
        case Seq(IndexedAddress(addr, X), A) => fromValue16(0x21, addr)
        case Seq(IndexedAddress(addr, Y), A) => fromValue16(0x22, addr)
        case Seq(IndirectAddress(addr), A) => fromValue16(0x23, addr)

        case Seq(Address(addr), X) => fromValue16(0x25, addr)
        case Seq(IndexedAddress(addr, Y), X) => fromValue16(0x26, addr)

        case Seq(Address(addr), Y) => fromValue16(0x27, addr)
        case Seq(IndexedAddress(addr, X), Y) => fromValue16(0x28, addr)

        case Seq(X, A) => const(noOperand(0x30))
        case Seq(Y, A) => const(noOperand(0x31))
        case Seq(A, X) => const(noOperand(0x32))
        case Seq(A, Y) => const(noOperand(0x33))
      },
      "jmp" -> { case Seq(Immediate(imm)) => fromValueRelative(0x40, imm) },
      "jz" -> { case Seq(Immediate(imm)) => fromValueRelative(0x41, imm) },
      "jnz" -> { case Seq(Immediate(imm)) => fromValueRelative(0x42, imm) },
      "jc" -> { case Seq(Immediate(imm)) => fromValueRelative(0x43, imm) },
      "jnc" -> { case Seq(Immediate(imm)) => fromValueRelative(0x44, imm) },
      "jn" -> { case Seq(Immediate(imm)) => fromValueRelative(0x45, imm) },
      "jnn" -> { case Seq(Immediate(imm)) => fromValueRelative(0x46, imm) },
      "call" -> { case Seq(Immediate(imm)) => fromValue16(0x49, imm) },
      "ret" -> { case Seq() => const(noOperand(0x4A)) }
    )
  }
}

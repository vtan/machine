package machine.assembler

private[assembler]
final case class Bytecode(
  bytes: Seq[Int],
  references: Seq[SymbolReference] = Seq.empty
) {

  def append(rhs: Bytecode): Bytecode =
    Bytecode(
      bytes = bytes ++ rhs.bytes,
      references = references ++ rhs.references
    )
}

private[assembler]
object Bytecode {

  val empty: Bytecode = Bytecode(Seq.empty)

  def noOperand(opcode: Int): Either[String, Bytecode] =
    Right(Bytecode(bytes = Seq(opcode)))

  def fromValue8(opcode: Int, value: Operand.Value)(currentOffset: Int): Either[String, Bytecode] =
    value match {
      case Operand.Literal(literal) =>
        if (literal >= -128 && literal <= 255) {
          Right(Bytecode(bytes = Seq(opcode, literal)))
        } else {
          Left(s"Operand does not fit in a byte ($literal)")
        }
      case Operand.Symbol(symbol) =>
        Right(Bytecode(
          bytes = Seq(opcode, 0),
          references = Seq(AbsoluteReference8(symbol, currentOffset))
        ))
    }

  def fromValue16(opcode: Int, value: Operand.Value)(currentOffset: Int): Either[String, Bytecode] =
    value match {
      case Operand.Literal(literal) =>
        if (literal >= 0 && literal <= 0xFFFF) {
          Right(Bytecode(bytes = opcode +: machine.wordToBytes(literal)))
        } else {
          Left(s"Operand does not fit in a word ($literal)")
        }
      case Operand.Symbol(symbol) =>
        Right(Bytecode(
          bytes = Seq(opcode, 0, 0),
          references = Seq(AbsoluteReference16(symbol, currentOffset))
        ))
    }

  def fromValueRelative(opcode: Int, value: Operand.Value)(currentOffset: Int): Either[String, Bytecode] =
    value match {
      case Operand.Literal(literal) =>
        if (literal >= -128 && literal <= 127) {
          Right(Bytecode(bytes = Seq(opcode, literal)))
        } else {
          Left(s"Operand does not fit in a signed byte ($literal)")
        }
      case Operand.Symbol(symbol) =>
        Right(Bytecode(
          bytes = Seq(opcode, 0),
          references = Seq(RelativeReference(symbol, currentOffset))
        ))
    }
}

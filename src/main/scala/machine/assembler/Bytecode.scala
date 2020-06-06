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

  def knownOperandByte(opcode: Int, operand: Int): Either[String, Bytecode] =
    if (operand >= -128 && operand <= 255) {
      Right(Bytecode(bytes = Seq(opcode, operand)))
    } else {
      Left(s"Operand does not fit in a byte ($operand)")
    }

  def knownOperandWord(opcode: Int, operand: Int): Either[String, Bytecode] =
    if (operand >= 0 && operand <= 0xFFFF) {
      Right(Bytecode(bytes = opcode +: machine.wordToBytes(operand)))
    } else {
      Left(s"Operand does not fit in a word ($operand)")
    }

  def relativeReference(opcode: Int, symbol: String, currentOffset: Int): Either[String, Bytecode] =
    Right(Bytecode(
      bytes = Seq(opcode, 0),
      references = Seq(RelativeReference(symbol, currentOffset))
    ))
}

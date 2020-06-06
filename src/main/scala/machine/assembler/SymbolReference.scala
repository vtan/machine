package machine.assembler

private[assembler]
sealed trait SymbolReference {
  val symbol: String

  def patchWithSymbolOffset(absoluteOffset: Int)(bytes: Seq[Int]): Either[String, Seq[Int]]
}

private[assembler]
final case class AbsoluteReference8(
  symbol: String,
  referringOpcodeOffset: Int
) extends SymbolReference {

  def patchWithSymbolOffset(absoluteOffset: Int)(bytes: Seq[Int]): Either[String, Seq[Int]] =
    Right(bytes.updated(referringOpcodeOffset + 1, absoluteOffset))
}

private[assembler]
final case class AbsoluteReference16(
  symbol: String,
  referringOpcodeOffset: Int
) extends SymbolReference {

  def patchWithSymbolOffset(absoluteOffset: Int)(bytes: Seq[Int]): Either[String, Seq[Int]] = {
    val lo = absoluteOffset & 0xFF
    val hi = absoluteOffset >> 8 & 0xFF
    Right(bytes.updated(referringOpcodeOffset + 1, lo).updated(referringOpcodeOffset + 2, hi))
  }
}

private[assembler]
final case class RelativeReference(
  symbol: String,
  referringOpcodeOffset: Int
) extends SymbolReference {

  def patchWithSymbolOffset(absoluteOffset: Int)(bytes: Seq[Int]): Either[String, Seq[Int]] = {
    val ipAfterInstruction = referringOpcodeOffset + 2
    val distance = absoluteOffset - ipAfterInstruction
    if (distance >= -128 && distance <= 127) {
      Right(bytes.updated(referringOpcodeOffset + 1, distance))
    } else {
      Left(s"Signed relative offset to $symbol does not fit a byte")
    }
  }
}

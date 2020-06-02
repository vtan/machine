package machine.assembler

private[assembler]
sealed trait InstructionBytecode {
  val toSeq: Seq[Int]
}

private[assembler]
final case class Final(toSeq: Int*) extends InstructionBytecode

private[assembler]
object Final {
  def withAbsoluteAddress(opcode: Int, address: Int): Final =
    Final(opcode +: machine.wordToBytes(address): _*)
}

private[assembler]
final case class NeedsRelativeOffset(opcode: Int, symbol: String) extends InstructionBytecode {
  val toSeq: Seq[Int] = Seq(opcode, 0)
}

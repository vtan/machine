package machine.assembler

private[assembler] final case class Instruction(
  operation: String,
  operands: Seq[Operand],
  position: Int
)

private[assembler] sealed trait Operand

private[assembler] object Operand {
  final case class Address(address: Int) extends Operand
  final case class Immediate(value: Int) extends Operand

  sealed trait Register extends Operand
  case object AX extends Register
  case object BX extends Register
}

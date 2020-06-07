package machine.assembler

private[assembler]
sealed trait Command {
  val position: Int
}

private[assembler]
final case class Instruction(
  operation: String,
  operands: Seq[Operand],
  position: Int
) extends Command

private[assembler]
final case class Label(
  symbol: String,
  position: Int
) extends Command

private[assembler]
final case class Directive(
  directive: String,
  arguments: Seq[DirectiveArgument],
  position: Int
) extends Command

private[assembler]
sealed trait Operand

private[assembler]
object Operand {
  final case class Address(value: Value) extends Operand
  final case class IndirectAddress(value: Value) extends Operand
  final case class IndexedAddress(value: Value, register: Register) extends Operand
  final case class IndirectIndexedAddress(value: Value, register: Register) extends Operand
  final case class Immediate(value: Value) extends Operand

  sealed trait Register extends Operand
  case object A extends Register
  case object X extends Register
  case object Y extends Register

  sealed trait Value
  final case class Literal(value: Int) extends Value
  final case class Symbol(value: String) extends Value
}

private[assembler]
sealed trait DirectiveArgument

private[assembler]
object DirectiveArgument {
  final case class IntArg(value: Int) extends DirectiveArgument
  final case class StringArg(value: String) extends DirectiveArgument
}
package machine.assembler

import fastparse._

private[assembler] object ProgramParser {

  def parse(input: String): Either[String, Seq[Instruction]] =
    fastparse.parse(input, program(_), verboseFailures = true) match {
      case Parsed.Success(result, _) => Right(result)
      case failure: Parsed.Failure => Left(failure.msg)
    }

  private implicit val whitespace: P[_] => P[Unit] = { implicit ctx: P[_] =>
    P(CharsWhileIn(" \t", 0) ~~ (";" ~~ CharPred(ch => ch != '\n' && ch != '\r').repX(0)).?)
  }

  private def newlines[_: P]: P[Unit] = CharsWhileIn("\n\r")

  private def program[_: P] : P[Seq[Instruction]] =
    P(newlines.? ~ instruction.?.rep(sep = newlines).map(_.flatten) ~ newlines.? ~ End)

  private def instruction[_: P]: P[Instruction] =
    P(identifier ~ operand.rep(sep = ","./) ~ Index).map((Instruction.apply _).tupled)

  private def operand[_: P]: P[Operand] =
    P(register | addressReference | immediate)

  private def immediate[_: P]: P[Operand] = P(number.map(Operand.Immediate))
  private def addressReference[_: P]: P[Operand] = P("[" ~ number ~ "]").map(Operand.Address)

  private def register[_: P]: P[Operand] =
    P("a".!.map(_ => Operand.A) | "x".!.map(_ => Operand.X) | "y".!.map(_ => Operand.Y))

  private def identifier[_: P]: P[String] = P(CharsWhileIn("a-z").!)
  private def number[_: P]: P[Int] = P(binaryNumber | hexNumber | decimalNumber)

  private def decimalNumber[_: P]: P[Int] =
    P(("0" | (CharIn("1-9_") ~~ CharsWhileIn("0-9_", 0))).!.map(toInt(_, 10)))

  private def binaryNumber[_: P]: P[Int] =
    P("0b" ~~ CharsWhileIn("01_").!.map(toInt(_, 2)))

  private def hexNumber[_: P]: P[Int] =
    P("0x" ~~ CharsWhileIn("0-9a-fA-F_").!.map(toInt(_, 16)))

  private def toInt(str: String, radix: Int): Int = Integer.parseInt(str.filter(_ != '_'), radix)
}

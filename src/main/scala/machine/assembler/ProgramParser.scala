package machine.assembler

import fastparse._

private[assembler] object ProgramParser {

  final case class WithIndex[+T](value: T, index: Int)

  def parse(input: String): Either[String, Seq[WithLocation[Command]]] = {
    val parserInput: ParserInput = input
    fastparse.parse(parserInput, program(_), verboseFailures = true) match {
      case Parsed.Success(commands, _) =>
        val result = commands.map {
          case WithIndex(command, index) => new WithLocation(parserInput.prettyIndex(index), command)
        }
        Right(result)
      case failure: Parsed.Failure => Left(failure.msg)
    }
  }

  private implicit val whitespace: P[_] => P[Unit] = { implicit ctx: P[_] =>
    P(CharsWhileIn(" \t", 0) ~~ (";" ~~ CharPred(ch => ch != '\n' && ch != '\r').repX(0)).?)
  }

  private def newlines[_: P]: P[Unit] = CharsWhileIn("\n\r")

  private def program[_: P] : P[Seq[WithIndex[Command]]] =
    P(line.rep(sep = newlines./).map(_.flatten) ~ End)

  private def line[_: P]: P[Seq[WithIndex[Command]]] =
    P(withIndex(label).? ~ withIndex(directive | instruction).?).map { case (l, i) => l.toSeq ++ i.toSeq }

  private def withIndex[_: P, T](p: P[T]): P[WithIndex[T]] =
    P(p ~~ Index).map((WithIndex.apply[T] _).tupled)

  private def label[_: P]: P[Label] =
    P(identifier ~~ ":").map(Label)

  private def instruction[_: P]: P[Instruction] =
    P(identifier ~ operand.rep(sep = ","./)).map((Instruction.apply _).tupled)

  private def operand[_: P]: P[Operand] =
    P((register ~~ !identifier) | indirectAddressReference | addressReference | immediate)

  private def immediate[_: P]: P[Operand] = P(value.map(Operand.Immediate))

  private def addressReference[_: P]: P[Operand] =
    P("[" ~ value ~ ("+" ~ register).? ~ "]").map {
      case (address, None) => Operand.Address(address)
      case (address, Some(reg)) => Operand.IndexedAddress(address, reg)
    }

  private def indirectAddressReference[_: P]: P[Operand] =
    P("[[" ~ value ~ "]" ~ ("+" ~ register).? ~ "]").map {
      case (address, None) => Operand.IndirectAddress(address)
      case (address, Some(reg)) => Operand.IndirectIndexedAddress(address, reg)
    }

  private def register[_: P]: P[Operand.Register] =
    P("a".!.map(_ => Operand.A) | "x".!.map(_ => Operand.X) | "y".!.map(_ => Operand.Y))

  private def value[_: P]: P[Operand.Value] =
    P(number.map(Operand.Literal) | identifier.map(Operand.Symbol))

  private def directive[_: P]: P[Directive] =
    P("." ~~/ identifier ~/ directiveArgument.rep).map((Directive.apply _).tupled)

  private def directiveArgument[_: P]: P[DirectiveArgument] =
    P(number.map(DirectiveArgument.IntArg) | identifier.map(DirectiveArgument.StringArg))

  private def identifier[_: P]: P[String] =
    P((CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z_0-9")).!)

  private def number[_: P]: P[Int] = P(binaryNumber | hexNumber | decimalNumber)

  private def decimalNumber[_: P]: P[Int] =
    P(("0" | (CharIn("1-9_") ~~ CharsWhileIn("0-9_", 0))).!.map(toInt(_, 10)))

  private def binaryNumber[_: P]: P[Int] =
    P("0b" ~~ CharsWhileIn("01_").!.map(toInt(_, 2)))

  private def hexNumber[_: P]: P[Int] =
    P("0x" ~~ CharsWhileIn("0-9a-fA-F_").!.map(toInt(_, 16)))

  private def toInt(str: String, radix: Int): Int = Integer.parseInt(str.filter(_ != '_'), radix)
}

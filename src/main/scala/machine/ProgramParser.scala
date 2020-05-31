package machine

import scala.util.Try

object ProgramParser {

  private val mnemonics: Set[String] = Set("add")

  private def parseNumber(str: String, radix: Int): Option[Int] =
    Try(Integer.parseInt(str, radix)).toOption

  sealed trait OperandToken

  object OperandToken {
    def unapply(str: String): Option[OperandToken] =
      str match {
        case ParsedImmediate(o) => Some(o)
        case ParsedRegister(o) => Some(o)
        case ParsedMemoryReference(o) => Some(o)
        case _ => None
      }
  }

  final case class Immediate(value: Int) extends OperandToken

  object ParsedImmediate {
    private val dec = "(0|[1-9][0-9_]*)".r
    private val bin = "0b([01_]+)".r
    private val hex = "0x([0-9A-Fa-f_]+)".r

    def unapply(str: String): Option[Immediate] = (
      dec.unapplySeq(str).flatMap(_.headOption).map(_.filter(_ != '_')).flatMap(parseNumber(_, 10)) orElse
      bin.unapplySeq(str).flatMap(_.headOption).map(_.filter(_ != '_')).flatMap(parseNumber(_, 2)) orElse
      hex.unapplySeq(str).flatMap(_.headOption).map(_.filter(_ != '_')).flatMap(parseNumber(_, 16))
    ).map(Immediate)
  }

  final case class Register(address: Int) extends OperandToken

  object ParsedRegister {
    private val registers: Map[String, Int] = Map(
      "ax" -> Machine.axAddr,
      "bx" -> Machine.bxAddr
    )

    def unapply(str: String): Option[Register] =
      registers.get(str).map(Register)
  }

  final case class MemoryReference(address: Int) extends OperandToken

  object ParsedMemoryReference {
    def unapply(str: String): Option[MemoryReference] =
      if (str.startsWith("[") && str.endsWith("]")) {
        ParsedImmediate.unapply(str.drop(1).dropRight(1).trim).map(i => MemoryReference(i.value))
      } else {
        None
      }
  }

  def parse(code: String): Either[String, Machine] = {
    val (errors, instructions) = code.linesIterator.zipWithIndex.toSeq.partitionMap {
      case (line, lineNumber) =>
        parseLine(line).fold(error => Left(s"Line ${lineNumber + 1}: $error"), Right(_))
    }
    errors match {
      case Seq() =>
        val bytes = instructions.flatten.toVector
        val zeroes = Vector.fill(0x10000 - bytes.length)(0)
        Right(Machine(bytes ++ zeroes))
      case firstError +: _ =>
        Left(firstError)
    }
  }

  def parseLine(line: String): Either[String, Vector[Int]] = {
    val (mnemonic, rest) = line.trim.takeWhile(_ != ';').span(!_.isWhitespace)
    if (mnemonics.contains(mnemonic)) {
      val words = rest.trim.split(",\\s*").toSeq
      val (errors, operands) = words.partitionMap {
        case OperandToken(operand) => Right(operand)
        case unknown => Left(s"Invalid operand: $unknown")
      }
      errors match {
        case Seq() =>
          mnemonic match {
            case "add" =>
              operands match {
                case Seq(op1, op2) => assemble(mnemonic, op1, op2)
                case _ => Left("Two operands required")
              }
          }
        case firstError +: _ => Left(firstError)
      }
    } else {
      Left("Invalid mnemonic")
    }
  }

  private def assemble(mnemonic: String, operand1: OperandToken, operand2: OperandToken): Either[String, Vector[Int]] = {
    import TwoOperandInstruction._
    val opcode = mnemonic match {
      case "add" => 0x01
    }
    val operands = (operand1, operand2) match {
      case (Register(dest), MemoryReference(src)) => Right(RegMem(dest, src))
      case (MemoryReference(dest), Register(src)) => Right(MemReg(dest, src))
      case (Register(dest), Register(src)) => Right(RegReg(dest, src))
      case (Register(dest), Immediate(src)) => Right(RegImm(dest, src))
      case _ => Left("Invalid operands for instruction")
    }
    operands.map {
      case RegMem(reg, mem) =>
        val discriminator = reg << 2
        opcode +: discriminator +: wordToBytes(mem)

      case MemReg(mem, reg) =>
        val discriminator = reg << 2 | 1
        opcode +: discriminator +: wordToBytes(mem)

      case RegReg(reg1, reg2) =>
        val discriminator = reg2 << 4 | reg1 << 2 | 2
        Vector(opcode, discriminator)

      case RegImm(reg, imm) =>
        val discriminator = reg << 2 | 3
        opcode +: discriminator +: wordToBytes(imm)
    }
  }
}

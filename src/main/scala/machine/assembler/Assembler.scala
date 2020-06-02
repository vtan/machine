package machine.assembler

import machine.Machine

object Assembler {

  def assemble(input: String): Either[String, Machine] =
    for {
      commands <- ProgramParser.parse(input)
      bytes <- BytecodeBuilder.build(commands)
    } yield {
      val padding = Vector.fill(0x10000 - bytes.length)(0)
      Machine(bytes ++ padding)
    }
}

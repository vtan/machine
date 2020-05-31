package machine

sealed trait Instruction

object Instruction {
  val allByOpcodeHead: Map[Int, TwoOperandInstruction] = Map(
    0x01 -> TwoOperandInstruction {
      (operands, machine) =>
        val (dest, src) = operands match {
          case TwoOperandInstruction.RegMem(d, s) => (machine.registers(d), machine.memory(s))
          case TwoOperandInstruction.MemReg(d, s) => (machine.memory(d), machine.registers(s))
          case TwoOperandInstruction.RegReg(d, s) => (machine.registers(d), machine.registers(s))
          case TwoOperandInstruction.RegImm(d, s) => (machine.registers(d), s)
        }
        val newDest = dest + src
        operands match {
          case TwoOperandInstruction.RegMem(d, _) => machine.copy(registers = machine.registers.updated(d, newDest))
          case TwoOperandInstruction.RegReg(d, _) => machine.copy(registers = machine.registers.updated(d, newDest))
          case TwoOperandInstruction.RegImm(d, _) => machine.copy(registers = machine.registers.updated(d, newDest))
          case TwoOperandInstruction.MemReg(d, _) => machine.copy(memory = machine.memory.updated(d, newDest & 0xFF).updated(d + 1, (newDest >> 8) & 0xFF))
        }
    }
  )
}

final case class TwoOperandInstruction(
  operation: (TwoOperandInstruction.Operands, Machine) => Machine
) extends Instruction

object TwoOperandInstruction {
  sealed trait Operands
  final case class RegMem(dest: Int, src: Int) extends Operands
  final case class MemReg(dest: Int, src: Int) extends Operands
  final case class RegReg(dest: Int, src: Int) extends Operands
  final case class RegImm(dest: Int, src: Int) extends Operands
}

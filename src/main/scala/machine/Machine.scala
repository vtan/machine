package machine

import scala.scalajs.js.typedarray.Uint8Array

class Machine(
  val memory: Uint8Array,
  val registers: Uint8Array,
  var error: Option[String]
) {
  def ax: Short = registers(Machine.axAddr)
  def bx: Short = registers(Machine.bxAddr)
  def ip: Short = registers(Machine.ipAddr)

  def step(): Unit =
    if (error.isEmpty) {
      val opcodeHead = memory(ip)
      if (opcodeHead == 0x01) {
        registers(Machine.axAddr) = (registers(Machine.axAddr) + 1).toShort
        registers(Machine.ipAddr) = (registers(Machine.ipAddr) + 1).toShort
      } else {
        error = Some(s"Invalid opcode: $opcodeHead")
      }
    }
}

object Machine {

  def apply(memory: Seq[Int]): Machine = new Machine(
    memory = Uint8Array.of(memory.map(_.toShort): _*),
    registers = new Uint8Array(length = 3),
    error = None
  )

  private val axAddr: Int = 0
  private val bxAddr: Int = 1
  private val ipAddr: Int = 2
}

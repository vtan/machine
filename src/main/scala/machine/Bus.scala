package machine

import scala.scalajs.js.typedarray.Uint8Array

class Bus(
  val memory: Uint8Array,
  var display: Option[Display]
) {

  def read(address: Int): Short = {
    val addressInRange = address & 0xFFFF
    memory(addressInRange)
  }

  def write(address: Int, byte: Short): Unit = {
    val addressInRange = address & 0xFFFF
    memory(addressInRange) = byte

    if (addressInRange >= 0xE000) {
      display.foreach(_.write(addressInRange - 0xE000, byte))
    }
  }
}

object Bus {
  def apply(memory: Seq[Int], display: Option[Display]): Bus = new Bus(
    memory = Uint8Array.of(memory.map(_.toShort): _*),
    display = display
  )
}

package machine

class Machine(
  val bus: Bus,
  val cpu: Cpu
)

object Machine {
  def apply(memory: Seq[Int], display: Option[Display] = None): Machine = {
    val bus = Bus(memory, display)
    val cpu = new Cpu(bus)
    new Machine(bus, cpu)
  }
}

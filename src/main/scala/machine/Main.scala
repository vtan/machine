package machine

import machine.assembler.Assembler

import com.thoughtworks.binding.Binding
import com.thoughtworks.binding.Binding.Var
import org.lrng.binding.html
import org.scalajs.dom
import org.scalajs.dom.raw.Event
import org.scalajs.dom.Node
import org.scalajs.dom.html.TextArea

class State(
  private var machine: Machine,
  val code: Var[String],
  val codeError: Var[Option[String]],
) {

  val machineError: Var[Option[String]] = Var(machine.error)
  val memory: Var[String] = Var(MemoryPrinter.print(machine.memory))
  val registers: Var[Map[String, Int]] = Var(machine.registers)

  def setCode(input: String): Unit =
    code.value = input.toLowerCase

  def loadCode(): Unit =
    Assembler.assemble(code.value) match {
      case Left(error) =>
        codeError.value = Some(error)
        machine = Machine(Nil)
        machineUpdated()
      case Right(parsed) =>
        codeError.value = None
        machine = parsed
        machineUpdated()
        dom.window.localStorage.setItem("code", code.value)
    }

  def stepMachine(): Unit = {
    machine.step()
    machineUpdated()
  }

  private def machineUpdated(): Unit = {
    machineError.value = machine.error
    memory.value = MemoryPrinter.print(machine.memory)
    registers.value = machine.registers
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    val savedCode = Option(dom.window.localStorage.getItem("code"))
    val state = new State(
      machine = Machine(Vector.fill(0x10000)(0)),
      code = Var(savedCode.getOrElse("")),
      codeError = Var(None)
    )
    html.render(dom.document.body, mainContainer(state))
  }

  @html
  def mainContainer(state: State): Binding[Node] = {
    <div class="mainContainer">
      { codePanel(state).bind }
      { machinePanel(state).bind }
    </div>
  }

  @html
  def codePanel(state: State): Binding[Node] = {
    <div class="codePanel">
      <textarea onchange={ (e: Event) => state.setCode(e.target.asInstanceOf[TextArea].value) }>{ state.code.bind }</textarea>
      <div>
        { state.codeError.bind match {
            case Some(error) => <strong>{ error }</strong>
            case None => <span/>
        } }
      </div>
      <button onclick={ (_: Event) => state.loadCode() }>Load</button>
    </div>
  }

  @html
  def machinePanel(state: State): Binding[Node] = {
    <div class="machinePanel">
      {
        val machineError = state.machineError.bind
        val memory = state.memory.bind
        val registers = state.registers.bind
        <pre>{ memory }</pre>
        <p>
          { machineError match {
              case Some(error) => <strong>{ error }</strong>
              case None => <span/>
          } }
        </p>
        <p><strong>IP</strong> { String.format("%04X", registers("ip")) }</p>
        <p><strong>A</strong> { String.format("%02X", registers("a")) }</p>
        <button onclick={ (_: Event) => state.stepMachine() }>Step</button>
      }
    </div>
  }
}

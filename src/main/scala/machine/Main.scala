package machine

import machine.assembler.Assembler
import com.thoughtworks.binding.Binding
import com.thoughtworks.binding.Binding.Var
import org.lrng.binding.html
import org.scalajs.dom
import org.scalajs.dom.Node
import org.scalajs.dom.html.TextArea
import org.scalajs.dom.raw.{Event, HTMLCanvasElement}

class State(
  private var machine: Machine,
  val code: Var[String],
  val codeError: Var[Option[String]],
) {

  val machineError: Var[Option[String]] = Var(machine.cpu.error)
  val memory: Var[String] = Var(MemoryPrinter.print(machine.bus.memory))
  val registers: Var[Map[String, Int]] = Var(machine.cpu.registers)

  private var canvas: Option[HTMLCanvasElement] = None

  def setCode(input: String): Unit =
    code.value = input.toLowerCase

  def loadCode(): Unit =
    Assembler.assemble(code.value) match {
      case Left(error) =>
        codeError.value = Some(error)
        machine = Machine(memory = Nil)
        machineUpdated()
      case Right(parsed) =>
        codeError.value = None
        machine = parsed
        machine.bus.display = canvas.map(Display(_))
        machineUpdated()
        dom.window.localStorage.setItem("code", code.value)
    }

  def stepMachine(): Unit = {
    machine.cpu.step()
    machineUpdated()
  }

  def canvasChanged(canvas: HTMLCanvasElement): Unit = {
    this.canvas = Some(canvas)
    machine.bus.display = Some(Display(canvas))
  }

  private def machineUpdated(): Unit = {
    machineError.value = machine.cpu.error
    memory.value = MemoryPrinter.print(machine.bus.memory)
    registers.value = machine.cpu.registers
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
        val canvas = <canvas id="displayCanvas" width="128" height="128" />
        state.canvasChanged(canvas.bind)
        canvas
      }
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
        <p><strong>X</strong> { String.format("%02X", registers("x")) }</p>
        <p><strong>Y</strong> { String.format("%02X", registers("y")) }</p>
        <p><strong>FLAGS</strong> { String.format("%02X", registers("flags")) }</p>
        <button onclick={ (_: Event) => state.stepMachine() }>Step</button>
      }
    </div>
  }
}

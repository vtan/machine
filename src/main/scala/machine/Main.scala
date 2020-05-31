package machine

import com.thoughtworks.binding.Binding
import com.thoughtworks.binding.Binding.Var
import org.lrng.binding.html
import org.scalajs.dom
import org.scalajs.dom.raw.Event
import org.scalajs.dom.Node
import org.scalajs.dom.html.TextArea

object Main {

  def main(args: Array[String]): Unit = {
    val codeInput: Var[String] = Var("")
    val codeError: Var[Option[String]] = Var(None)
    val machine: Var[Machine] = Var(Machine(Vector.fill(0x10000)(0)))
    html.render(dom.document.body, mainContainer(codeInput, codeError, machine))
  }

  @html
  def mainContainer(code: Var[String], codeError: Var[Option[String]], machine: Var[Machine]): Binding[Node] = {
    <div class="mainContainer">
      { codePanel(code, codeError, machine).bind }
      { machinePanel(machine).bind }
    </div>
  }

  @html
  def codePanel(code: Var[String], codeError: Var[Option[String]], machine: Var[Machine]): Binding[Node] = {
    def onLoad(e: Event): Unit =
      ProgramParser.parse(code.value) match {
        case Left(error) =>
          codeError.value = Some(error)
        case Right(parsed) =>
          codeError.value = None
          machine.value = parsed
      }

    <div class="codePanel">
      <textarea onchange={ (e: Event) => code.value = e.target.asInstanceOf[TextArea].value }>{ code.bind }</textarea>
      <div>
        { codeError.bind match {
            case Some(error) => <strong>{ error }</strong>
            case None => <span/>
        } }
      </div>
      <button onclick={ onLoad _ }>Load</button>
    </div>
  }

  @html
  def machinePanel(machineVar: Var[Machine]): Binding[Node] = {
    def onStep(): Unit = machineVar.value.step.foreach(machineVar.value = _)

    <div class="machinePanel">
      {
        val machine = machineVar.bind
        <pre>{ MemoryPrinter.print(machine.memory) }</pre>
        <p>
          { machine.error match {
              case Some(error) => <strong>{ error }</strong>
              case None => <span/>
          } }
        </p>
        <p><strong>IP</strong> { String.format("%04X", machine.ip) }</p>
        <p><strong>AX</strong> { String.format("%04X", machine.ax) }</p>
        <p><strong>BX</strong> { String.format("%04X", machine.bx) }</p>
        <button onclick={ (_: Event) => onStep() }>Step</button>
      }
    </div>
  }
}

package machine

import machine.assembler.Assembler

import com.thoughtworks.binding.Binding
import com.thoughtworks.binding.Binding.Var
import org.lrng.binding.html
import org.scalajs.dom
import org.scalajs.dom.raw.Event
import org.scalajs.dom.Node
import org.scalajs.dom.html.TextArea

object Main {

  trait Dispatch {
    def setCode(input: String): Unit
    def loadCode(): Unit
    def stepMachine(): Unit
  }

  class State(
    private var machine: Machine,
    val code: Var[String],
    val codeError: Var[Option[String]],
  ) extends Dispatch {

    val machineError: Var[Option[String]] = Var(machine.error)
    val memory: Var[String] = Var(MemoryPrinter.print(machine.memory))
    val registers: Var[Map[String, Int]] = Var(Map("ax" -> machine.ax, "bx" -> machine.bx, "ip" -> machine.ip))

    def setCode(input: String): Unit =
      code.value = input

    def loadCode(): Unit =
      Assembler.assemble(code.value) match {
        case Left(error) =>
          codeError.value = Some(error)
        case Right(parsed) =>
          codeError.value = None
          machine = parsed
          machineUpdated()
          dom.window.localStorage.setItem("code", code.value)
      }

    def stepMachine(): Unit =
      machine.step.foreach { newMachine =>
        machine = newMachine
        machineUpdated()
      }

    private def machineUpdated(): Unit = {
      machineError.value = machine.error
      memory.value = MemoryPrinter.print(machine.memory)
      registers.value = Map("ax" -> machine.ax, "bx" -> machine.bx, "ip" -> machine.ip)
    }
  }

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
      { codePanel(state.code.bind, state.codeError.bind, state) }
      { machinePanel(state.machineError.bind, state.memory.bind, state.registers.bind, state) }
    </div>
  }

  @html
  def codePanel(
    code: String,
    codeError: Option[String],
    dispatch: Dispatch
  ) = {
    <div class="codePanel">
      <textarea onchange={ (e: Event) => dispatch.setCode(e.target.asInstanceOf[TextArea].value) }>
        { code }
      </textarea>
      <div>
        { codeError match {
            case Some(error) => <strong>{ error }</strong>
            case None => <span/>
        } }
      </div>
      <button onclick={ (_: Event) => dispatch.loadCode() }>Load</button>
    </div>
  }

  @html
  def machinePanel(
    machineError: Option[String],
    memory: String,
    registers: Map[String, Int],
    dispatch: Dispatch
  ) = {
    <div class="machinePanel">
      <pre>{ memory }</pre>
      <p>
        { machineError match {
            case Some(error) => <strong>{ error }</strong>
            case None => <span/>
        } }
      </p>
      <p><strong>IP</strong> { String.format("%04X", registers("ip")) }</p>
      <p><strong>AX</strong> { String.format("%04X", registers("ax")) }</p>
      <p><strong>BX</strong> { String.format("%04X", registers("bx")) }</p>
      <button onclick={ (_: Event) => dispatch.stepMachine() }>Step</button>
    </div>
  }
}

package machine

import org.scalajs.dom.raw.{CanvasRenderingContext2D, HTMLCanvasElement}

class Display(context: CanvasRenderingContext2D) {
  context.fillStyle = "black"
  context.fillRect(0, 0, context.canvas.width.toDouble, context.canvas.height.toDouble)

  def write(address: Int, byte: Short): Unit = {
    val x = (address & 0xFF).toDouble
    val y = 0.0
    context.fillStyle = if (byte == 0) "black" else "white"
    context.fillRect(x, y, w = 1, h = 1)
  }
}

object Display {
  def apply(canvas: HTMLCanvasElement): Display =
    canvas.getContext("2d") match {
      case context: CanvasRenderingContext2D => new Display(context)
      case _ => sys.error("No 2D context for canvas")
    }
}

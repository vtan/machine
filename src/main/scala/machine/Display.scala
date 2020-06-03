package machine

import org.scalajs.dom.raw.{CanvasRenderingContext2D, HTMLCanvasElement}

class Display(context: CanvasRenderingContext2D) {
  context.fillStyle = Display.colors(0)
  context.fillRect(0, 0, context.canvas.width.toDouble, context.canvas.height.toDouble)

  def write(address: Int, byte: Short): Unit = {
    val x = ((address & 0x3F) << 1).toDouble  // 128 pixels in a row, 2 pixels per byte
    val y = (address >> 6 & 0x7F).toDouble    // 128 rows
    val lowColor = Display.colors(byte & 0xF)
    val highColor = Display.colors(byte >> 4 & 0xF)
    context.fillStyle = lowColor
    context.fillRect(x, y, w = 1, h = 1)
    context.fillStyle = highColor
    context.fillRect(x + 1, y, w = 1, h = 1)
  }
}

object Display {
  def apply(canvas: HTMLCanvasElement): Display =
    canvas.getContext("2d") match {
      case context: CanvasRenderingContext2D => new Display(context)
      case _ => sys.error("No 2D context for canvas")
    }

  private val colors: Array[String] = Array(
    "#000", "#AAA", "#00A", "#0A0", "#0AA", "#A00", "#A0A", "#A50",
    "#555", "#FFF", "#55F", "#5F5", "#5FF", "#F55", "#F5F", "#FF5",
  )
}

package machine

object MemoryPrinter {

  def print(bytes: Iterable[Short]): String = {
    val stringBuilder = new StringBuilder
    var prevEmpty = false
    var i = 0
    bytes.grouped(16).foreach { group =>
      if (group.forall(_ == 0)) {
        if (!prevEmpty) {
          stringBuilder ++= "...\n"
          prevEmpty = true
        }
      } else {
        val offset = i * 16
        stringBuilder.append(String.format("0x%04X   ", offset))
        group.foreach { byte =>
          stringBuilder.append(String.format(" %02X", byte))
        }
        stringBuilder.append("\n")
      }
      i += 1
    }
    stringBuilder.result
  }
}

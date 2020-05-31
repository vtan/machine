package machine

object MemoryPrinter {

  def print(bytes: Seq[Int]): String = {
    bytes.grouped(16).zipWithIndex.foldLeft((Vector.empty[String], false: Boolean)){
      case ((lines, prevEmpty), (group, index)) =>
        if (group.forall(_ == 0)) {
          if (prevEmpty) {
            (lines, true)
          } else {
            (lines :+ "...", true)
          }
        } else {
          val offset = index * 16
          val str = new StringBuilder
          str.append(String.format("0x%04X   ", offset))
          group.foreach { byte =>
            str.append(String.format(" %02X", byte))
          }
          (lines :+ str.result, false)
        }
    }._1.mkString("\n")
  }
}

package object machine {

  def wordToBytes(word: Int): Vector[Int] =
    Vector(word & 0xFF, word >> 8 & 0xFF)
}

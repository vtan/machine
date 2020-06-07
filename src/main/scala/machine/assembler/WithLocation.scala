package machine.assembler

final class WithLocation[T](
  lazyLocation: => String,
  val value: T
) {
  lazy val location: String = lazyLocation
}

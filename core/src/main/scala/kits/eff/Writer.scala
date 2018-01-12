package kits.eff

enum class Writer[W] extends Effect

object Writer {
  def tell[W](value: W): Eff[Writer[W], Unit] = Eff(new Put(value))

  def run[W]: Handler { type Result[A] = (Vector[W], A); type Union[R] = Writer[W] | R } =
    Handler(Vector.empty[W], new HandleRelayS[Vector[W], Writer[W]] {
      type Result[A] = (Vector[W], A)
      def pure[R, A](s: Vector[W], a: A) = Eff.Pure((s, a))
      def bind[R, A, B](s: Vector[W], fa: Writer[W] { type Value = A })(k: (Vector[W], A) => Eff[R, (Vector[W], B)]) =
        fa match {
          case Put(v) => k(s :+ v, ())
        }
      def isInstance(fa: Any) = classOf[Writer[W]].isInstance(fa)
    })

  case Put[W](value: W) extends Writer[W] {
    type Value = Unit
  }
}

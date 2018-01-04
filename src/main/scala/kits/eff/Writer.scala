package kits.eff

enum class Writer[W] extends Effect

object Writer {
  def run[W]: Handler { type Result[A] = (List[W], A); type Union[R] = Writer[W] | R } =
    new HandleRelay[Writer[W]](classOf[Writer[W]]) {
      type Result[A] = (List[W], A)
      def pure[R, A](a: A) = Eff.Pure((Nil, a))
      def bind[R, A, B](fa: Writer[W] { type Value = A })(k: A => Eff[R, (List[W], B)]) =
        fa match {
          case Tell(v) => k(()).map {
            case (w, a) => (v :: w, a)
          }
        }
    }

  def tell[W](value: W): Eff[Writer[W], Unit] = Eff(new Tell(value))

  case Tell[W](value: W) extends Writer[W] {
    type Value = Unit
  }
}

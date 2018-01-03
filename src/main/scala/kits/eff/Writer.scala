package kits.eff

class Writer[W] extends Effect

object Writer {
  case class Tell[W](value: W) extends Writer[W] {
    type Result = Unit
  }

  def run[W]: Eff.Handler[Writer[W]] =
    new Eff.Handler[Writer[W]] {
      type Result[A] = (List[W], A)
      def pure[R, A](a: A) = Eff.Pure((Nil, a))
      def bind[R, A, B](fa: Writer[W] { type Result = A })(k: A => Eff[R, (List[W], B)]) =
        fa match {
          case Tell(v) => k(().asInstanceOf[fa.Result]).map {
            case (w, a) => (v :: w, a)
          }
        }
    }

  def tell[R, W](value: W)(implicit F: Writer[W] <:< R): Eff[R, Unit] = Eff(Tell(value))
}

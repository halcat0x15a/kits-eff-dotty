package kits.eff

enum class Reader[I] extends Effect

object Reader {
  def run[I](value: I): Eff.Handler[Reader[I]] =
    new Eff.Handler[Reader[I]] {
      type Result[A] = A
      def pure[R, A](a: A) = Eff.Pure(a)
      def bind[R, A, B](fa: Reader[I] { type Result = A })(k: A => Eff[R, B]) =
        fa match {
          case Ask() => k(value)
        }
    }

  def ask[R, I](implicit F: Reader[I] <:< R): Eff[R, I] = Eff(new Ask[I])

  case Ask[I]() extends Reader[I] {
    type Result = I
  }
}

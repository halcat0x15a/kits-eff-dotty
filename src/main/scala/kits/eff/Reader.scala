package kits.eff

enum class Reader[I] extends Effect

object Reader {
  def run[I](value: I): Handler { type Result[A] = A; type Union[R] = Reader[I] | R } =
    new HandleRelay[Reader[I]](classOf[Reader[I]]) {
      type Result[A] = A
      def pure[R, A](a: A) = Eff.Pure(a)
      def bind[R, A, B](fa: Reader[I] { type Value = A })(k: A => Eff[R, B]) =
        fa match {
          case Ask() => k(value)
        }
    }

  def ask[I]: Eff[Reader[I], I] = Eff(new Ask[I])

  def local[I, R >: Reader[I], A](f: I => I)(eff: Eff[R, A]): Eff[R, A] =
    ask[I].flatMap { r0 =>
      val r = f(r0)
      run(r)(eff)
    }

  case Ask[I]() extends Reader[I] {
    type Value = I
  }
}

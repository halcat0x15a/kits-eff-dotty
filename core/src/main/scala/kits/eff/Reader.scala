package kits.eff

enum class Reader[I] extends Effect

object Reader {
  def ask[I]: Eff[Reader[I], I] = Eff(new Get[I])

  def local[I, R >: Reader[I], A](f: I => I)(eff: Eff[R, A]): Eff[R, A] =
    ask[I].flatMap { r0 =>
      val r = f(r0)
      run(r)(eff)
    }

  def run[I](value: I): Handler { type Result[A] = A; type Union[R] = Reader[I] | R } =
    Handler(new HandleRelay[Reader[I]] {
      type Result[A] = A
      def pure[R, A](a: A) = Eff.Pure(a)
      def bind[R, A, B](fa: Reader[I] { type Value = A })(k: A => Eff[R, B]) =
        fa match {
          case Get() => k(value)
        }
      def isInstance(fa: Any) = classOf[Reader[I]].isInstance(fa)
    })

  case Get[I]() extends Reader[I] {
    type Value = I
  }
}

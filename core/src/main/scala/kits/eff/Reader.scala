package kits.eff

enum Reader[I, A] {
  case Get[I]() extends Reader[I, I]
}

object Reader {
  def ask[I]: Eff[[A] => Reader[I, A], I] = Eff(Get())

  def local[I, R[a] >: Reader[I, a], A](f: I => I)(eff: Eff[R, A]): Eff[R, A] =
    ask.flatMap { r0 =>
      val r = f(r0)
      run(r)(eff)
    }

  def run[R[_], I, A](value: I)(eff: Eff[[A] => Reader[I, A] | R[A], A]): Eff[R, A] =
    Eff.handleRelay(eff)(new Eff.Handler[[A] => Reader[I, A], R, A, A] {
      def pure(a: A): Eff[R, A] = Eff.Pure(a)
      def flatMap[T](fa: Reader[I, T])(k: T => Eff[R, A]) =
        fa match {
          case Reader.Get() => k(value)
        }
    })
}

package kits.eff

object State {
  def run[R[_], S, A](state: S)(eff: Eff[[A] => State[S, A] | R[A], A]): Eff[R, (S, A)] =
    Eff.handleRelayS[[A] => State[S, A], R, S, A, (S, A)](state, eff)(new Eff.HandlerS[[A] => State[S, A], R, S, A, (S, A)] {
      def pure(s: S, a: A) = Eff.Pure((s, a))
      def flatMap[T](s: S, fa: State[S, T])(k: (S, T) => Eff[R, (S, A)]) =
        fa match {
          case Reader.Get() => k(s, s)
          case Writer.Put(s) => k(s, ())
        }
    })
}

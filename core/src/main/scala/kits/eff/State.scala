package kits.eff

object State {
  def run[S](state: S): Handler { type Result[A] = (S, A); type Union[R] = State[S] | R } =
    Handler(state, new HandleRelayS[S, Reader[S] | Writer[S]] {
      type Result[A] = (S, A)
      def pure[R, A](s: S, a: A) = Eff.Pure((s, a))
      def bind[R, A, B](s: S, fa: (Reader[S] | Writer[S]) { type Value = A })(k: (S, A) => Eff[R, (S, B)]) =
        fa match {
          case Reader.Get() => k(s, s)
          case Writer.Put(s) => k(s, ())
        }
      def isInstance(fa: Any) = classOf[Reader[S]].isInstance(fa) || classOf[Writer[S]].isInstance(fa)
    })
}

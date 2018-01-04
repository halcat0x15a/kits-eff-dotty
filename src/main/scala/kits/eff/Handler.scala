package kits.eff

trait Handler { self =>
  type Result[A]

  type Union[R]

  def apply[R, A](eff: Eff[Union[R], A]): Eff[R, Result[A]]

  def compose(that: Handler): Handler { type Result[A] = self.Result[that.Result[A]]; type Union[R] = that.Union[self.Union[R]] } =
    new Handler {
      type Result[A] = self.Result[that.Result[A]]
      type Union[R] = that.Union[self.Union[R]]
      def apply[R, A](eff: Eff[Union[R], A]): Eff[R, Result[A]] = self(that(eff))
    }
}

trait HandleRelay[F <: Effect](tag: Class[F]) extends Handler {
  type Result[A]

  type Union[R] = F | R

  def pure[R, A](a: A): Eff[R, Result[A]]

  def bind[R, A, B](fa: F { type Value = A })(k: A => Eff[R, Result[B]]): Eff[R, Result[B]]

  def apply[R, A](eff: Eff[F | R, A]): Eff[R, Result[A]] = {
    eff match {
      case Eff.Pure(v) => pure(v)
      case Eff.Impure(u, k) => u match {
        case fa: F if tag.isInstance(fa) =>
          bind(fa)(a => apply(k(a)))
        case r: R =>
          Eff.Impure[R, Any, Result[A]](r, Arrs.Leaf((a: Any) => apply(k(a))))
      }
    }
  }
}

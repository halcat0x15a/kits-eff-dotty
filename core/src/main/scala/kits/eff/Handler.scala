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

object Handler {
  def apply[F <: Effect](handleRelay: HandleRelay[F]): Handler { type Result[A] = handleRelay.Result[A]; type Union[R] = F | R } =
    new Handler {
      type Result[A] = handleRelay.Result[A]
      type Union[R] = F | R
      def apply[R, A](eff: Eff[F | R, A]): Eff[R, Result[A]] =
        eff match {
          case Eff.Pure(v) => handleRelay.pure(v)
          case Eff.Lazy(v, k) => Eff.Lazy(v, Arrs.Leaf(a => apply(k(a))))
          case Eff.Impure(u, k) => u match {
            case fa: F if handleRelay.isInstance(fa) =>
              handleRelay.bind(fa)(a => Eff.Lazy(a, Arrs.Leaf(a => apply(k(a)))))
            case r: R =>
              Eff.Impure[R, Any, Result[A]](r, Arrs.Leaf(a => apply(k(a))))
          }
        }
    }

  def apply[S, F <: Effect](s: S, handleRelayS: HandleRelayS[S, F]): Handler { type Result[A] = handleRelayS.Result[A]; type Union[R] = F | R } =
    new Handler {
      type Result[A] = handleRelayS.Result[A]
      type Union[R] = F | R
      def apply[R, A](eff: Eff[F | R, A]): Eff[R, Result[A]] = {
        def go(s: S, eff: Eff[F | R, A]): Eff[R, Result[A]] =
          eff match {
            case Eff.Pure(v) => handleRelayS.pure(s, v)
            case Eff.Lazy(v, k) => Eff.Lazy(v, Arrs.Leaf(a => go(s, k(a))))
            case Eff.Impure(u, k) => u match {
              case fa: F if handleRelayS.isInstance(fa) =>
                handleRelayS.bind(s, fa)((s, a) => Eff.Lazy(a, Arrs.Leaf(a => go(s, k(a)))))
              case r: R =>
                Eff.Impure[R, Any, Result[A]](r, Arrs.Leaf((a: Any) => go(s, k(a))))
            }
          }
        go(s, eff)
      }
    }
}

trait HandleRelay[F <: Effect] {
  type Result[A]

  def pure[R, A](a: A): Eff[R, Result[A]]

  def bind[R, A, B](fa: F { type Value = A })(k: A => Eff[R, Result[B]]): Eff[R, Result[B]]

  def isInstance(fa: Any): Boolean
}

trait HandleRelayS[S, F <: Effect] {
  type Result[A]

  def pure[R, A](s: S, a: A): Eff[R, Result[A]]

  def bind[R, A, B](s: S, fa: F { type Value = A })(k: (S, A) => Eff[R, Result[B]]): Eff[R, Result[B]]

  def isInstance(fa: Any): Boolean
}

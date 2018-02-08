package kits.eff

import scala.annotation.tailrec

sealed abstract class Eff[+R[_], A] {
  def map[B](f: A => B): Eff[R, B]

  def flatMap[S[_], B](f: A => Eff[S, B]): Eff[[A] => R[A] | S[A], B]
}

object Eff {
  def apply[F[_], A](fa: F[A]): Eff[F, A] = Impure(fa, Arrs.Leaf((a: A) => Pure(a)))

  trait Handler[F[_], R[_], A, B] {
    def pure(a: A): Eff[R, B]
    def bind[T](fa: F[T])(f: T => Eff[R, B]): Eff[R, B]
    def unapply(u: F[A] | R[A]): Option[F[A]]
  }

  def handleRelay[F[_], R[_], A, B](eff: Eff[[A] => F[A] | R[A], A])(handler: Handler[F, R, A, B]): Eff[R, B] =
    eff match {
      case Eff.Pure(a) => handler.pure(a)
      case Eff.Lazy(v, k) => Eff.Lazy(v, Arrs.Leaf(a => handleRelay(k(a))(handler)))
      case Eff.Impure(handler(fa), k) => handler.bind(fa)(a => Eff.Lazy(a, Arrs.Leaf(a => handleRelay(k(a))(handler))))
      case Eff.Impure(r: R[a], k) => Eff.Impure(r, Arrs.Leaf(a => handleRelay(k(a))(handler)))
    }

  trait HandlerS[F[_], R[_], S, A, B] {
    def pure(s: S, a: A): Eff[R, B]
    def bind[T](s: S, fa: F[T])(f: (S, T) => Eff[R, B]): Eff[R, B]
    def unapply(u: F[A] | R[A]): Option[F[A]]
  }

  def handleRelayS[F[_], R[_], S, A, B](state: S, eff: Eff[[A] => F[A] | R[A], A])(handler: HandlerS[F, R, S, A, B]): Eff[R, B] =
    eff match {
      case Eff.Pure(a) => handler.pure(state, a)
      case Eff.Lazy(v, k) => Eff.Lazy(v, Arrs.Leaf(a => handleRelayS(state, k(a))(handler)))
      case Eff.Impure(handler(fa), k) => handler.bind(state, fa)((s, a) => Eff.Lazy(a, Arrs.Leaf(a => handleRelayS(s, k(a))(handler))))
      case Eff.Impure(r: R[a], k) => Eff.Impure(r, Arrs.Leaf(a => handleRelayS(state, k(a))(handler)))
    }

  @tailrec
  def run[A](eff: Eff[Nothing, A]): A =
    (eff: @unchecked) match {
      case Pure(a) => a
      case Lazy(v, k) => run(k(v))
    }

  case class Pure[R[_], A](value: A) extends Eff[R, A] {
    def map[B](f: A => B): Eff[R, B] = Pure(f(value))
    def flatMap[S[_], B](f: A => Eff[S, B]): Eff[[A] => R[A] | S[A], B] = f(value)
  }

  case class Impure[R[_], A, B](union: R[A], arrs: Arrs[R, A, B]) extends Eff[R, B] {
    def map[C](f: B => C): Eff[R, C] = Impure(union, arrs :+ (x => Pure(f(x))))
    def flatMap[S[_], C](f: B => Eff[S, C]): Eff[[A] => R[A] | S[A], C] = Impure[[A] => R[A] | S[A], A, C](union, arrs :+ f)
  }

  case class Lazy[R[_], A, B](value: A, arrs: Arrs[R, A, B]) extends Eff[R, B] {
    def map[C](f: B => C): Eff[R, C] = Lazy(value, arrs :+ (x => Pure(f(x))))
    def flatMap[S[_], C](f: B => Eff[S, C]): Eff[[A] => R[A] | S[A], C] = Lazy(value, arrs :+ f)
  }
}

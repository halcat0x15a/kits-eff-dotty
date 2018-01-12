package kits.eff

import scala.annotation.tailrec

trait Effect {
  type Value
}

enum class Eff[+R, A] {
  def map[B](f: A => B): Eff[R, B]

  def flatMap[S, B](f: A => Eff[S, B]): Eff[R | S, B]
}

object Eff {
  def apply[F <: Effect, A](fa: F { type Value = A }): Eff[F, A] = Impure(fa, Arrs.Leaf((a: A) => Pure(a)))

  @tailrec
  def run[A](eff: Eff[Nothing, A]): A =
    (eff: @unchecked) match {
      case Pure(a) => a
      case Lazy(v, k) => run(k(v))
    }

  case Pure[R, A](value: A) extends Eff[R, A] {
    def map[B](f: A => B): Eff[R, B] = Pure(f(value))
    def flatMap[S, B](f: A => Eff[S, B]): Eff[R | S, B] = f(value)
  }

  case Impure[R, A, B](union: R, arrs: Arrs[R, A, B]) extends Eff[R, B] {
    def map[C](f: B => C): Eff[R, C] = Impure(union, arrs :+ (x => Pure(f(x))))
    def flatMap[S, C](f: B => Eff[S, C]): Eff[R | S, C] = Impure(union, arrs :+ f)
  }

  case Lazy[R, A, B](value: A, arrs: Arrs[R, A, B]) extends Eff[R, B] {
    def map[C](f: B => C): Eff[R, C] = Lazy(value, arrs :+ (x => Pure(f(x))))
    def flatMap[S, C](f: B => Eff[S, C]): Eff[R | S, C] = Lazy(value, arrs :+ f)
  }
}

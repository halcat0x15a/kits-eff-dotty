package kits.eff

import scala.reflect.ClassTag

trait Effect {
  type Result
}

sealed trait Mu extends Effect

enum class Eff[R, A] {
  def map[B](f: A => B): Eff[R, B]

  def flatMap[B](f: A => Eff[R, B]): Eff[R, B]
}

object Eff {
  def apply[F <: Effect, A, R](fa: F { type Result = A })(implicit ev: F <:< R): Eff[R, A] = Impure(fa, Arrs.Leaf((a: A) => Pure(a)))

  def run[A](free: Eff[Mu, A]): A = {
    (free: @unchecked) match {
      case Pure(a) => a
    }
  }

  trait Handler[F <: Effect] {
    type Result[A]
    def pure[R, A](a: A): Eff[R, Result[A]]
    def bind[R, A, B](fa: F { type Result = A })(k: A => Eff[R, Result[B]]): Eff[R, Result[B]]
    def apply[R, A](free: Eff[F | R, A])(implicit F: ClassTag[F]): Eff[R, Result[A]] = {
      free match {
        case Pure(v) => pure(v)
        case Impure(u, k) => u match {
          case fa: F if F.runtimeClass.isInstance(fa) =>
            bind(fa)(a => apply(k(a)))
          case r: R =>
            Impure[R, Any, Result[A]](r, Arrs.Leaf((a: Any) => apply(k(a))))
        }
      }
    }
  }

  case Pure[R, A](value: A) extends Eff[R, A] {
    def map[B](f: A => B): Eff[R, B] = Pure(f(value))
    def flatMap[B](f: A => Eff[R, B]): Eff[R, B] = f(value)
  }

  case Impure[R, A, B](union: R, arrs: Arrs[R, A, B]) extends Eff[R, B] {
    def map[C](f: B => C): Eff[R, C] = Impure(union, arrs :+ (x => Pure(f(x))))
    def flatMap[C](f: B => Eff[R, C]): Eff[R, C] = Impure(union, arrs :+ f)
  }
}

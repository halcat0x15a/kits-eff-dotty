package kits.eff

import scala.util.control.TailCalls._

class Bench {
  val N = 1000000

  def benchEffCall(): (Int, Int) = Eff.run(State.run(0)(Bench.benchEff(1 to N)))

  def benchTransCall(): (Int, Int) = Bench.benchTrans[TailRec](1 to N).apply(0).result

  def benchTransSCall(): (Int, (Int, Int)) = Bench.benchTrans[[A] => StateT[TailRec, Int, A]](1 to N).apply(0).apply(0).result

  def benchTransSSCall(): (Int, (Int, (Int, Int))) = Bench.benchTrans[[A] => StateT[[A] => StateT[TailRec, Int, A], Int, A]](1 to N).apply(0).apply(0).apply(0).result
}

object Bench {
  def benchEff(ns: Seq[Int]): Eff[[A] => State[Int, A], Int] =
    ns.foldLeft(Eff.Pure(1): Eff[[A] => State[Int, A], Int]) { (acc, n) =>
      if n % 5 == 0 then for {
        acc <- acc
        s <- Reader.ask[Int]
        _ <- Writer.tell(s + 1)
      } yield acc max n
      else acc.map(_ max n)
    }

  def benchTrans[F[_]: Monad](ns: Seq[Int]): StateT[F, Int, Int] =
    ns.foldLeft(StateT.pure[F, Int, Int](1)) { (acc, n) =>
      if n % 5 == 0 then for {
        acc <- acc
        s <- StateT.get[F, Int]
        _ <- StateT.put(s + 1)
      } yield acc max n
      else acc.map(_ max n)
    }
}

trait Monad[F[_]] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: => F[A])(f: A => F[B]): F[B]
}

object Monad {
  implicit def tailRec: Monad[TailRec] = new Monad[TailRec] {
    def pure[A](a: A) = done(a)
    def flatMap[A, B](fa: => TailRec[A])(f: A => TailRec[B]) = tailcall(fa).flatMap(f)
  }

  implicit def stateT[F[_], S](implicit F: Monad[F]): Monad[[A] => StateT[F, S, A]] = new Monad[[A] => StateT[F, S, A]] {
    def pure[A](a: A) = StateT.pure(a)
    def flatMap[A, B](fa: => StateT[F, S, A])(f: A => StateT[F, S, B]) = fa.flatMap(f)
  }
}

trait StateT[F[_], S, A] { self =>
  def apply(s: S): F[(S, A)]
  def map[B](f: A => B)(implicit F: Monad[F]): StateT[F, S, B] = flatMap(a => StateT.pure(f(a)))
  def flatMap[B](f: A => StateT[F, S, B])(implicit F: Monad[F]): StateT[F, S, B] =
    new StateT[F, S, B] {
      def apply(s: S) = {
        F.flatMap(self(s)) {
          case (s, a) => f(a)(s)
        }
      }
    }
}

object StateT {
  def apply[F[_], S, A](fa: F[A])(implicit F: Monad[F]): StateT[F, S, A] =
    new StateT[F, S, A] {
      def apply(s: S) = F.flatMap(fa)(a => F.pure((s, a)))
    }

  def pure[F[_], S, A](a: A)(implicit F: Monad[F]): StateT[F, S, A] =
    new StateT[F, S, A] {
      def apply(s: S) = F.pure((s, a))
    }

  def get[F[_], S](implicit F: Monad[F]): StateT[F, S, S] =
    new StateT[F, S, S] {
      def apply(s: S) = F.pure((s, s))
    }

  def put[F[_], S](s: S)(implicit F: Monad[F]): StateT[F, S, Unit] =
    new StateT[F, S, Unit] {
      def apply(ss: S) = F.pure((s, ()))
    }
}

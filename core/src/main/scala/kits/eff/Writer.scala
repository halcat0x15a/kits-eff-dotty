package kits.eff

enum Writer[W, A] {
  case Put[W](value: W) extends Writer[W, Unit]
}

object Writer {
  def tell[W](value: W): Eff[[A] => Writer[W, A], Unit] = Eff(Put(value))

  def runVec[R[_], W, A](eff: Eff[[A] => Writer[W, A] | R[A], A]): Eff[R, (Vector[W], A)] =
    Eff.handleRelayS(Vector.empty[W], eff)(new Eff.HandlerS[[A] => Writer[W, A], R, Vector[W], A, (Vector[W], A)] {
      def pure(s: Vector[W], a: A) = Eff.Pure((s, a))
      def flatMap[T](s: Vector[W], fa: Writer[W, T])(k: (Vector[W], T) => Eff[R, (Vector[W], A)]) =
        fa match {
          case Writer.Put(w) => k(s :+ w, ())
        }
    })

  def runList[R[_], W, A](eff: Eff[[A] => Writer[W, A] | R[A], A]): Eff[R, (List[W], A)] =
    Eff.handleRelay(eff)(new Eff.Handler[[A] => Writer[W, A], R, A, (List[W], A)] {
      def pure(a: A) = Eff.Pure((Nil, a))
      def flatMap[T](fa: Writer[W, T])(k: T => Eff[R, (List[W], A)]) =
        fa match {
          case Writer.Put(w) => k(()).map { case (acc, a) => (w :: acc, a) }
        }
    })
}

package kits.eff

import scala.annotation.tailrec

enum class Arrs[+R, A, B] {
  def apply(a: A): Eff[R, B] = view match {
    case Arrs.View.One(f) => f(a)
    case Arrs.View.Cons(f, g) => f(a) match {
      case Eff.Pure(b) => g(b)
      case Eff.Impure(r, h) => Eff.Impure(r, h ++ g)
    }
  }

  def view[RR >: R]: Arrs.View[RR, A, B]

  def :+[S, C](f: B => Eff[S, C]): Arrs[R | S, A, C] = Arrs.Node(this, Arrs.Leaf(f))

  def ++[S, C](f: Arrs[S, B, C]): Arrs[R | S, A, C] = Arrs.Node(this, f)
}

object Arrs {
  case Leaf[R, A, B](arr: A => Eff[R, B]) extends Arrs[R, A, B] {
    def view[RR >: R] = View.One(arr)
  }

  case Node[R, A, B, C](left: Arrs[R, A, B], right: Arrs[R, B, C]) extends Arrs[R, A, C] {
    def view[RR >: R] = {
      @tailrec def go(left: Arrs[RR, A, Any], right: Arrs[RR, Any, C]): View[RR, A, C] =
        left match {
          case Leaf(arr) => View.Cons(arr, right)
          case Node(l, r) => go(l, r ++ right)
        }
      go(left.asInstanceOf[Arrs[RR, A, Any]], right.asInstanceOf[Arrs[RR, Any, C]])
    }
  }

  enum View[R, A, B] {
    case One[R, A, B](arr: A => Eff[R, B]) extends View[R, A, B]
    case Cons[R, A, B, C](arr: A => Eff[R, B], arrs: Arrs[R, B, C]) extends View[R, A, C]
  }
}

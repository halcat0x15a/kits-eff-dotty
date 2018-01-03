package kits.eff

import scala.annotation.tailrec

enum class Arrs[R, A, B] {
  def apply(a: A): Eff[R, B] = view match {
    case Arrs.View.One(f) => f(a)
    case Arrs.View.Cons(f, g) => f(a) match {
      case Eff.Pure(b) => g(b)
      case Eff.Impure(r, h) => Eff.Impure(r, h ++ g)
    }
  }

  def view: Arrs.View[R, A, B]

  def :+[C](f: B => Eff[R, C]): Arrs[R, A, C] = Arrs.Node(this, Arrs.Leaf(f))

  def ++[C](f: Arrs[R, B, C]): Arrs[R, A, C] = Arrs.Node(this, f)
}

object Arrs {
  case Leaf[R, A, B](arr: A => Eff[R, B]) extends Arrs[R, A, B] {
    def view = View.One(arr)
  }

  case Node[R, A, B, C](left: Arrs[R, A, B], right: Arrs[R, B, C]) extends Arrs[R, A, C] {
    def view = {
      @tailrec def go(left: Arrs[R, A, Any], right: Arrs[R, Any, C]): View[R, A, C] =
        left match {
          case Leaf(arr) => View.Cons(arr, right)
          case Node(l, r) => go(l, r ++ right)
        }
      go(left.asInstanceOf[Arrs[R, A, Any]], right.asInstanceOf[Arrs[R, Any, C]])
    }
  }

  enum View[R, A, B] {
    case One[R, A, B](arr: A => Eff[R, B]) extends View[R, A, B]
    case Cons[R, A, B, C](arr: A => Eff[R, B], arrs: Arrs[R, B, C]) extends View[R, A, C]
  }
}

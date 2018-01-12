package kits.eff

import scala.annotation.tailrec

enum class Arrs[+R, A, B] extends (A => Eff[R, B]) {
  def apply(a: A): Eff[R, B] = {
    @tailrec def go(arrs: Arrs[R, Any, B], a: Any): Eff[R, B] =
      arrs.view match {
        case Arrs.View.One(f) => f(a)
        case Arrs.View.Cons(f, g) => f(a) match {
          case Eff.Pure(b) => go(g, b)
          case Eff.Impure(r, h) => Eff.Impure(r, h ++ g)
          case Eff.Lazy(v, h) => Eff.Lazy(v, h ++ g)
        }
      }
    go(this.asInstanceOf[Arrs[R, Any, B]], a)
  }

  def view: Arrs.View[R, A, B]

  def :+[S, C](f: B => Eff[S, C]): Arrs[R | S, A, C] = Arrs.Node(this, Arrs.Leaf(f))

  def ++[S, C](f: Arrs[S, B, C]): Arrs[R | S, A, C] = Arrs.Node(this, f)
}

object Arrs {
  enum View[+R, A, B] {
    case One[R, A, B](arr: A => Eff[R, B]) extends View[R, A, B]
    case Cons[R, A, B, C](arr: A => Eff[R, B], arrs: Arrs[R, B, C]) extends View[R, A, C]
  }

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
}

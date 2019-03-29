package kits.eff

import scala.annotation.tailrec

enum Arrs[+R[_], A, B] extends (A => Eff[R, B]) {
  case Leaf[+R[_], A, B](arr: A => Eff[R, B]) extends Arrs[R, A, B]

  case Node[+R[_], A, B, C](left: Arrs[R, A, B], right: Arrs[R, B, C]) extends Arrs[R, A, C]

  def apply(a: A): Eff[R, B] = {
    /*@tailrec def go[A](arrs: Arrs[R, A, B], a: A): Eff[R, B] =
      arrs.view match {
        case Arrs.View.One(f) => f(a)
        case Arrs.View.Cons(f, g) => f(a) match {
          case Eff.Pure(b) => go(g, b)
          case Eff.Impure(r, h) => Eff.Impure(r, h ++ g)
        }
      }
     go(this, a)*/
    ???
  }

  def view: Arrs.View[R, A, B] =
    this match {
      case Leaf(f) => Arrs.View.One(f)
      case Node(l, r) => ???
    }

  def :+[S[_], C](f: B => Eff[S, C]): Arrs[[A] => R[A] | S[A], A, C] = Arrs.Node(this, Arrs.Leaf(f))

  def ++[S[_], C](f: Arrs[S, B, C]): Arrs[[A] => R[A] | S[A], A, C] = Arrs.Node(this, f)
}

object Arrs {
  enum View[+R[_], A, B] {
    case One[+R[_], A, B](arr: A => Eff[R, B]) extends View[R, A, B]
    case Cons[+R[_], A, B, C](arr: A => Eff[R, B], arrs: Arrs[R, B, C]) extends View[R, A, C]
  }
}

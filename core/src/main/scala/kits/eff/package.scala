package kits

package object eff {
  type State[S, A] = Reader[S, A] | Writer[S, A]
}

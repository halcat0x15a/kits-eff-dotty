package kits

package object eff {
  type State[S] = Reader[S] | Writer[S]
}

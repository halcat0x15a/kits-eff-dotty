package kits.eff
/*
object Main {
  val hoge: Eff[[A] => Reader[Int, A] | Writer[String, A], Int] = for {
    i <- Reader.ask[Int]
    _ <- Writer.tell(i.toString)
  } yield i + 1

  val fuga: Eff[[A] => State[Int, A], Int] = for {
    i <- Reader.ask[Int]
    _ <- Writer.tell(i + 1)
    j <- Reader.ask[Int]
  } yield j + 1

  def main(args: Array[String]): Unit = {
    val r1 = Eff.run(Writer.runList(Reader.run(0)(hoge)))
    val r2 = Eff.run(Reader.run(0)(Writer.runList(hoge)))
    assert(r1 == (List("0") -> 1))
    assert(r1 == r2)

    val r3 = Eff.run(State.run(0)(fuga))
    assert(r3 == (1 -> 2))
  }
}
 */

package kits.eff

object Main {
  val hoge: Eff[Reader[Int] | Writer[String], Int] = for {
    i <- Reader.ask[Int]
    _ <- Writer.tell(i.toString)
  } yield i + 1

  val fuga: Eff[State[Int], Int] = for {
    i <- Reader.ask[Int]
    _ <- Writer.tell(i + 1)
    j <- Reader.ask[Int]
  } yield j + 1

  def main(args: Array[String]): Unit = {
    val r1: Eff[Writer[String], Int] = Reader.run(0)(hoge)
    val r2: Eff[Reader[Int], (Vector[String], Int)] = Writer.run(hoge)
    val r3: Eff[Nothing, (Vector[String], Int)] = Reader.run(0)(r2)
    val r4: (Vector[String], Int) = Eff.run(r3)
    val r5: (Vector[String], Int) = Eff.run(Reader.run(0)(r2))
    val r6: Eff[Nothing, (Vector[String], Int)] = (Reader.run(0) compose Writer.run[String])(hoge)
    val r7: (Vector[String], Int) = Eff.run(Reader.run(0) compose Writer.run[String] apply hoge)
    val r8: (Int, Int) = Eff.run(State.run(0)(fuga))
  }
}

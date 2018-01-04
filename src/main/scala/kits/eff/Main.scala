package kits.eff

object Main {
  val hoge: Eff[Reader[Int] | Writer[String], Int] = for {
    i <- Reader.ask[Int]
    _ <- Writer.tell(i.toString)
  } yield i + 1

  def main(args: Array[String]): Unit = {
    val r1: Eff[Writer[String], Int] = Reader.run(0)(hoge)
    val r2: Eff[Reader[Int], (List[String], Int)] = Writer.run(hoge)
    val r3: Eff[Nothing, (List[String], Int)] = Reader.run(0)(r2)
    val r4: (List[String], Int) = Eff.run(r3)
    val r5: (List[String], Int) = Eff.run(Reader.run(0)(r2))
    val r6: Eff[Nothing, (List[String], Int)] = (Reader.run(0) compose Writer.run[String])(hoge)
    val r7: (List[String], Int) = Eff.run(Reader.run(0) compose Writer.run[String] apply hoge)
  }
}

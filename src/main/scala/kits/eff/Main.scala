package kits.eff

object Main {
  def hoge[R]: implicit Reader[Int] <:< R => implicit Writer[String] <:< R => Eff[R, Int] = for {
    i <- Reader.ask
    _ <- Writer.tell(i.toString)
  } yield i + 1

  def main(args: Array[String]): Unit = {
    println(Eff.run(Reader.run(0)(Writer.run(hoge[Reader[Int] | Writer[String] | Mu]))))
    println(Eff.run(Writer.run(Reader.run(0)(hoge[Reader[Int] | Writer[String] | Mu]))))
  }
}

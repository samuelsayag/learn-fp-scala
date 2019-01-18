package fp.in.scala

object Part_2_7 {

  // pure functional parallelism
  // ideally we would want this kind of signature for the function

  //  val outputlist = parMap(inputList)(f)

  // sum of integer over a list
  def sumFold(s: Seq[Long]): Long = s.foldLeft(0L)(_ + _)


  def sumDC(s: IndexedSeq[Long]): Long = {
    if (s.length <= 1) s.headOption.getOrElse(0)
    else {
      val (l, r) = s.splitAt(s.length / 2)
      sumDC(l) + sumDC(r)
    }
  }

  object FunParTest {
    def testfold(s: Int) = sumFold(Seq.range(0, s - 1))

    def testDivConq(s: Int) = sumDC(IndexedSeq.range(0, s - 1))
  }

  def main(args: Array[String]): Unit = {
    import FunParTest._
    println(testfold(20000000))  // execute in about 7 sec
//    println(testDivConq(20000000)) // execute in about 6 sec why ???
  }
}

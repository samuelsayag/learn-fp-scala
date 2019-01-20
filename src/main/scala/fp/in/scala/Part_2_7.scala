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

  final case class Par[E](e: E)

  object Par {

    def fork[A](a: => Par[A]):Par[A] = ???

    def unit[A](a:  A): Par[A] = ???

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](pa: Par[A]): A = ???

    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = ???

  }

  def sumP(s: IndexedSeq[Long]): Par[Long] = {
    if (s.length <= 1)
      Par.unit(s.headOption getOrElse 0)
    else {
      val (l, r) = s.splitAt(s.length / 2)
     Par.map2(Par.fork(sumP(l)), Par.fork(sumP(r)))(_ + _)
    }
  }

  object FunParTest {
    def testfold(s: Int): Long = sumFold(Seq.range(0, s - 1))

    def testDivConq(s: Int): Long = sumDC(IndexedSeq.range(0, s - 1))
  }

  def main(args: Array[String]): Unit = {
    import FunParTest._
    //    println(testfold(20000000)) //[error] java.lang.OutOfMemoryError: GC overhead limit exceeded
    println(testDivConq(20000000)) // this execute in about 60 sec
  }
}

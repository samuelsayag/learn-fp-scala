package fp.in.scala

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

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

  //  final case class Par[E](e: E)

  type Par[A] = ExecutorService => Future[A]

  object Par {

    def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => {
      es.submit(new Callable[A] {
        override def call(): A = a(es).get()
      })
    }

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](pa: Par[A])(s: ExecutorService): Future[A] = pa(s)

    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
      es => UnitFuture(f(pa(es).get(), pb(es).get))

    // def of map in term of map2
    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))

    def parMap[A,B](la: List[A])(f: A => B): Par[List[B]] =
      Par.sequence(la.map(asyncF(f)))

    def sequence[A](lp: List[Par[A]]): Par[List[A]] =
      lp.foldRight(unit(List.empty[A]))((p, pl) => map2(p, pl)(_ :: _))

    // take a list of int and sort it
    def sortedMap(li: Par[List[Int]]): Par[List[Int]] = map(li)(_.sorted)

    def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

    private case class UnitFuture[A](get: A) extends Future[A] {
      override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

      override def get(timeout: Long, unit: TimeUnit): A = get

      override def isCancelled: Boolean = false

      override def isDone: Boolean = true
    }

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

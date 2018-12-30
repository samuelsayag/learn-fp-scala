package fp.in.scala

import scala.annotation.tailrec

object Part_1_2 {

  def main(args: Array[String]): Unit = {
    //    testFib()
    //    testSorded()
    testCurry()
  }

  def testFib(): Unit = {
    println(fib(0))
    println(fib(1))
    println(fib(2))
    println(fib(3))
    println(fib(4))
    println(fib(5))
  }

  // 2.1
  def fib(n: Int): Int = {
    if (n < 0) throw new Exception("n is not positive")

    @tailrec
    def loop(f: Int, s: Int, cnt: Int): Int =
      if (cnt > n) s else loop(s, f + s, cnt + 1)

    if (n == 0) 0
    else if (n == 1) 1
    else loop(0, 1, 2)
  }

  def testSorded(): Unit = {
    val o = (i: Int, j: Int) => i <= j
    val is = (a: Array[Int]) => println(isSorded(a, o))

    is(Array.emptyIntArray)
    is(Array(0))
    is(Array(0, 1))
    is(Array(1, 1))
    is(Array(1, 0))
    is(Array(1, 1, 2, 6, 9))
    is(Array(1, 1, 2, 9, 4))
  }

  // 2.2
  def isSorded[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(i: Int, j: Int): Boolean = {
      if (j > as.length - 1) true
      else if (!ordered(as(i), as(j))) false
      else loop(j, j + 1)
    }

    if (as.isEmpty || as.length == 1) true
    else loop(0, 1)
  }

  def testCurry(): Unit = {
    println(curry(isSorded))
  }

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => B => C = (a: A) => (b: B) => f(a, b)

  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  // 2.5
  def compose[A, B, C](g: B=> C, f: A => B) : A => C = (a: A) => g(f(a))

}

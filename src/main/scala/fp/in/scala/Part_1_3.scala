package fp.in.scala

object Part_1_3 {


  def main(args: Array[String]): Unit = {
    //    testTail(List.tail)
    //    testTail(List.tail2)
    testSetHead(List.setHead)
    testSetHead(List.setHead2)
  }

  def testTail(t: List[Int] => List[Int]): Unit = {
    val l1: List[Int] = Nil
    val l2: List[Int] = Cons(1, Cons(2, Nil))

    val f = (xs: List[Int]) => println(t(xs))

    f(l1)
    f(l2)
  }

  def testSetHead(t: (Int, List[Int]) => List[Int]): Unit = {
    val i: Int = 555
    val l1: List[Int] = Nil
    val l2: List[Int] = Cons(1, Cons(2, Nil))

    val f = (xs: List[Int]) => println(t(i, xs))

    f(l1)
    f(l2)
  }

  sealed trait List[+A] {
    def isEmpty: Boolean

    def head: A

    def tail: List[A]
  }

  case object Nil extends List[Nothing] {
    override def isEmpty: Boolean = true

    override def head: Nothing = throw new Exception("head() call on Nil")

    override def tail: List[Nothing] = Nil
  }

  case class Cons[+A](head: A, tail: List[A]) extends List[A] {
    override def isEmpty: Boolean = false
  }

  object List {

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def sum(xs: List[Int]): Int =
      xs match {
        case Nil => 0
        case Cons(h, t) => h + sum(t)
      }

    def product(xs: List[Double]): Double =
      xs match {
        case Nil => 1
        case Cons(0.0, _) => 0
        case Cons(h, t) => h * product(t)
      }

    // 3.2
    def tail[A](xs: List[A]): List[A] =
      xs match {
        case Nil => Nil
        case Cons(_, t) => t
      }

    // 3.2
    def tail2[A](xs: List[A]): List[A] =
      if (xs.isEmpty) xs
      else xs.tail

    def setHead[A](a: A, xs: List[A]): List[A] =
      xs match {
        case Nil => Nil
        case Cons(_, t) => Cons(a, t)
      }

    def setHead2[A](a: A, xs: List[A]): List[A] =
      if (xs.isEmpty) xs
      else Cons(a, xs.tail)
  }

}

package fp.in.scala

import scala.annotation.tailrec

object Part_1_3 {


  def main(args: Array[String]): Unit = {
    //    testTail(List.tail)
    //    testTail(List.tail2)
    //    testSetHead(List.setHead)
    //    testSetHead(List.setHead2)
    //    testDrop()
    //    testInit()
    //    testCopy()
    //    testLength()
    //    testFoldRight()
    //    testAppend()
    //    testConcatList()
    //    testAdd1()
    testDouble2Str()
  }

  def testDouble2Str(): Unit = {
    val l1 = Nil
    val l2: List[Double] = Cons(1, Nil)
    val l3: List[Double] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

    import List._
    val f = (xs: List[Double]) => println(double2Str(xs))

    f(l1)
    f(l2)
    f(l3)
  }

  def testAdd1(): Unit = {
    val l1 = Nil
    val l2 = Cons(1, Nil)
    val l3 = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

    import List._
    val f = (xs: List[Int]) => println(add1(xs))

    f(l1)
    f(l2)
    f(l3)
  }

  def testConcatList(): Unit = {
    val l1 = Nil
    val l2 = Cons(Cons(1, Nil), Nil)
    val l3 = Cons(
      Cons(1, Cons(2, Nil)),
      Cons(
        Cons(3, Cons(4, Nil)),
        Cons(
          Cons(5, Cons(6, Nil)),
          Nil
        )
      )
    )

    import List._
    val f = (xs: List[List[Int]]) => println(concatList(xs))

    f(l1)
    f(l2)
    f(l3)
  }

  def testAppend(): Unit = {
    val (l11, l12) = (Nil, Cons(1, Nil))
    val (l21, l22) = (Cons(1, Nil), Cons(2, Nil))
    val (l31, l32) = (Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Cons(5, Cons(6, Cons(7, Cons(8, Nil)))))

    import List._
    val f = (l1: List[Int], l2: List[Int]) => println(append(l1, l2))

    f(l11, l12)
    f(l21, l22)
    f(l31, l32)
  }

  def testFoldRight(): Unit = {
    val l1 = Nil
    val l2 = Cons(1, Nil)
    val l3 = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

    import List._
    val f = (xs: List[Int]) => println(foldRight2(xs, 0)(_ - _) == foldRight(xs, 0)(_ - _))

    f(l1)
    f(l2)
    f(l3)
  }


  def testCopy(): Unit = {
    val l1 = Nil
    val l2 = Cons(1, Nil)
    val l3 = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

    import List._
    val f = (xs: List[Int]) => println(copy(xs))

    f(l1)
    f(l2)
    f(l3)
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

  def testDrop(): Unit = {
    val l1 = Nil
    val l2 = Cons(1, Cons(2, Nil))
    val l3 = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

    import List._
    val f = (xs: List[Int], n: Int) => println(drop(xs, n))

    f(l1, 5)
    f(l2, 5)
    f(l3, 2)
  }

  def testDropWhile(): Unit = {
    val l1 = Nil
    val l2 = Cons(1, Cons(2, Nil))
    val l3 = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

    import List._
    val f = (xs: List[Int], f: Int => Boolean) => println(dropWhile(xs, f))
    val p = (i: Int) => i < 3

    f(l1, p)
    f(l2, p)
    f(l3, p)
  }

  def testInit(): Unit = {
    val l1 = Nil
    val l2 = Cons(1, Nil)
    val l3 = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

    import List._
    val f = (xs: List[Int]) => println(init(xs))

    f(l1)
    f(l2)
    f(l3)
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

    def sum2(xs: List[Int]): Int =
      xs match {
        case Nil => 0
        case Cons(h, t) => h + sum(t)
      }

    def product2(xs: List[Double]): Double =
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

    // 3.3
    def setHead[A](a: A, xs: List[A]): List[A] =
      xs match {
        case Nil => Nil
        case Cons(_, t) => Cons(a, t)
      }

    // 3.3
    def setHead2[A](a: A, xs: List[A]): List[A] =
      if (xs.isEmpty) xs
      else Cons(a, xs.tail)

    // 3.4
    def drop[A](l: List[A], n: Int): List[A] =
      (l, n) match {
        case (Nil, _) => Nil
        case (xs, 0) => xs
        case (Cons(_, t), _) => drop(t, n - 1)
      }

    // 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
      l match {
        case Nil => Nil
        case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
      }

    // 3.6
    def init[A](l: List[A]): List[A] =
      l match {
        case Nil | Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
      }

    def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      l match {
        case Nil => z
        case Cons(h, t) => f(h, foldRight(t, z)(f))
      }

    // 3.13
    def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)(f)

    // 3.12
    def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])(Cons(_, _))

    // 3.10
    def foldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
      @tailrec
      def loop(xs: List[A], zz: B): B =
        xs match {
          case Nil => zz
          case Cons(h, t) => loop(t, f(h, zz))
        }

      loop(l, z)
    }

    // 3.8
    def copy[A](l: List[A]): List[A] = foldRight(l, Nil: List[A])(Cons(_, _))

    // 3.9
    def length2[A](l: List[A]): Int = foldRight(l, 0)((_, c) => c + 1)

    // 3.11
    def length[A](l: List[A]): Int = foldLeft(l, 0)((_, c) => c + 1)

    def sum(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)

    def product(xs: List[Double]): Double = foldLeft(xs, 1.0)(_ * _)

    // 3.14
    def append[A](l1: List[A], l2: List[A]): List[A] =
      foldLeft(reverse(l1), l2)((e, l) => Cons(e, l))

    // 3.15
    def concatList[A](l: List[List[A]]): List[A] =
      foldLeft(reverse(l), Nil: List[A])((a, b) => append(a, b))

    // 3.16
    def add1(l: List[Int]): List[Int] = foldLeft(reverse(l), Nil: List[Int])((e, z) => Cons(e + 1, z))

    // 3.17
    def double2Str(l: List[Double]): List[String] =
      foldLeft(reverse(l), Nil: List[String])((e, z) => Cons(e.toString, z))

    // 3.18
    def map[A, B](l: List[A])(f: A => B): List[B] =
      foldLeft(reverse(l), Nil: List[B])((e, z) => Cons(f(e), z))

    // 3.19
    def filter[A](l: List[A])(p: A => Boolean): List[A] =
      foldLeft(reverse(l), Nil: List[A])((e, z) => if (p(e)) Cons(e, z) else z)
  }

}

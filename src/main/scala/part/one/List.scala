package part.one

import scala.annotation.tailrec

sealed trait List[+A] {
  def tail: List[A]

  def setHead[B >: A](b: B): List[B]

  def drop(n: Int): List[A]

  def dropWhile(f: A => Boolean): List[A]

  def init: List[A]

  def length: Int

  def foldLeft[B](z: B)(f: (A, B) => B): B

  def reverse: List[A]

  def foldRight[B](z: B)(f: (A, B) => B): B

  def append[B >: A](b: B): List[B]
}

case object Nil extends List[Nothing] {
  override def tail: List[Nothing] = this

  override def setHead[B >: Nothing](b: B): List[B] = List(b)

  override def drop(n: Int): List[Nothing] = this

  override def dropWhile(f: Nothing => Boolean): List[Nothing] = this

  override def init: List[Nothing] = this

  override def length: Int = 0

  override def foldLeft[B](z: B)(f: (Nothing, B) => B): B = z

  override def reverse: List[Nothing] = this

  override def foldRight[B](z: B)(f: (Nothing, B) => B): B = z

  override def append[B >: Nothing](b: B): List[B] = setHead(b)
}

case class Cons[+A](head: A, tail: List[A]) extends List[A] {

  override def setHead[B >: A](b: B): List[B] = Cons(b, tail)

  override def drop(n: Int): List[A] =
    if (n <= 0) this
    else tail.drop(n - 1)

  override def dropWhile(f: A => Boolean): List[A] =
    if (f(head)) tail.dropWhile(f)
    else this

  override def init: List[A] =
    tail match {
      case Cons(_, Nil) => Cons(head, Nil)
      case _ => Cons(head, tail.init)
    }

  // NOT tail recursive solution of foldRight
  //  override def foldRight[B](z: B)(f: (A, B) => B): B =
  //    f(head, tail.foldRight(z)(f))

  override def length: Int = foldRight(0)((_, y) => 1 + y)

  override def foldLeft[B](z: B)(f: (A, B) => B): B = {
    @tailrec
    def foldLoop(acc: B, l: List[A]): B = {
      l match {
        case Nil => acc
        case Cons(h, t) => foldLoop(f(h, acc), t)
      }
    }

    foldLoop(z, this)
  }

  override def reverse: List[A] =
    foldLeft(Nil: List[A])(Cons(_, _))

  override def foldRight[B](z: B)(f: (A, B) => B): B = {
    val rl = this.reverse
    rl.foldLeft(z)(f)
  }

  override def append[B >: A](b: B): List[B] = ???

}

object List {

  def sum(ints: List[Int]): Int = ints.foldRight(0)(_ + _)

  def sum1(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(ds: List[Double]): Double = ds.foldRight(1.0)(_ * _)

  def product1(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}

package part.one

sealed trait List[+A] {
  def tail: List[A]

  def setHead[B >: A](b: B): List[B]

  def drop(n: Int): List[A]
}

case object Nil extends List[Nothing] {
  override def tail: List[Nothing] = this

  override def setHead[B >: Nothing](b: B): List[B] = List(b)

  override def drop(n: Int): List[Nothing] = this
}

case class Cons[+A](head: A, tail: List[A]) extends List[A] {

  override def setHead[B >: A](b: B): List[B] = Cons(b, tail)

  override def drop(n: Int): List[A] =
    if (n <= 0) this
    else tail.drop(n-1)
}

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}

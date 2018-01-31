package part.one

object Exercise3_12 {

  import part.one.List._
  import utils.Helper._

  def main(args: Array[String]): Unit = {

    val f = () => println(s"The answer is $x. (ex. 3.1)")
    wrapPrint(f)

    wrapPrint(demoTail)
    wrapPrint(demoSetHead)
    wrapPrint(demoDrop)
  }

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def demoTail() = {
    val l = List(1, 2, 3)
    println(s"Tail of $Nil  is ${Nil.tail}")
    println(s"Tail of $l  is ${l.tail} (ex. 3.2)")
  }

  def demoSetHead() = {
    val l = List(1, 2, 3)
    println(s"setHead(5) of $Nil  is ${Nil.setHead(5)}")
    println(s"setHead(5) of $l  is ${l.setHead(5)} (ex. 3.3)")
  }

  def demoDrop() = {
    val l = List(1, 2, 3, 4, 5, 6)
    println(s"drop(5) of $Nil  is ${Nil.drop(5)}")
    println(s"drop(5) of $l  is ${l.drop(5)} ")
    println(s"drop(10) of $l  is ${l.drop(10)} (ex. 3.3)")
  }

}

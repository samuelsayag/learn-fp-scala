package part.one

object Exercise3_12345 {

  import part.one.List._
  import utils.Helper._

  def main(args: Array[String]): Unit = {

    val f = () => println(s"The answer is $z. (ex. 3.1)")
    wrapPrint(f)

    wrapPrint(() => demoTail())
    wrapPrint(() => demoSetHead())
    wrapPrint(() => demoDrop())
    wrapPrint(() => demoDropWhile())
  }

  val z: Int = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def demoTail(): Unit = {
    val l = List(1, 2, 3)
    println(s"Tail of $Nil  is ${Nil.tail}")
    println(s"Tail of $l  is ${l.tail} (ex. 3.2)")
  }

  def demoSetHead(): Unit = {
    val l = List(1, 2, 3)
    println(s"setHead(5) of $Nil  is ${Nil.setHead(5)}")
    println(s"setHead(5) of $l  is ${l.setHead(5)} (ex. 3.3)")
  }

  def demoDrop(): Unit = {
    val l = List(1, 2, 3, 4, 5, 6)
    println(s"drop(5) of $Nil  is ${Nil.drop(5)}")
    println(s"drop(5) of $l  is ${l.drop(5)} ")
    println(s"drop(10) of $l  is ${l.drop(10)} (ex. 3.3)")
  }


  def demoDropWhile(): Unit = {
    val l = List(1, 2, 3, 4, 5, 6)
    println(s"dropWhile( _ < 10) of $Nil  is ${List[Int]().dropWhile(_ < 10)}")
    println(s"dropWhile( _ < 5) of is ${l.dropWhile(_ < 5)} ")
    println(s"dropWhile( _ < 10) of $l  is ${l.dropWhile(_ < 10)} (ex. 3.3)")
  }


}

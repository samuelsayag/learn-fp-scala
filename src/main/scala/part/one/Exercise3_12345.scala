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
    wrapPrint(() => demoInit())
    wrapPrint(() => demoSum())
    wrapPrint(() => demoProduct())
    wrapPrint(() => demoLength())
  }

  val z: Int = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }


  def demoLength(): Unit = {
    val l = List(1,2,3,4)
    println(s"sum of $Nil  is ${Nil.length}")
    println(s"sum of $l  is ${l.length}")
  }


  def demoProduct(): Unit = {
    val l = List(1.0, 2.0, 3.0)
    println(s"sum of $Nil  is ${List.product(l)}")
    println(s"sum of $l  is ${List.product(l)}")
  }

  def demoSum(): Unit = {
    val l = List(1, 2, 3)
    println(s"sum of $Nil  is ${List.sum(l)}")
    println(s"sum of $l  is ${List.sum(l)}")
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
    println(s"drop(10) of $l  is ${l.drop(10)} (ex. 3.4)")
  }

  def demoDropWhile(): Unit = {
    val l = List(1, 2, 3, 4, 5, 6)
    println(s"dropWhile( _ < 10) of $Nil  is ${List[Int]().dropWhile(_ < 10)}")
    println(s"dropWhile( _ < 5) of is ${l.dropWhile(_ < 5)} ")
    println(s"dropWhile( _ < 10) of $l  is ${l.dropWhile(_ < 10)} (ex. 3.5)")
  }

  def demoInit(): Unit = {
    val l = List(1, 2, 3, 4, 5, 6)
    println(s"init of $Nil  is ${Nil.init}")
    println(s"init of $l is ${l.init} ")
    println(s"init of ${List(1)}  is ${List(1).init} (ex. 3.5)")
  }

}

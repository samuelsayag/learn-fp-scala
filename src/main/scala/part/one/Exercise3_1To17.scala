package part.one

object Exercise3_1To17 {

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
    wrapPrint(() => demoFoldLeft())
    wrapPrint(() => demoReverse())
    wrapPrint(() => demoRight())
    wrapPrint(() => demoAppend())
    wrapPrint(() => demoConcatenate())
    wrapPrint(() => demoAddOne())
    wrapPrint(() => demoAddOne2())
    wrapPrint(() => demoMkString())
  }

  val z: Int = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }


  def demoLength(): Unit = {
    val l = List(1, 2, 3, 4)
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

  def demoFoldLeft(): Unit = {
    val l = List(1, 2, 3, 4, 5, 6)
    val f = (x: Int, y: Int) => {
      println(s"Value of acc: $y")
      x + y
    }
    println(s"sum (by foldLeft) of $l  is ${l.foldLeft(0)(f)}")
  }

  def demoRight(): Unit = {
    val l = List(1, 2, 3, 4, 5, 6)
    val f = (x: Int, y: Int) => {
      println(s"Value of acc: $y")
      x + y
    }
    println(s"sum (by foldRight) of $l  is ${l.foldRight(0)(f)}")
  }

  def demoReverse(): Unit = {
    val l = List(1, 2, 3, 4, 5, 6)
    println(s"reverse of $l  is ${l.reverse}")
  }

  def demoAppend(): Unit = {
    val l1 = List(1, 2, 3, 4, 5, 6)
    val l2 = List(7, 8, 9)
    println(s"append of $l2 to $l1  is ${l1.append(l2)}")
  }

  def demoConcatenate(): Unit = {
    val xss = List(List(1, 2), List(3, 4), List(5, 6, 7), List(8, 9))
    println(s"concatenate of $xss is ${List.concat(xss)}")
  }

  def demoAddOne(): Unit = {
    val xs = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    println(s"addOne of $xs is ${List.addOne(xs)}")
  }

  def demoAddOne2(): Unit = {
    val xs = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    println(s"addOne (with map) of $xs is ${xs.map(_ + 1)}")
  }

  def demoMkString(): Unit = {
    val xs = List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
    println(s"""mkString of $xs is "${List.mkString(xs)}" """)
  }
}

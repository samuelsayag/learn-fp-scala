package part.one

object Exercice2_1 {

  def main(args: Array[String]): Unit = {

    val f = (i: Int) => println(fib(i))
    //        val f: Int => Unit = i => println(fib(i))
    (0 to 10) foreach f
  }

  def fib(n: Int): Int = {

    def fibLoop(n: Int, fb1: Int, fb2: Int): Int = {
      if (n <= 0)
        fb1
      else fibLoop(n - 1, fb2, fb1 + fb2)
    }

    fibLoop(n, 0, 1)
  }

}

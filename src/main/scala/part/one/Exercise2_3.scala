package part.one

object Exercise2_3 {


  def main(args: Array[String]): Unit = {
    val f = (a: Int, b: Int) => a + b

    import utils.Helper._
    println(getType(curry(f)))
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

}

package part.one

object Exercise2_345 {


  def main(args: Array[String]): Unit = {
    val f = (a: Int, b: Int) => a + b

    val cf = curry(f)

    import utils.Helper._
    println(s"Curry of type: [ ${getType(f)} ] gives [ ${getType(cf)} ]")

    val uc = uncurry(cf)
    println(s"Uncurry of type: [ ${getType(cf)} ] gives [ ${getType(uc)} ]")

    val f1 = (i: Int) => i + 1
    val g = (i: Int) => i.toString
    val cp = compose(g, f1)
    println(s"Compose of type: [ ${getType(f1)} ] with [ ${getType(g)} ] gives [ ${getType(cp)} ]")
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

}

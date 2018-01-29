package part.one

object Exercise2_2 {

  def main(args: Array[String]): Unit = {

    def res(end: Int) = isSorted(
      ((1 to 10) :+ end).toArray,
      (a: Int, b: Int) => a < b
    )

    println(res(0))
    println(res(11))
  }


  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    def isLoop(acc: Boolean, idx: Int): Boolean =
      if (idx > as.length - 1) acc
      else isLoop(
        acc && ordered(as(idx - 1), as(idx)),
        idx + 1)


    if (as.length < 2) true else isLoop(acc = true, 1)
  }

}

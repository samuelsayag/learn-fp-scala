package part.one.four

object OptionExercize {

  import scala.{Option => SOption}

  def mean(xs: Seq[Double]): Option[Double] =
    Some(xs).filter(_.nonEmpty).map(i => i.sum / i.length)


  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap { m =>
      mean(xs.map(i => Math.pow(i - m, 2)))
    }


  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(p1 => b.map(p2 => f(p1, p2)))

  // old version of sequence
  /*  def sequence[A](a: List[Option[A]]): Option[List[A]] =
      a.foldRight(Some(List.empty[A]): Option[List[A]])(
        (oa, acc) => oa.flatMap(i => acc.map(i +: _)))*/


  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity[Option[A]])


  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(List.empty[B]): Option[List[B]])(
      (a, acc) =>
        f(a).flatMap(i => acc.map(i +: _)))

}

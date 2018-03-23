package part.one.four

object OptionExercize {

  import scala.{Option => SOption}

  def mean(xs: Seq[Double]): Option[Double] =
    Some(xs).filter(_.nonEmpty).map(i => i.sum / i.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap { m =>
      mean(xs.map(i => Math.pow(i-m,2)))
    }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a.flatMap(p1 => b.map(p2 => f(p1,p2)))

}

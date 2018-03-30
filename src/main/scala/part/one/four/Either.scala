package part.one.four

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case l@Left(_) => l
    case Right(r) => Right(f(r))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case l@Left(_) => l
    case Right(r) => f(r)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case r@Right(_) => r
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      tp <- b
    } yield f(a, tp)

}

object Either {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldLeft(Right(List.empty[B]): Either[E, List[B]]) { (lb, a) =>
      lb match {
        case l@Left(_) => l
        case Right(l) => f(a).map(l :+ _)
      }
    }
  }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

}

case class Left[E](e: E) extends Either[E, Nothing]

case class Right[A](a: A) extends Either[Nothing, A]




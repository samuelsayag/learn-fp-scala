package fp.in.scala

object Part_4_14 {

  // --------------------------------------------------------------------------------------------
  // The first version of the IO trait with just the description of the effect capability
  // --------------------------------------------------------------------------------------------
  //  trait IO {
  //    def run(): Unit
  //  }

  // --------------------------------------------------------------------------------------------
  // The second version of the IO trait with the monoid capabilities
  // --------------------------------------------------------------------------------------------
  //  trait IO {
  //    self =>
  //
  //    def run(): Unit
  //
  //    def ++(io: IO): IO = () => {
  //      self.run()
  //      io.run()
  //    }
  //  }

  // --------------------------------------------------------------------------------------------
  // The third version of the IO trait with the monad capabilities
  // --------------------------------------------------------------------------------------------
  sealed trait IO[E] {
    self =>

    import IO._

    def run(): E

    def map[B](f: E => B): IO[B] = unit(f(self.run()))

    def flatMap[B](f: E => IO[B]): IO[B] = unit(f(self.run()).run())

    def ++(io: IO[E]): IO[E] = unit[E] {
      self.run()
      io.run()
    }

    // implement ++ with the flatMap (monad capability
    //    def ++(io: IO[E]): IO[E] = self flatMap (_ => io)
  }

  object IO {
    def empty: IO[Unit] = unit(())

    def unit[A](a: A): IO[A] = IO(a)

    def apply[A](a: A): IO[A] = new IO[A] {
      override def run(): A = a
    }

    def map[A, B](a: IO[A])(f: A => B): IO[B] = a map f

    def flatMap[A, B](a: IO[A])(f: A => IO[B]): IO[B] = a flatMap f
  }


  // new version when the monad code is done
  def PrintLine(msg: String): IO[Unit] = IO(println(msg))

  // version with no IO monad
  //  def PrintLine(msg: String): IO = new IO {
  //    override def run(): Unit = println(msg)
  //  }


  object GameModel {

    case class Player(name: String, score: Int)

    def winner(p1: Player, p2: Player): Option[Player] =
      if (p1.score > p2.score) Some(p1)
      else if (p2.score > p1.score) Some(p2)
      else None

    def winnerMsg(p: Option[Player]): String =
      p match {
        case Some(Player(name, score)) => s"winner is $name with a score of $score"
        case _ => "It is a draw"
      }

    def contest(p1: Player, p2: Player): IO[Unit] = PrintLine(winnerMsg(winner(p1, p2)))
  }

  object TestIO {

    import GameModel._

    def contest1(): Unit = contest(Player("alice", 555), Player("Bob", 666)).run()

    def contest2(): Unit =
      (PrintLine("here is the result of the contest") ++
        PrintLine("--------------------------------------") ++
        contest(Player("alice", 555), Player("Bob", 666))).run()
  }


  def main(args: Array[String]): Unit = {
    import TestIO._
    //    contest1()
    contest2()
  }
}

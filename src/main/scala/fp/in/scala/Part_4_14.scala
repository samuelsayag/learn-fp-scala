package fp.in.scala

object Part_4_14 {

  //  trait IO {
  //    def run(): Unit
  //  }

  // with the monoid capabilities
  trait IO {
    self =>

    def run(): Unit

    def ++(io: IO): IO = () => {
      self.run()
      io.run()
    }
  }

  object IO {
    def empty: IO = () => {
      ()
    }
  }

  def PrintLine(msg: String): IO = new IO {
    override def run(): Unit = println(msg)
  }


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

    def contest(p1: Player, p2: Player): IO = PrintLine(winnerMsg(winner(p1, p2)))
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
    //    contest1
    contest2
  }
}

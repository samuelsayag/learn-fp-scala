package part.one.four

import org.scalatest.FlatSpec

class TestOption extends FlatSpec {

  "Option[Int].map(_.toDouble)" should "return Option[Double]" in {
    assert(part.one.four.Some(5).
      map(_.toDouble).
      isInstanceOf[Option[Double]])
  }

  "None.map(_.toDouble)" should "return None" in {
    assert(part.one.four.None.asInstanceOf[Option[Int]].
      map(_.toDouble) == None)
  }

  "Option[Int].flatMap(Some(_.toDouble))" should "return Option[Double]" in {
    assert(part.one.four.Some(5).
      flatMap(i => Some(i.toDouble)).
      isInstanceOf[Option[Double]])
  }

  "None.flatMap(i => Some(i.toDouble))" should "return None" in {
    assert(part.one.four.None.asInstanceOf[Option[Int]].
      flatMap(i => Some(i.toDouble)) == None)
  }


  "Some(x).getOrElse(y)" should "return x" in {
    assert(part.one.four.Some(5).getOrElse(6) == 5)
  }

  "None.getOrElse(x)" should "return x" in {
    assert(part.one.four.None.getOrElse(5) == 5)
  }

}

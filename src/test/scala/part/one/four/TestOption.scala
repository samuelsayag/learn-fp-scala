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


  "Some(x).orElse(Some(y))" should "return Some(y)" in {
    assert(part.one.four.Some(5).orElse(Some(6)) == Some(5))
  }


  "None.orElse(Some(x))" should "return Some(x)" in {
    assert(part.one.four.None.orElse(Some(5)) == Some(5))
  }


  "Some(5).filter(_ > 4)" should "return Some(5)" in {
    assert(part.one.four.Some(5).filter(_ > 4) == Some(5))
  }


  "Some(5).filter(_ < 4)" should "return None" in {
    assert(part.one.four.Some(5).filter(_ < 4) == None)
  }


  "None.filter(_ < 4)" should "return None" in {
    assert(part.one.four.None.asInstanceOf[Option[Int]].filter(_ < 4) == None)
  }

  "List(1,2,3,4,5,6,7,8,9)" should "have a not None variance" in {
    assert(OptionExercize.variance(List(1, 2, 3, 4, 5, 6, 7, 8, 9)) != None)
  }

  "List.empty[Double]" should "have a None variance" in {
    assert(OptionExercize.variance(List.empty[Double]) == None)
  }
}

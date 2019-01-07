package fp.in.scala

object Part_1_6 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  val rint: Rand[Int] = _.nextInt

  def map[A, B](rna: Rand[A])(op: A => B): Rand[B] =
    rng => {
      val (a, nrng) = rna(rng)
      (op(a), nrng)
    }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt(): (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

    // 6.1
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (ni, ns) = rng.nextInt
      (if (ni < 0) -(ni + 1) else ni, ns)
    }

    // 6.2
    def double(rng: RNG): (Double, RNG) = {
      val (i, s) = nonNegativeInt(rng)
      (i.toDouble / (Int.MaxValue + 1), s)
    }

    // 6.5
    val nonNegativeInt2: Rand[Int] = map(rint)(i => if (i < 0) -(i + 1) else i)

    val double2: Rand[Double] = map(nonNegativeInt2)(_.toDouble / (Int.MaxValue + 1))

    // 6.3
    // ... definition in the book is problematic.
    // Double to be understood as a Double that belongs [0, 1]

    // 6.4
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      (0 until count).foldLeft[(List[Int], RNG)]((List.empty[Int], rng)) {
        case ((l, crng), _) =>
          val (ni, nrng) = crng.nextInt
          (ni :: l, nrng)
      }
    }
  }


}

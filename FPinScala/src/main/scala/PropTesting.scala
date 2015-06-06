package fpinscala;

case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  // List of the samples with random length.
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(i => Gen.listOfN(this, i))

  def unsized: SGen[A] =
    SGen(_ => this)
}

object Gen {

  def gen[A](sampler: RNG => (A, RNG)) =
    Gen(State(sampler))

  def unit[A](a: A): Gen[A] = gen { rng =>
    (a, rng)
  }

  def choose(a: Int, b: Int): Gen[Int] = gen { rng =>
    Random.map(Random.nonNegativeLessThan(b-a))(i => a + i)(rng)
  }

  def boolean: Gen[Boolean] =
    Gen(choose(0, 2).sample.map(_ == 1))

  def listOfN[A](a: Gen[A], n: Int): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(a.sample)))

  def listOf[A](a: Gen[A]): Gen[List[A]] =
    a.listOfN(choose(1, 101))

  def map[A, B](a: Gen[A])(f: A => B): Gen[B] =
    Gen(a.sample.map(f))

  def toOption[A](a: Gen[A])(f: A => Option[A]): Gen[Option[A]] =
    map(a)(f)

  def fromOption[A](a: Gen[Option[A]])(f: Option[A] => A): Gen[A] =
    map(a)(f)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val dbl = gen { Random.double }
    val ((ga, wa), (gb, wb)) = (g1, g2)
    val cutoff = wa / (wa + wb)
    dbl.flatMap(d => if (d < cutoff) ga else gb)
  }

}

/** Sized generator. */
case class SGen[A](forSize: Int => Gen[A])

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(i => g.listOfN(Gen.unit(i)))
}


trait Prop {
  import Prop.CheckResult

  def check: State[RNG, CheckResult]
  def &&(p: Prop): Prop = And(this, p)
}
case class GenerateAndCheck[A](a: Gen[A], f: A => Boolean, tests: Int = 100) extends Prop {
  import Prop.CheckResult
  def check = State { rng =>
    def loop(rng: RNG, i: Int): (CheckResult, RNG) = {
      if (i == tests)
        (Right(i), rng)
      else {
        val (sa, nrng) = a.sample.run(rng)
        if (!f(sa))
          (Left(s"Falsified after ${i} tests passed. \n. ARG: $sa", i), rng)
        else
          loop(nrng, i+1)
      }
    }
    loop(rng, 0)
  }
}
case class And(a: Prop, b: Prop) extends Prop {
  def check = State.map2(a.check, b.check)((ra, rb) => {
    (ra, rb) match {
      case (Right(ta), Right(tb)) => Right(ta + tb)
      case (Left((ea, ta)), Right(tb)) => Left(ea, ta + tb)
      case (Right(ta), Left((eb, tb))) => Left(eb, ta + tb)
      case (Left((ea, ta)), Left((eb, tb))) => Left(ea + "\n" + eb, ta + tb)
    }
  })
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type CheckResult = Either[(FailedCase, SuccessCount), SuccessCount]

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop =
    GenerateAndCheck(a, f)

}

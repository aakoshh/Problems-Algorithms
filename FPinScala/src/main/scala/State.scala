package fpinscala;


trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Random {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, next) = rng.nextInt
    val nni = if (i == Int.MinValue) Int.MaxValue else i.abs
    (nni, next)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, next) = nonNegativeInt(rng)
    val d = i.toDouble / Int.MaxValue.toDouble
    (d, next)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(n: Int, xs: List[Int], rng: RNG): (List[Int], RNG) = {
      if (n == count)
        (xs, rng)
      else {
        val (x, nrng) = rng.nextInt
        loop(n + 1, x :: xs, nrng)
      }
    }
    loop(0, List(), rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  // Given a state, return a constant value.
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, next) = s(rng)
    (f(a), next)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a,b), r2)
  }

  def mapDouble: Rand[Double] =
    map(Random.nonNegativeInt)(i => i.toDouble / Int.MaxValue.toDouble)

  def intDouble: Rand[(Int, Double)] =
    map2(nonNegativeInt, double)((_, _))


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldRight((List[A](), rng))((f, s) => {
      val (rs, rg) = s
      val (r, nrg) = f(rg)
      (r :: rs, nrg)
    })
  }

  def intSequence(n: Int): Rand[List[Int]] = {
    sequence(List.fill(n)(nonNegativeInt))
  }


  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = f(rng)
    val rb = g(a)
    val (b, rng2) = rb(rng1)
    (b, rng2)
  }

  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        unit { f(a, b) }
      }
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    })

}

// Generalization
object State {
  //type State[S, +A] = S => (A, S)

  def unit[S, A](a: A) = State((s: S) => (a, s))

  def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sa.flatMap(a =>
      sb.flatMap(b =>
        unit(f(a, b))))


  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    State {
      s => {
        // Collect the results of applying all computations,
        // passing the modified state along. Start with the first.
        val (rrs, fs) = ss.foldLeft((List.empty[A], s))((acc, si) => {
          val (rs, s) = acc
          val (r, ns) = si.run(s)
          (r :: rs, ns)
        })
        (rrs.reverse, fs)
      }
    }

  // Read the value of state.
  def get[S]: State[S, S] =
    State {
      s => (s, s)
    }

  // Replace the state with the given value,
  // returning nothing as result.
  def set[S](s: S): State[S, Unit] =
    State {
      _ => ((), s)
    }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State {
      s => {
        val (a, s1) = run(s)
        (f(a), s1)
      }
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State {
      s => {
        val (a, s1) = run(s)
        val sb = f(a)
        sb.run(s1)
      }
    }
}


object CandyMachine {
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  // Operate machine and return remaining contents.
  def operation(input: Input): State[Machine, (Int, Int)] = State {
    m => {
      val nm = input match {
        case Coin if !m.locked => m
        case Coin if m.candies > 0 => m.copy(locked = false, coins = m.coins + 1)
        case Coin if m.candies == 0 => m
        case Turn if m.locked => m
        case Turn if !m.locked => m.copy(locked = true, candies = m.candies - 1)
      }
      ((nm.coins, nm.candies), nm)
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val ops = inputs.map(operation)
    State.sequence(ops).map(_.last)
  }

}
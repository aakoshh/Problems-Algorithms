package fpinscala.monads

import org.scalacheck._, Prop.forAll, Arbitrary._
import scala.reflect.ClassTag

object MonadProperties extends Properties("Monad") {

  implicit def arbId[A](implicit ev: Arbitrary[A]) = Arbitrary {
    arbitrary[A] map (Id(_))
  }

  def monadLaws[F[_], A](name: String, m: Monad[F])
                        (implicit ev1: Arbitrary[F[A]],
                                  ev2: Arbitrary[A => F[A]],
                                  ev3: Arbitrary[A]) = {
    property(s"$name map") = forAll { (ma: F[A]) =>
      m.map(ma)(a => a) == ma
    }

    // Using the same A => F[A] for f and g for convenience. Should be F[B] and F[C]
    property(s"$name is associative") = forAll { (ma: F[A], f: A => F[A], g: A => F[A]) =>
      m.flatMap(m.flatMap(ma)(f))(g) == m.flatMap(ma){ a => m.flatMap(f(a))(g)}
    }

    property(s"$name compose is associative") = forAll { (a: A, f: A => F[A], g: A => F[A], h: A => F[A]) =>
      m.compose(f, m.compose(g, h))(a) == m.compose(m.compose(f, g), h)(a)
    }

    property(s"$name left identity") = forAll { (a: A, f: A => F[A]) =>
      m.compose(f, m.unit[A])(a) == f(a)
    }

    property(s"$name right identity") = forAll { (a: A, f: A => F[A]) =>
      m.compose(m.unit[A], f)(a) == f(a)
    }
  }

  monadLaws[List, Int]("listMonad", Monad.listMonad)
  monadLaws[Option, String]("optMonad", Monad.optMonad)
  monadLaws[Id, String]("idMonad", Monad.idMonad)

  property(s"sequence of options") = forAll { (as: List[Option[Int]]) =>
    Monad.optMonad.sequence(as) == (if (as.contains(None)) None else (Some(as.map(_.get))))
  }

  property(s"replication of list") = forAll(arbitrary[String], Gen.choose(0,5)) { (a: String, n: Int) =>
    Monad.listMonad.replicateM(n, List(a)) == List(List.fill(n)(a))
  }

  property(s"product of lists") = forAll { (as: List[Int], bs: List[String]) =>
    Monad.listMonad.product(as, bs) == (for { a <- as; b <- bs } yield (a,b))
  }

  property(s"replication of state") = forAll(arbitrary[Int], arbitrary[Int], Gen.choose(0,5)) { (a: Int, b: Int, n: Int) =>
    import fpinscala.State
    val sm = Monad.stateMonad[Int]

    def add(x: Int): State[Int, Int] =
      State { i => (i + x, i + x) }

    sm.replicateM[Int](n, add(a)).run(b)._2 == b + n * a
  }

  property(s"map2 of state") = forAll { (a: Int, b: Int, c: Int) =>
    import fpinscala.State
    val sm = Monad.stateMonad[Int]

    def add(x: Int): State[Int, Int] =
      State { i => (i + x, i + x) }

    def sub(x: Int): State[Int, Int] =
      State { i => (i - x, i - x) }

    val both = sm.map2(add(a), sub(b)) { (_, _) }
    both.run(c)._1 == (c+a, c+a-b)
  }
}
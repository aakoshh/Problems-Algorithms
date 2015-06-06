package fpinscala.monoids

import org.scalacheck._
import org.scalacheck.Prop.forAll
import scala.reflect.ClassTag

object MonoidProperties extends Properties("Monoid") {

  def monoidLaws[A](name: String, m: Monoid[A])(implicit ev: Arbitrary[A]) = {
    property(s"$name op is associative") = forAll { (a: A, b: A, c: A) =>
      m.op(m.op(a, b), c) == m.op(a, m.op(b, c))
    }

    property(s"$name op is commutative in zero") = forAll { (a: A) =>
      m.op(a, m.zero) == m.op(m.zero, a)
    }
  }

  def monoidLawsFun[A,B](name: String, m: Monoid[A => B])
                   (implicit ev1: Arbitrary[A], ev2: Arbitrary[A => B]) = {
    property(s"$name function application is associative") =
      forAll { (a: A => B, b: A => B, c: A => B, x: A) =>
        m.op(m.op(a, b), c)(x) == m.op(a, m.op(b, c))(x)
      }

    property(s"$name function is commutative in zero") = forAll { (a: A => B, x: A) =>
      m.op(a, m.zero)(x) == m.op(m.zero, a)(x)
    }
  }

  // Has to come before the functions that use it.
  implicit val intEndos = Arbitrary(Gen.oneOf(Seq(
    (x: Int) => x + 1, (x: Int) => x * 2, (x: Int) => x - 3)))

  monoidLaws("intAddition", Monoids.intAddition)
  monoidLaws("intMultiplication", Monoids.intMultiplication)
  monoidLaws("booleanOr", Monoids.booleanOr)
  monoidLaws("booleanAnd", Monoids.booleanAnd)
  monoidLaws("option", Monoids.optionMonoid[Int])
  monoidLawsFun("endo", Monoids.endoMonoid[Int])

  property(s"concatenate reduces list") = forAll { (a: List[Int]) =>
    Monoids.concatenate(a, Monoids.intAddition) == a.sum
  }

  property(s"foldMap reduces list after mapping") = forAll { (a: List[String]) =>
    Monoids.foldMap(a, Monoids.intAddition)(_.size) == a.foldLeft(0)(_ + _.size)
  }
}
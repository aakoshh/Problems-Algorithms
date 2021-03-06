package fpinscala.monoids

import org.scalacheck._, Prop.forAll, Arbitrary._
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

  implicit val arbWC: Arbitrary[WC] = {
    val genS = arbitrary[String] map (Stub(_))

    val genP = for {
      a <- arbitrary[String]
      b <- arbitrary[String]
      c <- arbitrary[Int]
    } yield Part(a,c,b)

    val genWC = for {
      stop <- arbitrary[Boolean]
      wc   <- if (stop) genS else genP
    } yield wc

    Arbitrary(genWC)
  }

  monoidLaws("intAddition", Monoids.intAddition)
  monoidLaws("intMultiplication", Monoids.intMultiplication)
  monoidLaws("booleanOr", Monoids.booleanOr)
  monoidLaws("booleanAnd", Monoids.booleanAnd)
  monoidLaws("option", Monoids.optionMonoid[Int])
  monoidLawsFun("endo", Monoids.endoMonoid[Int])
  monoidLawsFun("fun", Monoids.functionMonoid[Int, String](Monoids.stringConcat))
  monoidLaws("wc", Monoids.wcMonoid)
  monoidLaws("product", Monoids.productMonoid(Monoids.intAddition, Monoids.booleanAnd))
  monoidLaws("mapMerge", Monoids.mapMerge[String, Int](Monoids.intMultiplication))

  property(s"concatenate reduces list") = forAll { (a: List[Int]) =>
    Monoids.concatenate(a, Monoids.intAddition) == a.sum
  }

  property(s"foldMap reduces list after mapping") = forAll { (a: List[String]) =>
    Monoids.foldMap(a, Monoids.intAddition)(_.size) == a.foldLeft(0)(_ + _.size)
  }

  property(s"foldLeft works with monoid") = forAll { (as: List[Int], b: Int) =>
    Monoids.foldLeft(b)(as)(_ + _)(Monoids.intAddition) == as.sum + b
  }

  property(s"foldMapV") = forAll { (as: Vector[String]) =>
    Monoids.foldMapV(as, Monoids.stringConcat)(identity) == as.mkString("")
  }

  property(s"isOrdered") = forAll { (as: Vector[Int]) =>
    Monoids.isOrdered(as) == as.size <= 1 || (1 to as.size-1).forall(i => as(i-1) <= as(i))
  }

  property(s"wordCount") = forAll { (s: String) =>
    Monoids.wordCount(s) == s.split(" ").filterNot(_ == "").size
  }

  property(s"bag") = forAll { (as: Vector[String]) =>
    Monoids.bag(as) == as.groupBy(identity).mapValues(_.length)
  }
}
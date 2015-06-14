package fpinscala.monoids

import fpinscala.Par
import fpinscala.Parallel.Par

/** Monoid laws:
  * op(a, op(b, c)) == op(op(a,b), c)
  * op(a, zero) == op(zero, a)
  */
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoids {
  val intAddition = new Monoid[Int] {
    def op(a: Int, b: Int) = a + b
    def zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a: Int, b: Int) = a * b
    def zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean) = a || b
    def zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean) = a && b
    def zero = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a: Option[A], b: Option[A]) = a orElse b
    def zero = None
  }

  /** Monoid for a endofunctions, which have the same input and output type. */
  def endoMonoid[A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) = f andThen g
    def zero = identity
  }

  /** Monoid for functions whose results are monoids. */
  def functionMonoid[A, B](mb: Monoid[B]) = new Monoid[A => B] {
    def zero = (_: A) => mb.zero
    def op(f: A => B, g: A => B) = (x: A) => mb.op(f(x), g(x))
  }

  /** Tuples of monoids are monoids themselves */
  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]) = new Monoid[(A, B)] {
    def zero = (ma.zero, mb.zero)
    def op(ab1: (A,B), ab2: (A,B)) = (ma.op(ab1._1, ab2._1), mb.op(ab1._2, ab2._2))
  }

  def stringConcat = new Monoid[String] {
    def op(a: String, b: String) = a + b
    def zero = ""
  }

  def mapMerge[K, V](mv: Monoid[V]) = new Monoid[Map[K, V]] {
    def zero = Map.empty[K, V]
    def op(a: Map[K, V], b: Map[K, V]) = {
      (a.keySet ++ b.keySet).foldLeft(Map[K, V]()) { (acc, k) =>
        val va = a.get(k).getOrElse(mv.zero)
        val vb = b.get(k).getOrElse(mv.zero)
        acc + (k -> mv.op(va, vb))
      }
    }
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  /** Turn a list of items that don't have a monoid instance to an object that is. */
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as map f, m)

  /** foldLeft in terms of foldMap */
  def foldLeft[A, B](z: B)(as: List[A])(f: (B, A) => B)(implicit m: Monoid[B]): B = {
    // Use foldMap to reduce the list. B is a monoid so it should be commutative and associative.
    val b = foldMap(as, m)(a => f(m.zero, a))
    // Combining the seed with the folded result should be all the same to a monoid.
    m.op(z, b)
  }

  /** Fold a list by splitting in 2 parts recursively. */
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    def loop(v: IndexedSeq[A]): B = {
      if (v.size <= 1) {
        (v map f).headOption.getOrElse(m.zero)
      } else {
        val (left, right) = v.splitAt(v.size / 2)
        m.op(loop(left), loop(right))
      }
    }
    loop(as)
  }

  /** Given a monoid, create a new one that can combine
    * parallel computations on the underlying type. */
  def par[A](m: Monoid[A]) = new Monoid[Par[A]] {
    def zero =
      Par.unit(m.zero)

    def op(a: Par[A], b: Par[A]): Par[A] =
      Par.map2(a, b)(m.op)
  }

  /** Fold list in parallel. */
  def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val pm = par(m)
    def loop(v: IndexedSeq[A]): Par[B] = {
      if (v.size <= 1) {
        v.map(x => Par.unit(f(x)))
         .headOption.getOrElse(pm.zero)
      } else {
        val (l, r) = v.splitAt(v.size / 2)
        pm.op(loop(l), loop(r))
      }
    }
    loop(as)
  }

  sealed trait OrderCheck[+A]
  object OrderCheck {
    case object Empty extends OrderCheck[Nothing]
    case object OutOfOrder extends OrderCheck[Nothing]
    case class InOrder[A](min: A, max: A) extends OrderCheck[A]
  }

  def order[A <% Ordered[A]] = new Monoid[OrderCheck[A]] {
    import OrderCheck._
    def zero = Empty
    def op(a: OrderCheck[A], b: OrderCheck[A]) = (a, b) match {
      case (Empty, x) => b
      case (a, Empty) => a
      case (InOrder(mina, maxa), InOrder(minb, maxb)) if maxa <= minb =>
        InOrder(mina, maxb)
      case _ =>
        OutOfOrder
    }
  }

  /** Check whether some ints are ordered using foldMap. */
  def isOrdered(xs: IndexedSeq[Int]): Boolean = {
    foldMapV(xs, order[Int])(x => OrderCheck.InOrder(x,x)) match {
      case OrderCheck.OutOfOrder => false
      case _ => true
    }
  }

  val wcMonoid = new Monoid[WC] {
    val zero = Stub("")
    def op(a: WC, b: WC): WC = (a, b) match {
      case (Stub(" "), Stub(b)) =>
        Part("", 0, b)
      case (Stub(a), Stub(" ")) =>
        Part(a, 0, "")
      case (Stub(a), Stub(b)) =>
        Stub(a + b)
      case (Stub(a), Part(lb, cnt, rb)) =>
        Part(a + lb, cnt, rb)
      case (Part(la, cnt, ra), Stub(b)) =>
        Part(la, cnt, ra + b)
      case (Part(la, ca, ra), Part(lb, cb, rb)) =>
        Part(la, ca + cb + (if (ra == "" && lb == "") 0 else 1), rb)
    }
  }

  def wordCount(text: String): Int = {
    def sc(c: String) = if (c == "") 0 else 1
    foldMapV(text.toVector, wcMonoid)(c => Stub(c.toString)) match {
      case Stub(a)       => sc(a)
      case Part(a, c, b) => sc(a) + c + sc(b)
    }
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val m = mapMerge[A, Int](intAddition)
    foldMapV(as, m)(a => Map(a -> 1))
  }
}

sealed trait WC
case class Stub(chars: String) extends WC // Incomplete word.
case class Part(lStub: String, words: Int, rStub: String) extends WC

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B

  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero) { (b,a) => mb.op(b, f(a)) }

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List.empty[A])(_ :: _)
}

object Foldable {
  implicit val foldableList = new Foldable[List] {
    def foldRight[A,B](as: List[A])(z: B)(f: (A,B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A,B](as: List[A])(z: B)(f: (B,A) => B): B =
      as.foldLeft(z)(f)
  }

  implicit val foldableIndexedSeq = new Foldable[IndexedSeq] {
    def foldRight[A,B](as: IndexedSeq[A])(z: B)(f: (A,B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A,B](as: IndexedSeq[A])(z: B)(f: (B,A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A,B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      Monoids.foldMapV(as, mb)(f)
  }

  implicit val foldableOption = new Foldable[Option] {
    def foldRight[A,B](as: Option[A])(z: B)(f: (A,B) => B): B = {
      as map (f(_, z)) getOrElse z
    }
    def foldLeft[A,B](as: Option[A])(z: B)(f: (B,A) => B): B = {
      as map (f(z, _)) getOrElse z
    }
  }
}
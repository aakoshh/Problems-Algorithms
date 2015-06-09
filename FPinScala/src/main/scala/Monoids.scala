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

  def stringConcat = new Monoid[String] {
    def op(a: String, b: String) = a + b
    def zero = ""
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
}

sealed trait WC
case class Stub(chars: String) extends WC // Incomplete word.
case class Part(lStub: String, words: Int, rStub: String) extends WC
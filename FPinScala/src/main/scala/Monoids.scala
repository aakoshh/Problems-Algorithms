package fpinscala.monoids

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

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  /** Turn a list of items that don't have a monoid instance to an object that is. */
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as map f, m)

}
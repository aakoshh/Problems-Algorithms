package fpinscala.applicatives

import fpinscala.monads.Functor

trait Applicative[F[_]] extends Functor[F] {
  // Primitive function are unit and map2. It doesn't have flatMap and join.
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C]
  def unit[A](a: A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(Unit))((a, _) => f(a))

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = {
    as.foldRight(unit(List.empty[B])) { (a, acc) =>
      map2(f(a), acc)(_ :: _)
    }
  }

  def sequence[A](as: List[F[A]]): F[List[A]] =
    traverse(as)(identity)

  def replacateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((_, _))

  // An alternative primitive set of functions are unit and apply.
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]

  def mapViaApply[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2ViaApply[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = {
    val f1 = (a: A) => (b: B) => f(a,b) // f.curried
    val fab: F[B => C] = mapViaApply(fa)(f1)
    apply(fab)(fb)
  }



  def applyViaMap2[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab,a) => ab(a))

}
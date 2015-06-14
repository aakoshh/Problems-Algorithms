package fpinscala.monads

import fpinscala.State

trait Monad[F[_]] {
  def unit[A](a: A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    lma.foldRight(unit(List[A]())) { (ma, macc) =>
      map2(ma, macc) { (a, acc) =>
        a :: acc
      }
    }
  }

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = {
    sequence(la map f)
  }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    sequence(List.fill(n)(ma))
  }
}

object Monad {
  val listMonad = new Monad[List] {
    def unit[A](a: A) =
      List(a)
    def flatMap[A,B](ma: List[A])(f: A => List[B]) =
      ma flatMap f
  }

  val optMonad = new Monad[Option] {
    def unit[A](a: A): Option[A] =
      Some(a)
    def flatMap[A,B](ma: Option[A])(f: A => Option[B]) =
      ma flatMap f
  }

  /** The State monad has a fixed S state and another type
    * type parameter which varies per method. Use a type
    * alias to make it work with the Monad which expects
    * a single type parameter.
    */
  def stateMonad[S] = {
    type StateS[A] = State[S, A]

    new Monad[StateS] {
      def unit[A](a: A) =
        State.unit[S, A](a)

      def flatMap[A,B](ma: StateS[A])(f: A => StateS[B]) =
        ma flatMap f
    }
  }
}
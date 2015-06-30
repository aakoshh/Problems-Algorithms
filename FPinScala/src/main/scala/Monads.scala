package fpinscala.monads

import fpinscala.State

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

/** Type class for a monad handling a specific higher kinded type. */
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def flatten[A](mma: F[F[A]]): F[A] = {
    flatMap(mma)(ma => ma)
  }

  def join[A](mma: F[F[A]]): F[A] = {
    flatten(mma)
  }

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

  def product[A,B](a: F[A], b: F[B]): F[(A,B)] = {
    map2(a,b)((_,_))
  }

  /** Filter by function that returns a monad, and return overall monad.
    * For example filter returns a future, end result is future of list. */
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    val mbs: F[List[Boolean]] = traverse(ms)(f)
    map(mbs) { bs => (ms zip bs) filter (_._2) map (_._1) }
  }

  /** Combine to Kleisli arrows (A => F[B]). */
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = {
    a => flatMap(f(a))(b => g(b))
  }

  /** unit and compose would be another minimal combo */
  def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    val seed = (_: Unit) => ma
    compose(seed, f)(Unit) // Run it with unit.
  }

  /** map unit and join are also minimal */
  def flatMapViaJoinAndMap[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    join { map(ma)(f) }
  }

  def composeViaJoinAndMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = {
    a => join { map(f(a))(g) }
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
    //type StateS[A] = State[S, A]
    // If used like `new Monad[StateS]` it doesn't compile
    // when I try to call it. Type lambda works.

    new Monad[({type StateS[A] = State[S, A]})#StateS] {
      def unit[A](a: A) =
        State.unit[S, A](a)

      def flatMap[A,B](ma: State[S,A])(f: A => State[S,B]) =
        ma flatMap f
    }
  }

  val idMonad = new Monad[Id] {
    def unit[A](a: A) =
      Id(a)
    def flatMap[A, B](ma: Id[A])(f: A => Id[B]) =
      ma flatMap f
  }


  def readerMonad[R] = {
    new Monad[({type f[x] = Reader[R, x]})#f] {
      /** Read constant value. */
      def unit[A](a: A) =
        Reader((_: R) => a)

      /** Read a value and based on that decide what other value to read and return. */
      def flatMap[A, B](ma: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
        Reader { (r: R) =>
          val a = ma.run(r)
          f(a).run(r)
        }

      // sequence: Read all values in a list.
    }
  }
}

/** Data structure for the Identity monad. */
case class Id[A](value: A) {
  def flatMap[B](f: A => Id[B]) = f(value)
  def map[B](f: A => B)         = Id(f(value))
}

case class Reader[R, A](run: R => A)
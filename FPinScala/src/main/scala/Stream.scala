package fpinscala

// +A means a stream of Lions can become a Stream of Animals
sealed trait Stream[+A] {
  import Stream.{cons, empty}

  def toList(): List[A] = {
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList()
    }
  }

  // Take n elements or the whole stream if there is less.
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 =>
      cons(h(), t().take(n-1))
    case _ => Empty
  }

  // Drop the first n elements and return the rest
  // or an empty stream if there are less than n.
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case s => s
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) =>
      cons(h(), t().takeWhile(p))
    case _ =>
      Empty
  }

  // Fold from right to left but passing the state
  // computation to the function by name so it can
  // decide not to evaluate it.
  def foldRight[B](s: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(s)(f))
    case Empty => s
  }

  def exists(p: A => Boolean) = {
    foldRight(false)((h, rest) => p(h) || rest)
  }

  def forAll(p: A => Boolean) = {
    foldRight(true)((h, rest) => p(h) && rest)
  }

  def takeWhileFold(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, rest) => if (p(h)) cons(h, rest) else Empty)
  }

  def headOption() : Option[A] = {
    foldRight(None: Option[A])((h, rest) => Some(h))
  }

  def map[R](f: A => R): Stream[R] = {
    foldRight(empty[R])((h, rest) => cons(f(h), rest))
  }

  def filter(p: A => Boolean) = {
    foldRight(empty[A])((h, rest) => if (p(h)) cons(h, rest) else rest)
  }

  // Need to be a supertype of the elements in this stream.
  def append[B >: A](other: => Stream[B]) = {
    foldRight(other)((h, rest) => cons(h, rest))
  }

  def flatMap[B](f: A => Stream[B]) = {
    foldRight(empty[B])((h, rest) => f(h).append(rest))
  }

  def unfoldMap[B](f: A => B): Stream[B] = {
    Stream.unfold(this)(s => s match {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    })
  }

  def unfoldTake(n: Int): Stream[A] = {
    Stream.unfold((n, this))(pair => {
      val (n, s) = pair
      s match {
        case Cons(h, t) if n > 0 => Some((h(), (n-1, t())))
        case _ => None
      }
    })
  }

  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] = {
    Stream.unfold((this, other))(pair => {
      pair match {
        case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
        case (Cons(ha, ta), Empty) => Some((Some(ha()), None), (ta(), Empty))
        case (Empty, Cons(hb, tb)) => Some((None, Some(hb())), (Empty, tb()))
        case (Empty, Empty) => None
      }
    })
  }

  def startsWith[A](prefix: Stream[A]): Boolean = {
    this.zipAll(prefix).foldRight(true) {(pair, rest) =>
      pair match {
        case (_, None) => true
        case (Some(a), Some(b)) => a == b && rest
        case _ => false
      }
    }
  }

  def tails(): Stream[Stream[A]] = {
    Stream.unfold(Some(this): Option[Stream[A]])(ss =>
      ss match {
        case Some(s @ Cons(h, t)) => Some(s, Some(t()))
        case Some(Empty) => Some(Empty, None)
        case None => None
      })
  }

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    tails().map(t => t.foldRight(z)(f))
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

// Stream constructors.
object Stream {

  def cons[A](h: => A, t: => Stream[A]) = {
    // memoize the pass by name parameters once computed by using lazy
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  // Helper method so we get a stream, not the Empty class itself. Aids type inference.
  def empty[A]() : Stream[A] = Empty

  def apply[A](items: A*) : Stream[A] = {
    if (items.isEmpty)
      Empty
    else
      cons(items.head, apply(items.tail: _*))
  }


  def constant[A](x: A): Stream[A] = {
    // Lazy is needed for the recursive definition to work.
    lazy val xs: Stream[A] = cons(x, xs)
    xs
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  def fibs(): Stream[Int] = {
    def loop(a: Int, b: Int) : Stream[Int] = {
      cons(a, loop(b, a + b))
    }
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]) : Stream[A] = {
    f(z) match {
      case Some((x, z)) => cons(x, unfold(z)(f))
      case None => Empty
    }
  }

  def unfoldFibs(): Stream[Int] = {
    def step(pair: (Int, Int)) = {
      val (cur, nxt) = pair
      Some(cur, (nxt, cur + nxt))
    }
    unfold(0,1)(step)
  }

  def unfoldFrom(n: Int): Stream[Int] = {
    unfold(n)(n => Some((n, n+1)))
  }

  def unfoldConstant[A](x: A): Stream[A] = {
    unfold(x)(x => Some((x, x)))
  }

  def unfoldOnes() = unfold(1)(_ => Some((1, 1)))
}
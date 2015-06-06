package fpinscala

import java.util.concurrent._


object Par {
  import Parallel.Par

  /* Elevate a constant value to be a parallel computation. */
  def unit[A](a: A): Par[A] =
    es => new Future[A]() {
      def get: A = a
      def get(timeout: Long, unit: TimeUnit) = a
      def cancel(mayInterruptIfRunning: Boolean): Boolean = false
      def isCancelled: Boolean = false
      def isDone: Boolean = true
    }

  /* Run and extract the results of a parallel computation. */
  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  /* Explicitly run a parallel computation on a separate thread */
  def fork[A](p: => Par[A]): Par[A] =
    es => {
      def task = new Callable[A](){
        def call() = run(es)(p).get
      }
      es.submit(task)
    }

  /* Evaluate an expression thunk on a separate thread. */
  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  /* Convert a synchronous function to an async one */
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A,B) => C): Par[C] =
    es => {
      // Start them both before blocking on get.
      val fa = run(es)(pa)
      val fb = run(es)(pb)
      new Future[C]() {
        def cancel(mayInterruptIfRunning: Boolean): Boolean =
          fa.cancel(mayInterruptIfRunning) && fb.cancel(mayInterruptIfRunning)
        def isCancelled: Boolean =
          fa.isCancelled || fb.isCancelled
        def isDone: Boolean =
          fa.isDone && fb.isDone
        def get(): C =
          f(fa.get, fb.get)
        def get(timeout: Long, unit: TimeUnit): C = {
          val timeoutMillis = unit.toMillis(timeout)
          val start = System.currentTimeMillis
          val a = fa.get(timeoutMillis, TimeUnit.MILLISECONDS)
          val left = Math.max(0, timeoutMillis - (System.currentTimeMillis - start))
          val b = fb.get(left, TimeUnit.MILLISECONDS)
          f(a,b)
        }
      }
    }

  def map[A, B](p: Par[A])(f: A => B): Par[B] =
    map2(p, unit(()))((a, _) => f(a))

  def sortPar(lst: Par[List[Int]]): Par[List[Int]] =
    map(lst)(_.sorted)


  /* Collect parallel results. */
  def sequenceFuture[A](ps: List[Par[A]]): Par[List[A]] =
    es => {
      val fs: List[Future[A]] = ps.map(run(es)(_))
      new Future[List[A]]() {
        def cancel(mayInterruptIfRunning: Boolean): Boolean =
          fs.forall(_.cancel(mayInterruptIfRunning))
        def isCancelled: Boolean =
          fs.exists(_.isCancelled)
        def isDone: Boolean =
          fs.forall(_.isDone)
        def get(): List[A] =
          fs.map(_.get)
        def get(timeout: Long, unit: TimeUnit): List[A] = {
          val (_, xs) = fs.foldLeft((unit.toMillis(timeout), List[A]()))((pair, f) => {
            val (countdown, xs) = pair
            val start = System.currentTimeMillis
            val x = f.get(countdown, TimeUnit.MILLISECONDS)
            val left = Math.max(0, countdown - (System.currentTimeMillis() - start))
            (left, x :: xs)
          })
          xs.reverse
        }
      }
    }

  /* Combining existing functions to do sequence but not really parallel. */
  def sequenceFold[A](ps: List[Par[A]]): Par[List[A]] = {
    val acc: Par[List[A]] = unit(Nil)
    ps.foldRight(acc)((px, pxs) =>
      map2(px, pxs)((x, xs) =>
        x :: xs))
  }

  /* Divide and conquer. */
  def sequenceSplit[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    // Need to be careful with this as futures start futures
    // and in case the fixed thread executor is already executing
    // the as many as it can it can never finish anything.
    fork {
      if (ps.length == 0)
        unit(Vector())
      else if (ps.length == 1)
        map(ps.head)(Vector(_))
      else {
        val (l, r) = ps.splitAt(ps.length / 2)
        map2(sequenceSplit(l), sequenceSplit(r))(_ ++ _)
      }
    }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    // Splitting doesn't work with 4 threads.
    // map(sequenceSplit(ps.toIndexedSeq))(_.toList)
    sequenceFuture(ps)

  /* Map over a list in parallel. */
  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    fork {
      val bs: List[Par[B]] = as.map(asyncF(f))
      sequence(bs)
    }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val lpls: List[Par[List[A]]] = as.map(asyncF(a => if (f(a)) List(a) else List()))
    val plls = sequence(lpls)
    map(plls)(_.flatten)
  }

  /* Execute a function which tells us which of the choices to use to generate the final result */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val i = run(es)(n).get
      run(es)(choices(i))
    }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]) = {
    val pick = map(cond)(x => if (x) 0 else 1)
    choiceN(pick)(List(t, f))
  }

  def choiceMap[K,V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      val k = run(es)(key).get
      run(es)(choices(k))
    }

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    es => {
      val a = run(es)(pa).get
      val pb = f(a)
      run(es)(pb)
    }

  def choiceViaMap[A](cond: Par[Boolean])(t: Par[A], f: Par[A]) =
    flatMap(cond)(c => if (c) t else f)

  def choiceNViaMap[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n)(i => choices(i))

  def join[A](a: Par[Par[A]]): Par[A] =
    es => {
      val pa = run(es)(a).get
      run(es)(pa)
    }

  def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = {
    val pb: Par[Par[B]] = map(pa)(a => f(a))
    join(pb)
  }

  def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] = {
    flatMap(ppa)(identity)
  }
}


object Parallel {
  type Par[A] = ExecutorService => Future[A]

  def execute[A](p: Par[A]): A = {
    val es = Executors.newFixedThreadPool(4)
    try {
      Par.run(es)(p).get
    }
    finally {
      es.shutdown()
    }
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.length <= 1)
      Par.unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
  }
}
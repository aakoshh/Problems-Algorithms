import fpinscala.Stream

val ones: Stream[Int] = Stream.cons(1, ones)
println(ones.take(5).toList)
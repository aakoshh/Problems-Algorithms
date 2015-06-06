import fpinscala.{Par, Parallel}
import org.scalatest.FunSuite


class ParallelSuite extends FunSuite {
  test("Sum of ints can be calculated") {
    val xs = Vector(1,2,3,4)
    assert(Parallel.execute(Parallel.sum(xs)) == 10)
  }

  test("Parallel map works") {
    val xs = List(1,2,3)
    val ys = Parallel.execute(Par.parMap(xs)(_ * 2))
    assert(ys == List(2,4,6))
  }
}

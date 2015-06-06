import org.scalatest.FunSuite
import fpinscala.{Random, SimpleRNG, State, CandyMachine}


class StateSuite extends FunSuite  {

  val rng = SimpleRNG(42)

  test("Non negative int can be generated") {
    assert(Random.nonNegativeInt(rng)._1 >= 0)
  }

  test("Double between 0 and 1") {
    val d = Random.double(rng)._1
    assert(d >= 0 && d < 1)
  }

  test("Double between 0 and 1 with map") {
    val d = Random.mapDouble(rng)._1
    assert(d >= 0 && d < 1)
  }

  test("Can generate list of random ints") {
    val lst = Random.ints(5)(rng)._1
    assert(lst.length == 5)
    assert(lst.head != lst.tail.head)
  }

  test("Can generate sequence of random values") {
    val doubles = Random.sequence(List(Random.mapDouble, Random.mapDouble, Random.double _))
    val ds = doubles(rng)._1
    assert(ds.size == 3)
  }

  test("Can generate int sequence") {
    val ints = Random.intSequence(5)(rng)._1
    assert(ints.size == 5)
  }

  test("Can generate random value less than n") {
    val i = Random.nonNegativeLessThan(5)(rng)._1
    assert(i >= 0 && i < 5)
  }

  test("State sequence is applied left to right.") {
    val add1 = State[Int, Int](x => (x + 1, x + 1))
    val mul3 = State[Int, Int](x => (x * 3, x * 3))
    val all = State.sequence(List(add1, mul3))
    val res = all.run(1)
    assert(res._1 == List(2, 6))
    assert(res._2 == 6)
  }

  test("Candy machine calculates final contents") {
    import CandyMachine.{Turn, Coin, Machine}
    val machine = Machine(locked = true, candies = 5, coins = 10)
    val ops = List(Coin, Turn, Turn, Coin, Coin, Turn, Coin, Turn, Coin, Turn, Turn)
    val result = CandyMachine.simulateMachine(ops).run(machine)
    assert(result._1 == (14, 1))
  }
}

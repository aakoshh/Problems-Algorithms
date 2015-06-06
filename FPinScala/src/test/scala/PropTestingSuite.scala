import fpinscala.{SimpleRNG, Gen}
import fpinscala.Prop.forAll
import org.scalatest.FunSuite


class PropTestingSuite extends FunSuite {

  val rng = SimpleRNG(42)

  test("Random int can be generated") {
    val ints = Gen.choose(50, 100)
    val i = ints.sample.run(rng)._1
    assert(50 <= i && i < 100)
  }

  test("List of random ints can be generated") {
    val ints = Gen.listOfN(Gen.choose(0, 100), 5)
    assert(ints.sample.run(rng)._1.length == 5)

    val (fst, rng1) = ints.sample.run(rng)
    val (snd, rng2) = ints.sample.run(rng1)
    assert(fst != snd)
  }

  test("List of random ints of random length can be generated") {
    val ints = Gen.choose(10, 15).listOfN(Gen.choose(1, 4))
    val sample = ints.sample.run(rng)._1
    assert(sample.length >= 1 && sample.length <= 3)
    assert(sample(1) >= 10 && sample(1) < 15)
  }

  test("Positive tests can pass") {
    val intList = Gen.listOf(Gen.choose(0,100))
    val prop = forAll(intList)(ns => ns.reverse.reverse == ns) &&
               forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
    val (result, _) = prop.check.run(rng)
    assert(result.isRight)
  }

  test("Failing tests return description") {
    val intList = Gen.listOf(Gen.choose(0,100))
    val prop = forAll(intList)(ns => ns.reverse == ns)
    val (result, _) = prop.check.run(rng)
    assert(result.isLeft)
  }

  test("Unions can be generated") {
    val a = Gen.choose(0, 10)
    val b = Gen.choose(10, 20)
    val ab = Gen.listOfN(Gen.union(a, b), 100)
    val sample = ab.sample.run(rng)._1

    assert(sample.exists(_ < 10))
    assert(sample.exists(_ > 10))
  }

  test("Weighted generates according to expectations.") {
    val a = Gen.unit(1)
    val b = Gen.unit(2)
    val ab = Gen.weighted((a, 1), (b, 4)).listOfN(Gen.unit(1000))
    val sample = ab.sample.run(rng)._1

    assert(sample.count(_ == 2) >= 700 && sample.count(_ == 2) <= 900)
  }
}

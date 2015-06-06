import org.scalatest.FunSuite
import fpinscala.Stream

class StreamSuite extends FunSuite {

  test("Stream should be convertible to a list") {
    assert(Stream(1, 2, 3).toList == List(1, 2, 3))
  }

  test("Can take n elements from the stream.") {
    assert(Stream(1, 2, 3).take(2).toList == List(1, 2))
  }

  test("Can take n elements from the stream with unfold.") {
    assert(Stream(1, 2, 3).unfoldTake(2).toList == List(1, 2))
  }

  test("Take returns the whole list if asked for longer.") {
    assert(Stream(1, 2, 3).take(5).toList == List(1, 2, 3))
  }

  test("Can drop n elements from the stream.") {
    assert(Stream(1, 2, 3).drop(2).toList == List(3))
  }

  test("Drop returns empty list if asked to drop more.") {
    assert(Stream(1, 2, 3).drop(5).toList == List())
  }

  test("TakeWhile should stop at the first item that doesn't pass.") {
    assert(Stream(1, 2, 3, 2, 1).takeWhile(_ <= 2).toList == List(1, 2))
  }

  test("TakeWhileFold should stop at the first item that doesn't pass.") {
    assert(Stream(1, 2, 3, 2, 1).takeWhileFold(_ <= 2).toList == List(1, 2))
  }

  test("Exists finds matching element.") {
    assert(Stream(1, 2, 3).exists(_ == 2) == true)
  }

  test("Exists is false if element not found.") {
    assert(Stream(1, 2, 3).exists(_ < 0) == false)
  }

  test("Exists is false for empty.") {
    assert(Stream().exists(_ == 2) == false)
  }

  test("ForAll true if all pass.") {
    assert(Stream(1, 2, 3).forAll(_ <= 3) == true)
  }

  test("ForAll false if one fails.") {
    assert(Stream(1, 2, 3).forAll(_ != 2) == false)
  }

  test("ForAll is true for empty.") {
    assert(Stream().forAll(_ == 2) == true)
  }

  test("Head option returns first element") {
    assert(Stream(1, 2).headOption == Some(1))
  }

  test("Map works on all elements") {
    assert(Stream(1, 2, 3).map(_ * 2).toList == List(2, 4, 6))
  }

  test("Map with map works on all elements") {
    assert(Stream(1, 2, 3).unfoldMap(_ * 2).toList == List(2, 4, 6))
  }

  test("Filter works on all elements") {
    assert(Stream(1, 2, 3).filter(_ % 2 == 1).toList == List(1, 3))
  }

  test("Append streams all elements") {
    assert(Stream(1, 2).append(Stream(3, 4)).toList == List(1,2,3,4))
  }

  test("Flatmap appends all results") {
    assert(Stream(1, 2, 3).flatMap(x => Stream(x, -x)).toList == List(1, -1, 2, -2, 3, -3))
  }

  test("Constant creates infinite stream") {
    assert(Stream.constant("a").take(3).toList == List("a", "a", "a"))
  }

  test("From creates increasing sequence") {
    assert(Stream.from(5).take(3).toList() == List(5,6,7))
  }

  test("Fibonacci series is generated") {
    assert(Stream.fibs.take(8).toList() == List(0, 1, 1, 2, 3, 5, 8, 13))
  }

  test("Fibonacci with unfold") {
    assert(Stream.unfoldFibs.take(8).toList() == List(0, 1, 1, 2, 3, 5, 8, 13))
  }

  test("From with fold yields increasing sequence") {
    assert(Stream.unfoldFrom(5).take(3).toList() == List(5,6,7))
  }

  test("Constant with unfold creates infinite stream") {
    assert(Stream.unfoldConstant("a").take(3).toList == List("a", "a", "a"))
  }

  test("ZipAll works until both streams are empty") {
    val s1 = Stream(1, 2, 3)
    val s2 = Stream("a", "b")
    val lst = s1.zipAll(s2).toList()
    assert(lst == List((Some(1), Some("a")), (Some(2), Some("b")), (Some(3), None)))
  }

  test("StartsWith for prefix returns true") {
    assert(Stream(1,2,3).startsWith(Stream(1,2)) == true)
  }

  test("StartsWith for non-prefix returns false") {
    assert(Stream(1,2).startsWith(Stream(1, 2, 3)) == false)
  }

  test("Tails returns all postfixes") {
    val ts = Stream(1,2,3).tails().map(s => s.toList).toList
    assert(ts == List(List(1,2,3), List(2,3), List(3), List()))
  }

  test("ScanRight produces folds of all tails") {
    assert(Stream(1,2,3).scanRight(0)(_ + _).toList == List(6, 5, 3, 0))
  }

}

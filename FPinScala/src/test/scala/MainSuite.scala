import org.scalatest.FunSuite

class MainSuite extends FunSuite {
  test("counting an empty collection") {
    assert(Main.count(Array[Int]()) == 0)
    assert(Main.count(Map()) == 0)
    assert(Main.count(Set()) == 0)
  }

  test("counting a non-empty collection") {
    assert(Main.count(Array(1)) == 1)
    assert(Main.count(Map(1 -> 1)) == 1)
    assert(Main.count(Set(1)) == 1)
  }
}
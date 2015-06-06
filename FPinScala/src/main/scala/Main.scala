object Main {
  def main(args: Array[String]) {
    println("Hello, World!")
    println("# of arguments: %d" format count(args))
  }

  def count[T](it: Iterable[T]): Int = it.size
}
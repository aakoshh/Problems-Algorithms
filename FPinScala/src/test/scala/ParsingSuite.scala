import org.scalatest.FunSuite
import fpinscala.parsing._

class ParsingSuite extends FunSuite {
/*
  val P: Parsers = ???
  import P._

  test("Character can be parsed") {
    val c = 'a'
    assert(P.run(P.char(c))(c.toString) == Right(c))
  }

  test("String can be parsed") {
    val s = "abba"
    assert(P.run(P.string(s))(s) == Right(s))
  }

  test("OR can be parsed") {
    val por = "abra" | "kadabra"
    assert(parse(por)("abra") == Right("abra"))
    assert(parse(por)("kadabra") == Right("kadabra"))
  }

  test("Repetitions can be recognized") {
    assert(parse(listOfN(3, "ab" | "cd"))("ababcd") == Right("ababcd"))
  }

  test("Many occurrences can be counted") {
    val pat = char('a').many.map(_.size)
    assert(parse(pat)("aaab") == Right(3))
    assert(parse(pat)("b") == Right(0))
  }

  test("N occurrences can be parsed") {
    val pat = listOfN(3, char('a'))
    assert(parse(pat)("aaab") == Right(List('a','a','a')))
    assert(parse(pat)("aab").isLeft)
  }

  test("Product can be parsed") {
    val pat = zeroOrMore(char('a')) ** oneOrMore(char('b'))
    assert(parse(pat)("aaabbc") == Right(3, 2))
    assert(parse(pat)("bbc") == Right(0, 2))
  }

  test("Context sensitive repetition can be parsed") {
    val pat = digits.map(i => listOfN(i, char('a')))
    assert(parse(pat)("3aaab") == Right(List('a','a','a')))
    assert(parse(pat)("3aab").isLeft)
  }

  test("JSON can be parsed") {
    val json = """{
                 |"Company name" : "Microsoft Corporation",
                 |"Ticker" : "MSFT",
                 |"Active" : true,
                 |"Price" : 30.66,
                 |"Shares outstanding" : 8.38e9,
                 |"Related companies" :
                 |[ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
                 |}""".stripMargin
    //val pat = JSON.jsonParser(P)
    val parsed: JSON = ???

  }
*/
}

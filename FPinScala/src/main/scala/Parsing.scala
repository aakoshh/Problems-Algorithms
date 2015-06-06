package fpinscala.parsing

import scala.util.matching.Regex
;

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def parse[A](p: Parser[A])(input: String) = run(p)(input)

  // Implicits should allow simpler syntax...
  implicit def char(a: Char): Parser[Char]
  implicit def string(s: String): Parser[String]

  // ... but we need this extra function that allows a 2 step conversion
  // from any type to a Parser[String] (using string above) then to rich parser.
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): RichParser[String] =
    f(a) // applying the other conversion implicitly


  def succeed[A](value: A): Parser[A]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def product[A, B](a: Parser[A], b: => Parser[B]): Parser[(A,B)] = {
    a.flatMap(x =>
      b.map(y =>
        (x, y)))
  }

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(x => succeed(f(x)))

  // Second argument pass by name so many can terminate when the first fails.
  def map2[A, B, C](a: Parser[A], b: => Parser[B])(f: (A,B) => C): Parser[C] =
    (a ** b).map { case (x, y) => f(x, y) }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  /** Recognize N repetitions of a pattern. */
  def listOfN[A](n: Int, a: Parser[A]): Parser[List[A]] = {
    if (n == 0) succeed(List())
    else        map2(a, listOfN(n-1, a))(_ :: _)
  }

  /** Collect the occurrences of a pattern at the beginning */
  def many[A](p: Parser[A]): Parser[List[A]] =
    many1(p) | succeed(List.empty[A])

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))((x, xs) => x :: xs)

  /** Count 0 or more occurrence of a character at the beginning. */
  def zeroOrMore[A](a: Parser[A]): Parser[Int] =
    a.many.slice.map(_.length)

  /** Count 1 or more occurrences of a character at the beginning. */
  def oneOrMore[A](a: Parser[A]): Parser[Int] =
    // Start with parsing at least one a, if that succeeds then parse
    (a ** a.many).map { case (_, lst) => 1 + lst.length }

  /** Return the part of the string that was matched by the parser. */
  def slice[A](p: Parser[A]): Parser[String]

  /** Regular expression parser. */
  implicit def regex(r: Regex): Parser[String]

  /** Recognize a number. */
  def digits: Parser[Int] =
    regex("\\d+".r).map(i => Integer.parseInt(i))

  implicit def parser2rich[A](p: Parser[A]): RichParser[A] =
    RichParser(p)

  /** Special operators */
  case class RichParser[A](a: Parser[A]) {
    /** Or with supertype of A */
    def |[B >: A](b: => Parser[B]): Parser[B] = self.or(a, b)

    def **[B](b: => Parser[B]): Parser[(A,B)] = self.product(a, b)

    def map[B](f: A => B) = self.map(a)(f)

    def flatMap[B](f: A => Parser[B]) = self.flatMap(a)(f)

    def many = self.many(a)

    def many1 = self.many1(a)

    def slice = self.slice(a)
  }



}


trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON


  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._

    val spaces = (char(' ') | string("\n")).many.slice
    val alphanum = regex("\\w*".r)

    val text: Parser[JString] = for {
      _ <- char('"')
      k <- alphanum
      _ <- char('"')
    } yield JString(k)

    val boolean: Parser[JBool] = {
      val t = string("true").map(_ => JBool(true))
      val f = string("false").map(_ => JBool(false))
      t | f
    }

    val number: Parser[JNumber] =
      regex("\\d+\\.?\\d*|\\.\\d+".r).map(s => JNumber(java.lang.Double.parseDouble(s)))

    val nil: Parser[JSON] =
      string("null").map(_ => JNull)

    lazy val value: Parser[JSON] =
      text | boolean | number | nil | array | obj

    lazy val item: Parser[JSON] = for {
      _ <- spaces
      v <- value
      _ <- spaces
      _ <- char(',') | succeed("")
    } yield v

    lazy val array: Parser[JArray] = for {
      _ <- spaces
      _ <- char('[')
      items <- value.many
      _ <- char(']')
      _ <- spaces
    } yield JArray(items.toIndexedSeq)

    lazy val property: Parser[(JString, JSON)] = for {
      _ <- spaces
      k <- text
      _ <- spaces
      _ <- char(':')
      v <- item
    } yield (k, v)

    lazy val obj: Parser[JObject] = for {
      _ <- char('{')
      ps <- property.many1
      _ <- char('}')
    } yield JObject(ps.map(kv => (kv._1.get, kv._2)).toMap)

    obj | array
  }
}

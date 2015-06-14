package fpinscala.monads

import org.scalacheck._, Prop.forAll, Arbitrary._
import scala.reflect.ClassTag

object MonadProperties extends Properties("Monad") {

  property(s"sequence of options") = forAll { (as: List[Option[Int]]) =>
    Monad.optMonad.sequence(as) == (if (as.contains(None)) None else (Some(as.map(_.get))))
  }
}
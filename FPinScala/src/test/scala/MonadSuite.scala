package fpinscala.monads

import org.scalacheck._, Prop.forAll, Arbitrary._
import scala.reflect.ClassTag

object MonadProperties extends Properties("Monad") {

  def monadLaws[F[_], A](name: String, m: Monad[F])(implicit ev: Arbitrary[F[A]]) = {
    property(s"$name map") = forAll { (ma: F[A]) =>
      m.map(ma)(a => a) == ma
    }
  }

  monadLaws[List, Int]("listMonad", Monad.listMonad)
  monadLaws[Option, String]("optMonad", Monad.optMonad)

  property(s"sequence of options") = forAll { (as: List[Option[Int]]) =>
    Monad.optMonad.sequence(as) == (if (as.contains(None)) None else (Some(as.map(_.get))))
  }
}
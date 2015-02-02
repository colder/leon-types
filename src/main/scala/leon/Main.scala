package leon;
//import annotation.unchecked.uncheckedVariance
import scala.language.higherKinds


object Main {

  abstract class Pure
  abstract class XLang extends Pure

  abstract class Tree[-E <: Pure : Manifest]  {
    val manifest = implicitly[Manifest[E]]
  }

  trait Literal[-E <: Pure] extends Tree[E] {
  }

  case class Plus[-E <: Pure : Manifest](l: Tree[E], r: Tree[E]) extends Tree[E]
  case class Times[-E <: Pure : Manifest](l: Tree[E], r: Tree[E]) extends Tree[E]
  case class Lit(v: Int) extends Literal[Pure]

  case class Effect2[-E <: XLang : Manifest](a: Tree[E], b: Tree[E]) extends Tree[E] with Extractable2[E, XLang] {
    def extract[U <: XLang : Manifest] = {
      (a, b, Effect2.apply[U] _)
    }
  }

  class Bound[T <: Pure]

  def main(args: Array[String]) {
    val t1 = Plus(Lit(42), Lit(1))
    val t2 = Times(Effect2(Lit(42), Lit(27)), Lit(2))

    println("Sanitize t1: ")
    sanitize(t1)
    println("Sanitize t2: ")
    sanitize(t2)

    implicit val b = new Bound[Pure]
    Extractor2.unapply[XLang, Pure](Effect2(Lit(42), Lit(27)))
  }

  def sanitize(t: Tree[_]): Tree[Pure] = {
    implicit val b = new Bound[Pure]

    t match {
      case Extractor2(a, b, cc) => cc(sanitize(a), sanitize(b))
      case l: Tree[_] if l.manifest <:< manifest[Pure] => l
    }
  }

  trait Extractable2[-T <: Pure, -B <: Pure] {
    def extract[U <: B : Manifest]: (Tree[T], Tree[T], (Tree[U], Tree[U]) => Tree[U])
  }

  object Extractor2 {
    def unapply[T <: Pure : Manifest, U <: Pure : Bound : Manifest](expr: Tree[T]) : Option[(Tree[T], Tree[T], (Tree[U], Tree[U]) => Tree[U])] = expr match {
      case Plus(a,b) => Some((a, b, Plus.apply[U] _))
      case Times(a,b) => Some((a, b, Times.apply[U] _))
      case e2: Extractable2[T, U] => Some(e2.extract)
      case _ => None
    }
  }

}

import scala.compiletime.erasedValue

// make a joke which is also an intro to scala 3 implicit
// in scala 2, you would write implicit in different places
// and it had a different meaning: parameter, method, class, conversion
// in scala 3 there are instead givens, derives, extension
// unfortunately scala 3 introduced another keyword which has a meaning
// depending on location: inline
// First, explanation about inline
// enables us to inline code (actually ASTs). can be applied to parameters,
// method, match, ifs, and more.
inline def length[T]: Int =
  inline erasedValue[T] match
    case _: (head *: tail) => 1 + length[tail]
    case _: EmptyTuple => 0

import scala.compiletime.summonInline

trait Show[T]:
  def show(value: T): String

inline def prints[T](value: T): String =
  summonInline[Show[T]].show(value)

// the follwing is equal to prints, without inline:

def prints2[T](value: T)(using ev: Show[T]): String =
  ev.show(value)

// providing the instance
given stringShow: Show[String] with
  def show(value: String): String = s"showing $value"

// why write code when the compiler can do it for us?
// case class MyCaseClass(id: Int, bla: Option[String]) derives Equals, MagicTypeClass
// would generate the following code in the companion
// object MyAwesomeClass:
//   given $e = Equals.derives[MyCaseClass]
//   given $s = MagicTypeClass.derives[MyCaseClass]

// import scala.deriving._, the compiler gives us
// the structure of the case class in a way that's simple to access
// and consistent
// let's see an example
import scala.deriving._
import scala.compiletime.summonFrom
import scala.compiletime.{error, constValue}
import scala.deriving.Mirror.Product

// inline def fields[T](): List[String] =
//   summonFrom {
//     case m: Mirror.ProductOf[T] =>
//       collectTypes[m.MirroredElemLabels]()
//     case _ =>
//       error("failed to collect types")
//   }

// inline def collectTypes[T](): List[String] =
//   erasedValue[T] match
//     case _: (head *: tail) => constValue[String] :: collectTypes[tail]()
//     case EmptyTuple => Nil


@main def hello: Unit =
  println(length[("a", 1, "there")])

  println(prints("hey there"))

  println(prints2("hey there 2"))

  case class IceCream(price: Double, n: Int)

  // fields[IceCream]()


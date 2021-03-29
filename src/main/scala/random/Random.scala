package random

import scala.deriving._
import scala.compiletime.{erasedValue, summonInline}

/**
 * outline:
 * type class derivation in general
 * scala 3 - we don't need libraries like scala 2
 * working at the type level vs the value level
 * generating a random value - what needs to be done for products vs enums
 * the Random trait
 * the derived function
 * the summonAll function
 * the randomProduct function
 * the randomSum function
 * listToTuple
 * Examples - defining givens, summon
 * derivation libraries for scala 3
 * 
 * publish this jar and try to use from a 2.12 project
 */

// first we define the type class
trait Random[A]:
  def generate(): A

// this function works at the type level - summoning type classes for a type
inline def summonAll[A <: Tuple]: List[Random[_]] =
   inline erasedValue[A] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[Random[t]] :: summonAll[ts]

// this function is at the value level
def toTuple(xs: List[_], acc: Tuple): Tuple =
  xs match
    case Nil => acc
    case (h :: t) => h *: toTuple(t, acc)

object Random:

  inline given derived[A](using m: Mirror.Of[A]): Random[A] =
    lazy val instances = summonAll[m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[A]     => randomSum(s, instances)
      case p: Mirror.ProductOf[A] => randomProduct(p, instances)

  // receives a list of random instances for the branches of an enum and chooses one at random
  private def randomSum[A](s: Mirror.SumOf[A], instances: => List[Random[_]]): Random[A] =
    new Random[A]:
      def generate(): A =
        instances(scala.util.Random.nextInt(instances.size))
          .asInstanceOf[Random[A]]
          .generate()

  // receives a list of random instances for the parameters of a case class and instantiates it
  private def randomProduct[A](p: Mirror.ProductOf[A], instances: => List[Random[_]]): Random[A] =
    new Random[A]:
      def generate(): A =
        p.fromProduct(toTuple(instances.map(_.generate()), EmptyTuple))

end Random
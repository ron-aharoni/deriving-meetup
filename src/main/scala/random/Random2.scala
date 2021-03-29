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
def listToTuple[A](xs: List[A], acc: Tuple): Tuple =
  xs match
    case Nil => acc
    case (h :: t) => h *: listToTuple(t, acc)

object Random:

  // receives a list of random instances for the branches of an enum and chooses one at random
  inline def randomSum[A](s: Mirror.SumOf[A], instances: => List[Random[_]]): Random[A] =
    new Random[A]:
      def generate(): A =
        val instances = summonAll[s.MirroredElemTypes]
        val idx = scala.util.Random.nextInt(instances.size)
        instances(idx).asInstanceOf[Random[A]].generate()

  // receives a list of random instances for the parameters of a case class and instantiates it
  private def randomProduct[A](
    p: Mirror.ProductOf[A],
    instances: => List[Random[_]]
  ): Random[A] =
    new Random[A]:
      def generate(): A =
        val tt = listToTuple(instances.map(_.generate()), EmptyTuple)
        p.fromProduct(tt)

  inline given derived[A](using m: Mirror.Of[A]): Random[A] =
    val instances = summonAll[m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[A]     => randomSum(s, instances)
      case p: Mirror.ProductOf[A] => randomProduct(p, instances)

end Random

@main def test(): Unit =

   case class IceCream(n: Int, s: String)
 
   println(listToTuple(List(1, "abc", 'd'), EmptyTuple))

   given randStr: Random[String] with
     def generate(): String = scala.util.Random.nextString(10)

   given randInt: Random[Int] with
     def generate(): Int = scala.util.Random.nextInt()

   given randDouble: Random[Double] with
     def generate(): Double = scala.util.Random.nextDouble()

   1.to(3).foreach { _ =>
     println(summon[Random[IceCream]].generate())
   }

   enum A derives Random:
     case B(a: String, b: Int)
     case C(c: Double)

   1.to(3).foreach { _ =>
     println(summon[Random[A]].generate())
   }
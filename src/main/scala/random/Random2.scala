package random

import scala.deriving._
import scala.compiletime.{erasedValue, summonInline}

inline def summonAll[A <: Tuple]: List[Random[_]] =
   inline erasedValue[A] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[Random[t]] :: summonAll[ts]

def generateProduct(instances: List[Random[_]], acc: Tuple): Tuple =
   instances match
      case Nil => acc
      case h :: t => h.generate() *: generateProduct(t, acc)

inline def sizeT[A <: Tuple]: Int =
   inline erasedValue[A] match
      case _: EmptyTuple => 0
      case _: (t *: ts) => 1 + sizeT[ts]

trait Random[A]:
   def generate(): A

object Random:

   inline def randomSum[A](s: Mirror.SumOf[A], instances: => List[Random[_]]): Random[A] =
      new Random[A]:
         def generate(): A =
            val nSubtyes = sizeT[s.MirroredElemTypes]
            val idx = scala.util.Random.nextInt(nSubtyes)
            instances(idx).asInstanceOf[Random[A]].generate()

   private def randomProduct[A](
     p: Mirror.ProductOf[A],
     instances: => List[Random[_]]
     ): Random[A] =
      new Random[A]:
         def generate(): A =
            p.fromProduct(generateProduct(instances, EmptyTuple))

   inline given derived[A](using m: Mirror.Of[A]): Random[A] =
      lazy val instances = summonAll[m.MirroredElemTypes]
      inline m match
         case s: Mirror.SumOf[A]     => randomSum(s, instances)
         case p: Mirror.ProductOf[A] => randomProduct(p, instances)
   
end Random
@main def test(): Unit =

   case class IceCream(n: Int, s: String)

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
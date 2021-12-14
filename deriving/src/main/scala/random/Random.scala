package random

import scala.deriving._
import scala.compiletime.{erasedValue, summonInline}

trait Random[A]:
  def generate(): A

inline def summonAll[A <: Tuple]: List[Random[_]] =
   inline erasedValue[A] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[Random[t]] :: summonAll[ts]

def toTuple(xs: List[_], acc: Tuple): Tuple =
  xs match
    case Nil => acc
    case (h :: t) => h *: toTuple(t, acc)

object Random:

  inline given derived[A](using m: Mirror.Of[A]): Random[A] =
    lazy val instances = summonAll[m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[A]     => deriveSum(s, instances)
      case p: Mirror.ProductOf[A] => deriveProduct(p, instances)

  private def deriveSum[A](s: Mirror.SumOf[A], instances: => List[Random[_]]): Random[A] =
    new Random[A]:
      def generate(): A =
        instances(scala.util.Random.nextInt(instances.size))
          .asInstanceOf[Random[A]]
          .generate()

  private def deriveProduct[A](p: Mirror.ProductOf[A], instances: => List[Random[_]]): Random[A] =
    new Random[A]:
      def generate(): A =
        p.fromProduct(toTuple(instances.map(_.generate()), EmptyTuple))

end Random
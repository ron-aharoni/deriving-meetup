import scala.deriving._
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline, constValue}

type TC[A] = Random[A]

inline def summonAll[A <: Tuple]: List[TC[_]] =
   inline erasedValue[A] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[TC[t]] :: summonAll[ts]

inline def sizeT[A <: Tuple]: Int =
   inline erasedValue[A] match
      case _: EmptyTuple => 0
      case _: (t *: ts) => 1 + sizeT[ts]

trait Random[A]:
   def generate(): A

object Random:
   private def iterator[A](p: A) = p.asInstanceOf[Product].productIterator

   private def randomSum[A](s: Mirror.SumOf[A], instances: => List[TC[_]]): TC[A] =
      new TC[A]:
         def generate(): A =
            //val ss = sizeT[s.MirroredElemTypes]
            //val idx = scala.util.Random.nextInt(ss)
            //val typ = s.MirroredElemTypes(idx)
            ???


   private def randomProduct[A](p: Mirror.ProductOf[A], instances: => List[TC[_]]): TC[A] =
      new TC[A]:
         def generate(): A =
            val ps = instances.map(i => i.generate())
            // p.fromProduct(ps)
            ???

   inline given derived[A](using m: Mirror.Of[A]): TC[A] =
      lazy val elemInstances = summonAll[m.MirroredElemTypes]
      inline m match
         case s: Mirror.SumOf[A]     => randomSum(s, elemInstances)
         case p: Mirror.ProductOf[A] => randomProduct(p, elemInstances)

end Random

// number of args in a case class
inline def caseClassSize[A](using m: Mirror.Of[A]): Int =
   sizeT[m.MirroredElemTypes]


@main def test(): Unit =
   // val eqoi = summon[Eq[Opt[Int]]]
   // assert(!eqoi.eqv(Sm(23), Nn))

   println("hello")

   case class IceCream(n: Int, s: String)

   println(caseClassSize[IceCream])

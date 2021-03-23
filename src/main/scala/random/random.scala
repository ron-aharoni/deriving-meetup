import scala.deriving._
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline, constValue}

type TC[A] = Random[A]

inline def summonAll[A <: Tuple]: List[TC[_]] =
   inline erasedValue[A] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[TC[t]] :: summonAll[ts]

def collectTypeclassProduct(tcs: List[Random[_]], col: Tuple): Tuple =
   tcs match
      case Nil => col
      case h :: t => h.generate() *: collectTypeclassProduct(t, col)

inline def sizeT[A <: Tuple]: Int =
   inline erasedValue[A] match
      case _: EmptyTuple => 0
      case _: (t *: ts) => 1 + sizeT[ts]

trait Random[A]:
   def generate(): A

object Random:
   private def iterator[A](p: A) = p.asInstanceOf[Product].productIterator

   inline def randomSum[A](s: Mirror.SumOf[A], instances: => List[TC[_]]): TC[A] =
      new TC[A]:
         def generate(): A =
            val i = sizeT[s.MirroredElemTypes]
            val idx = scala.util.Random.nextInt(i)
            instances(idx).generate().asInstanceOf[A]

   private def randomProduct[A](p: Mirror.ProductOf[A], instances: => List[TC[_]]): TC[A] =
      new TC[A]:
         def generate(): A =
            val ps = collectTypeclassProduct(instances, EmptyTuple)
            p.fromProduct(ps)

   inline given derived[A](using m: Mirror.Of[A]): TC[A] =
      lazy val elemInstances = summonAll[m.MirroredElemTypes]
      inline m match
         case s: Mirror.SumOf[A]     => randomSum(s, elemInstances)
         case p: Mirror.ProductOf[A] => randomProduct(p, elemInstances)

end Random

// number of args in a case class
inline def caseClassSize[A](using m: Mirror.Of[A]): Int =
   sizeT[m.MirroredElemTypes]


def rndGen[A](using rnd: Random[A]): A =
   rnd.generate()


@main def test(): Unit =
   // val eqoi = summon[Eq[Opt[Int]]]
   // assert(!eqoi.eqv(Sm(23), Nn))

   println("hello")

   case class IceCream(n: Int, s: String)

   println(caseClassSize[IceCream])

   given randStr: Random[String] with
     def generate(): String = scala.util.Random.nextString(10)

   given randInt: Random[Int] with
     def generate(): Int = scala.util.Random.nextInt()

   println(rndGen[String])
   println(rndGen[Int])

   println(rndGen[IceCream])
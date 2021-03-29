package random

import org.junit.Test
import org.junit.Assert._

given randStr: Random[String] with
  def generate(): String = scala.util.Random.nextString(10)

given randInt: Random[Int] with
  def generate(): Int = scala.util.Random.nextInt()

case class MyCaseClass(n: Int, s: String)

enum MyEnum:
  case E1(a: String, b: Int)
  case E2

sealed trait MySealedTrait
case object S1 extends MySealedTrait
case class S2(a: String, b: Int) extends MySealedTrait

class RandomTest {
  
  @Test def testPrimitive(): Unit = {
    summon[Random[String]].generate()
    summon[Random[Int]].generate()
  }

  @Test def testCaseClass(): Unit = {
    summon[Random[MyCaseClass]].generate()
  }

  @Test def testEnum(): Unit = {
    summon[Random[MyEnum]].generate()
  }
  
  @Test def testSealedTrait(): Unit = {
    summon[Random[MySealedTrait]].generate()
  }
  
}

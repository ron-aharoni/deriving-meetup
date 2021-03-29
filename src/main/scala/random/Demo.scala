package random

case class MyCaseClass(n: Int, s: String)

enum MyEnum:
  case E1(a: String, b: Int)
  case E2

sealed trait MySealedTrait
case object S1 extends MySealedTrait
case class S2(a: String, b: Int) extends MySealedTrait

given randStr: Random[String] with
  def generate(): String = scala.util.Random.nextString(5)

given randInt: Random[Int] with
  def generate(): Int = scala.util.Random.nextInt()

@main def randomDemo(): Unit =
  println(s"case class:\t\t${summon[Random[MyCaseClass]].generate()}")
  println(s"enum:\t\t\t${summon[Random[MyEnum]].generate()}")
  println(s"sealed trait:\t${summon[Random[MySealedTrait]].generate()}")
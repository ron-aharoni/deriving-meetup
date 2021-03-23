
import org.junit.Test
import org.junit.Assert._

class Test1 {
  @Test def t1(): Unit = {
    case class IceCream(n: Int, s: String)
    given randStr: Random[String] with
      def generate(): String = scala.util.Random.nextString(10)

    given randInt: Random[Int] with
      def generate(): Int = scala.util.Random.nextInt()

    summon[Random[String]].generate()
    summon[Random[Int]].generate()
    summon[Random[IceCream]].generate()

    assertEquals(true, true) // just assert compiles
  }
}

package random

import org.junit.Test
import org.junit.Assert._

class RandomTest {

  @Test def testPrimitive(): Unit = {
    summon[Random[String]].generate()
    summon[Random[Int]].generate()
  }

  @Test def testCaseClass(): Unit = {
    summon[Random[SiteMember]].generate()
  }
 
}
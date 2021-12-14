package random

import org.junit.Test
import org.junit.Assert._

class RandomTest {

  @Test def testDerivation(): Unit = {
    summon[Random[SiteMember]].generate()
  }
 
}
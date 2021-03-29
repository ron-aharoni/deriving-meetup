package random

import org.junit.Test
import org.junit.Assert._

class SizedTest {
  
  @Test def testCaseClassSize(): Unit = {
    case class MyCaseClass(n: Int, s: String)

    assertEquals(caseClassSize[MyCaseClass], 2)
  }

}

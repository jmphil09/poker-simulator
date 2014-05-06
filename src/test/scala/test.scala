import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit._
import org.scalatest.mock._
import org.mockito.Mockito._
 
class HelloTest extends AssertionsForJUnit with MockitoSugar {
 
  @Test
  def verifySomethingTest() {
    val three = 3
    assert(3 === three)
  }
 
  @Test
  def mockTest() {
    val mockTurtle = mock[Turtle]
    when(mockTurtle.walk(1)).thenReturn(3L)
    assert(3L === mockTurtle.walk(1))
    verify(mockTurtle).walk(1)
  }
 
  @Test(expected = classOf[IllegalArgumentException])
  def exceptionTest() {
    Integer.parseInt("one")
  }
}
 
trait Turtle {
  def walk(i: Int): Long
}
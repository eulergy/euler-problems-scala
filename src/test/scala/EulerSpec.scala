import org.specs._
import eulergy.NumberHelpers._
import scala.math._

class EulerSpec extends Specification {

  "Euler Problem Number 6" should {
    "Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum" in {
		val sumOfSquares = Iterator.range(1, 101).collect {
			case i => pow(i, 2)
		}.sum
		val squareOfSums = pow(Iterator.range(1, 101).sum, 2)
		
		val result = squareOfSums - sumOfSquares
		println("Result: %s".format(result))
		result must_== 232792560
    }
  }

}

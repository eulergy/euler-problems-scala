import org.specs._
import eulergy.NumberHelpers._

class EulerSpec extends Specification {

  "Euler Problem Number 5" should {
    "What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20" in {
		val divisors = (1 to 20)
		val result = Iterator.from(20).dropWhile {
			case i => !divisors.forall( _ isDivisorOf i )
		}.next
		
		println("Result: %s".format(result))
		result must_== 232792560
    }
  }

}

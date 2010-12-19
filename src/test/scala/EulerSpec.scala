import org.specs._
import eulergy.NumberHelpers._

class EulerSpec extends Specification {

  "Euler Problem Number 4" should {
    "Find the largest palindrome made from the product of two 3-digit numbers" in {
		val highestProduct = Iterator.iterate(999L, 999L) {
			case (100, i) => (i-1, i-1)
			case (i, j) => (i-1, j)
		} collect {
			case (i, j) => (i, j, i*j)
		} collect {
			case (i, j, product) if (product isPalindrome) => (i, j, product)
		} takeWhile {
			case (i, j, product) => j > 100
		} reduceLeft {
			(a,b) => if(a._3 > b._3) a else b
		}
		
		val (i, j, result) = highestProduct
		println("Result: %s = %s * %s".format(result, i, j))
		result must_!= 698896
    }
  }

}

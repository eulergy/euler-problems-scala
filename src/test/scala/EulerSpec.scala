import org.specs._
import eulergy.NumberHelpers._

class EulerSpec extends Specification {

  "Euler Problem Number 4" should {
    "Find the largest palindrome made from the product of two 3-digit numbers" in {
		val highestProduct = Iterator.iterate( Seq(999L, 999L) ) {
			case Seq(100, i) => Seq(i-1, i-1)
			case Seq(i, j) => Seq(i-1, j)		
		} filter(_.product isPalindrome) takeWhile {
			case Seq(i, j) => j > 100
		} reduceLeft {
			(a,b) => if(a.product > b.product) a else b
		}
		
		println("Result: %s = %s * %s".format(highestProduct.product, highestProduct(0), highestProduct(1)))
		highestProduct.product must_!= 906609
    }
  }

}

package eulergy

import org.specs._
import eulergy.NumberHelpers._

class NumberHelpersSpec extends Specification {
	"Number helpers" should {
		"help me with divisors" in {
			(5 isDivisorOf 10) must_== true
			(10 isDivisorOf 5) must_== false
			(3 isDivisorOf 9) must_== true
		}
		"help me with multiples" in {
			(5 isMultipleOf 10) must_== false
			(10 isMultipleOf 5) must_== true
			(9 isMultipleOf 3) must_== true
		}
		"help me with palindromic numbers" in {
			(9009 isPalindromic) must_== true
			(9008 isPalindromic) must_== false
			(1234554321 isPalindrome) must_== true
		}
  	}
}

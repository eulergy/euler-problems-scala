package eulergy

import org.specs._
import eulergy._
import NumberHelpers._
import Streams._

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

	"Streams" should {
		"identity is the infinite sequence of 1's" in {
			Streams.identity.head must_== 1
			Streams.identity.hasDefiniteSize must_== false
			Streams.identity take 10 forall( i => i==1) must_== true
		}
		"naturalNumbers is the infinite sequence of natural numbers" in {
			Streams.naturalNumbers.head must_== 1
			Streams.naturalNumbers.tail.head must_== 2
			Streams.naturalNumbers.hasDefiniteSize must_== false
			Streams.naturalNumbers.take(10).toSet mustEqual(Set(1,2,3,4,5,6,7,8,9,10))
		}
		"primes should be the Stream of prime numbers" in {
			Streams.primes.take(6).toList mustEqual(List(2, 3, 5, 7, 11, 13))
		}
	}
}

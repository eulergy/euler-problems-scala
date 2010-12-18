import org.specs._

class EulerSpec extends Specification {
  
  "Euler Problem Number 3" should {
    "What is the largest prime factor of the number 600851475143" in {
		val iter = Iterator.iterate(600851475143L,2) {
			case (reduction, i) if (i isDivisorOf reduction) => (reduction / i, i+1)
			case (reduction, i) => (reduction, i+1)
		} dropWhile {
			case (reduction, i) => (reduction / i) > 1
		}
		val (result, other) = iter.next
		println("Result: %s".format(result))
		result must_== 6857
    }
  }

  def primes = Iterator.iterate(1, 2) { 
	case (i, j) => (j, i + j) 
  }

  "Number helpers" should {
	"help me with divisors" in {
		(5 isDivisorOf 10) must_== true
		(10 isDivisorOf 5) must_== false
		(3 isDivisorOf 9) must_== true
	}
	"help me with multipls" in {
		(5 isMultipleOf 10) must_== false
		(10 isMultipleOf 5) must_== true
		(9 isMultipleOf 3) must_== true
	}	
  }

  implicit def numberHelper(i: Long): NumberHelpers = new NumberHelpers(i)
  
  class NumberHelpers(i: Long) {
  	def isDivisorOf(toCheck: Long): Boolean = (toCheck % i) == 0
	def isMultipleOf(multiple: Long): Boolean  = (i % multiple) == 0
  }

}

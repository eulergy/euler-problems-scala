import org.specs._
import eulergy._
import NumberHelpers._
import Streams._
import scala.math._

class EulerSpec extends Specification {

  "Euler Problem Number 7" should {
    "What is the 10001st prime number" in {
		
		lazy val primes: Stream[Int] = 
			Stream.cons(2, Streams.naturalNumbers.tail filterNot {
				case 2 => true
				case i => 
						val potentialPrimeFactors = primes takeWhile {
							case j => j*j <= i
						}						
						potentialPrimeFactors exists(_ isDivisorOf i)
			})
		primes.take(10001).last must_== 104743
    }
  }

}

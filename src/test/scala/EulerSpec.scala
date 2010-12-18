import org.specs._

class EulerSpec extends Specification {
  
  "Euler Problem Number 3" should {
    "What is the largest prime factor of the number 600851475143" in {
    }
  }

  def primes = Iterator.iterate(1, 2) { 
	case (i, j) => (j, i + j) 
  }

}

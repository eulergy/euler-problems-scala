import org.specs._

class EulerSpec extends Specification {
  
  "Euler Problem Number 3" should {
    "What is the largest prime factor of the number 600851475143" in {
    }
  }

  def fib_iter = Iterator.iterate(1, 2){ i => (i._2, i._1 + i._2) }

}

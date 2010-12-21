import org.specs._

class EulerSpec extends Specification {
  
  "Euler Problem Number 2" should {
    "Find the sum of all the even-valued terms in the Fibonacci sequence which do not exceed four million" in {
		
		val fourMillion = 4000000
		
		val result = fib_iter.collect{  
			case (a,b) => a
		}.filter( _ % 2 == 0 )
		.takeWhile( _ < fourMillion )
		.foldLeft(0) {
			case (acc, i) => acc + i
		}
		println("Result %s".format(result))
		result must_== 4613732
    }
  }

  def fib_iter = Iterator.iterate(1, 2){ i => (i._2, i._1 + i._2) }

}

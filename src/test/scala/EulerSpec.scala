import org.specs._

class EulerSpec extends Specification {
  
  "Euler Problem Number 1" should {
    "Find the sum of all the multiples of 3 or 5 below 1000." in {
		import Problem1._
		
		val range = (1 until 1000)
		val result = range.foldLeft(0) {
			case (accum, i) if isMultipleOfThree(i) => accum + i
			case (accum, i) if isMultipleOfFive(i) => accum + i
			case (accum, i) => accum
		}
		println("Result is: %s".format(result))
		result must_== 233168
    }
  }

  "Problem1" should {
	import Problem1._
	"give multiple of 3" in {
		isMultipleOfThree(3) mustEq true
		isMultipleOfThree(6) mustEq true
		isMultipleOfThree(2) mustEq false
	}
	"give multiple of 5" in {
		isMultipleOfFive(5) mustEq true
		isMultipleOfThree(15) mustEq true
		isMultipleOfThree(4) mustEq false
	}	
  }

  object Problem1 {
	def isMultipleOfThree = isMultipleOf(3) _
	def isMultipleOfFive = isMultipleOf(5) _
	def isMultipleOf(multiple:Int)(i : Int) = (i % multiple) == 0
  }
  
}

package eulergy {

class NumberHelpers(i: Long) {
	def isDivisorOf(toCheck: Long): Boolean = (toCheck % i) == 0
	def isMultipleOf(multiple: Long): Boolean  = (i % multiple) == 0
	def isPalindrome = StringHelper.isPalindrome(i.toString.toList)
	def isPalindromic = StringHelper.isPalindrome(i.toString.toList)
}

object NumberHelpers {
	implicit def numberHelper(i: Long): NumberHelpers = NumberHelpers(i)	
	
	def apply(i: Long) = new NumberHelpers(i)
}

object StringHelper {
	def isPalindrome(c: List[Char]): Boolean = c match {              
 		case Nil => false                                                  
 		case x :: Nil => true                                              
 		case x :: y :: Nil if x == y => true                               
 		case x :: y if x == y.reverse.head => isPalindrome(y.reverse.tail)
 		case _ => false                                                    
 	}
}

object Streams {
	import NumberHelpers._
	
	lazy val identity: Stream[Int] = Stream.iterate(1)(i => 1)
	lazy val naturalNumbers: Stream[Int] = Stream.iterate(1)(i => i+1)
	lazy val primes: Stream[Int] = 
		Stream.cons(2, Streams.naturalNumbers.tail filterNot {
			case 2 => true
			case i => 
					val potentialPrimeFactors = primes takeWhile {
						case j => j*j <= i
					}						
					potentialPrimeFactors exists(_ isDivisorOf i)
		})	
}
	
}
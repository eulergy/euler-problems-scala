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
	
}
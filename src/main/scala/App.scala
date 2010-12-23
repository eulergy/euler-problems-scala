import eulergy.Streams.primes

object App {
  def main(args: Array[String]) {
//    primes.takeWhile( i => )
      primes.takeWhile( p => true ).foreach { p => println(p) }
  }

}
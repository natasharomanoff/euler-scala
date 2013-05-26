
package problem003

/* 
 * Largest prime factor
 * 
 * The prime factors of 13195 are 5, 7, 13 and 29.
 * 
 * What is the largest prime factor of the number 600851475143 ? 
 * 
 */

object Euler003 {
  
  def isPrime(n: Long): Boolean = {
    val limit = math.sqrt(n).toLong + 1
    List.range(2L, limit).forall(x => n % x != 0)
  }

  def largestDivisor(n: Long): Option[Long] = {
    Stream.range(2L, n).find(p => n % p == 0) match {
      case Some(d) => Some(n / d)
      case None => None
    }
  }

  def largestPrimeFactor(n: Long): Long = {
    largestDivisor(n) match {
      case None => n
      case Some(d) => if (isPrime(d)) d else largestPrimeFactor(d)
    }
  }

  def main(args: Array[String]): Unit = { println("Result is " + largestPrimeFactor(600851475143L) ) }

}
	
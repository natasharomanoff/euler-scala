
package problem005

/* 
 * Smallest multiple
 * 
 * 2520 is the smallest number that can be divided by 
 * each of the numbers from 1 to 10 without any remainder.
 * 
 * What is the smallest positive number that is evenly divisible 
 * by all of the numbers from 1 to 20? 
 */

object Euler005 {

  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long = (a * b) / gcd(a, b)

  def minEvenlyDivisible(divisors: List[Long]) = divisors.foldLeft(1L)(lcm(_, _))
 
  def solution: Long = minEvenlyDivisible((1L to 20L).toList)

  def main(args: Array[String]): Unit = { println("Result is " + solution ) }
  

}
	
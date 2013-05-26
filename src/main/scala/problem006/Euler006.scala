
package problem006

/* 
 * Sum square difference
 * 
 * The sum of the squares of the first ten natural numbers is,
 * 1^2 + 2^2 + ... + 10^2 = 385
 * 
 * The square of the sum of the first ten natural numbers is,
 * (1 + 2 + ... + 10)^2 = 552 = 3025
 * 
 * Hence the difference between the sum of the squares of the first 
 * ten natural numbers and the square of the sum is 3025 - 385 = 2640.
 * Find the difference between the sum of the squares of the first 
 * one hundred natural numbers and the square of the sum. 
 * 
 */

object Euler006 {
  
  def sqr(n: Int): Int = n * n

  def sumSquareDiff(n: Int): Int = {
    val r = List.range(1, n + 1)
    sqr(r.foldLeft(0)(_ + _)) - r.foldLeft(0)(_ + sqr(_))
  }
  

  def main(args: Array[String]): Unit = { println("Result is " + sumSquareDiff(100) ) }

}
	
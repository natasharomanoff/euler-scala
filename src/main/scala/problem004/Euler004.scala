
package problem004

/* 
 * Largest palindrome product
 * 
 * A palindromic number reads the same both ways. 
 * 
 * The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 x 99.
 * 
 * Find the largest palindrome made from the product of two 3-digit numbers. 
 */

object Euler004 {

  def reverse(n: Int): Int = {
    def revh(n: Int, acc: Int): Int = if (n == 0) acc else revh(n / 10, (10 * acc) + (n % 10))
    revh(n, 0)
  }

  def isPalindrome(n: Int): Boolean = n == reverse(n)

  def largestPalindrome(digits: Int): Int = {
    def ubound(digits: Int): Int = (math.pow(10, digits) - 1).toInt
    def lbound(digits: Int): Int = math.pow(10, digits - 1).toInt

    (for {
      i <- Stream.range(ubound(digits), lbound(digits), -1)
      j <- Stream.range(ubound(digits), lbound(digits), -1)
      if (isPalindrome(i * j))
    } yield (i * j)).max
  }

  def main(args: Array[String]): Unit = { println("Result is " + largestPalindrome(3)) }

}
	

package problem149

/* 
 * Searching for a maximum-sum subsequence.
 * 
 * Looking at the table below, it is easy to verify that the maximum possible sum of adjacent numbers in any direction (horizontal, vertical, diagonal or anti-diagonal) is 16 (= 8 + 7 + 1).
 * 253296513273184  8
 * Now, let us repeat the search, but on a much larger scale:
 * First, generate four million pseudo-random numbers using a specific form of what is known as a "Lagged Fibonacci Generator":
 * For 1 k  55, sk = [100003  200003k + 300007k3] (modulo 1000000)  500000.
 * For 56 k  4000000, sk = [sk24 + sk55 + 1000000] (modulo 1000000)  500000.
 * Thus, s10 = 393027 and s100 = 86613.
 * The terms of s are then arranged in a 20002000 table, using the first 2000 numbers to fill the first row (sequentially), the next 2000 numbers to fill the second row, and so on.
 * Finally, find the greatest sum of (any number of) adjacent entries in any direction (horizontal, vertical, diagonal or anti-diagonal). 
 */

object Euler149 {

  def main(args: Array[String]): Unit = { println("Result is ") }

}
	
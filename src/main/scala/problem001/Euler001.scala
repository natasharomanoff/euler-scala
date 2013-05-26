
package problem001

/* 
 * Multiples of 3 and 5
 * 
 * If we list all the natural numbers below 10 
 * that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
 * The sum of these multiples is 23.
 * 
 * Find the sum of all the multiples of 3 or 5 below 1000. 
 */

object Euler001 {
  
  def multiplesOf(args: Int*)(below: Int)(): List[Int] = (1 until below).toList.filter { e =>
    args.exists(x => (e % x == 0))
  }

  def main(args: Array[String]): Unit = { println("Result is " + multiplesOf(3, 5)(1000).sum ) }

}
	
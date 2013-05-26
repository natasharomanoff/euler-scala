
package problem020

/* 
 * Factorial digit sum
 * 
 * n! means n  (n  1)  ...  3  2  1
 * For example, 10! = 10  9  ...  3  2  1 = 3628800,and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
 * Find the sum of the digits in the number 100! 
 */


object Euler020 {
  
  def factorial(n: BigInt): BigInt = {
    def fact(n: BigInt, acc: BigInt): BigInt = if (n <= 1) acc else fact(n - 1, n * acc)
    fact(n, 1)
  }

  def sumOfDigits(n: BigInt): BigInt = {
    def sum(n: BigInt, acc: BigInt): BigInt = if (n < 10) acc + n else sum(n / 10, acc + (n % 10))
    sum(n, 0)
  }

  def factorialDigitSum(n: BigInt): BigInt = sumOfDigits(factorial(n))

  def main(args: Array[String]): Unit = { println("Result is " + factorialDigitSum(100) ) }

}
	

package problem298

/* 
 * Selective Amnesia
 * 
 * table.p298, table.p298 th, table.p298 td {
 * border-width: 1px 1px 1px 1px;
 * border-style: solid solid solid solid;
 * border-color: black black black black;
 * text-align:center;
 * -moz-border-radius: 0px 0px 0px 0px;
 * }
 * table.p298 {
 * border-spacing: 1px;
 * border-collapse: separate;
 * background-color: rgb(255,255,255);
 * }
 * table.p298 th, table.p298 td {
 * padding: 1px 6px 1px 6px;
 * }
 * table.p298 th { background-color: rgb(200,220,250); }
 * table.p298 td { background-color: rgb(255,255,255); }
 * Larry and Robin play a memory game involving of a sequence of random numbers between 1 and 10, inclusive, that are called out one at a time. Each player can remember up to 5 previous numbers. When the called number is in a player's memory, that player is awarded a point. If it's not, the player adds the called number to his memory, removing another number if his memory is full.
 * Both players start with empty memories. Both players always add new missed numbers to their memory but use a different strategy in deciding which number to remove:
 * Larry's strategy is to remove the number that hasn't been called in the longest time.
 * Robin's strategy is to remove the number that's been in the memory the longest time.
 * Example game:Turn
 * Callednumber
 * Larry'smemory
 * Larry'sscore
 * Robin'smemory
 * Robin'sscore
 * 1
 * 1
 * 1
 * 0
 * 1
 * 0
 * 2
 * 2
 * 1,2
 * 0
 * 1,2
 * 0
 * 3
 * 4
 * 1,2,4
 * 0
 * 1,2,4
 * 0
 * 4
 * 6
 * 1,2,4,6
 * 0
 * 1,2,4,6
 * 0
 * 5
 * 1
 * 1,2,4,6
 * 1
 * 1,2,4,6
 * 1
 * 6
 * 8
 * 1,2,4,6,8
 * 1
 * 1,2,4,6,8
 * 1
 * 7
 * 10
 * 1,4,6,8,10
 * 1
 * 2,4,6,8,10
 * 1
 * 8
 * 2
 * 1,2,6,8,10
 * 1
 * 2,4,6,8,10
 * 2
 * 9
 * 4
 * 1,2,4,8,10
 * 1
 * 2,4,6,8,10
 * 3
 * 10
 * 1
 * 1,2,4,8,10
 * 2
 * 1,4,6,8,10
 * 3
 * Denoting Larry's score by L and Robin's score by R, what is the expected value of |L-R| after 50 turns? Give your answer rounded to eight decimal places using the format x.xxxxxxxx . 
 */

object Euler298 {

  def main(args: Array[String]): Unit = { println("Result is ") }

}
	
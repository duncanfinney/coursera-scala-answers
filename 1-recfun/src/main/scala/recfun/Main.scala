package recfun

import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def isBalanced(chars: List[Char], numberOpen: Int): Boolean = chars match {
      //reached the nd of the string, lets get out of here
      case Nil => numberOpen == 0
      case '(' :: tail => isBalanced(tail, numberOpen + 1)
      case ')' :: tail => numberOpen > 0 && isBalanced(tail, numberOpen - 1) // have to account for them closing before opening.
      case _ :: tail => isBalanced(tail, numberOpen)
    }
    isBalanced(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeImp(money: Int, coins: List[Int]): Int = {
      if (coins == Nil) 0                 // end of the list
      else if (money - coins.head == 0) 1 //we have a match
      else if (money - coins.head < 0) 0  // this route is not going to work
      else countChangeImp(money - coins.head, coins) + countChangeImp(money, coins.tail) //using this guy + not using this guy to be exaustive
    }

    val sorted = coins.sorted
    countChangeImp(money, sorted)
  }
}

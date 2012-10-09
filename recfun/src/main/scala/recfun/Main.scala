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
    else {
      pascal(c - 1, r -1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def innerBalance(charList: List[Char], leftParenInStack: Int): Boolean = {
      charList match {
        case Nil => leftParenInStack == 0
        case x :: y => {
          x.toString match {
            case "(" => innerBalance(y, leftParenInStack + 1)
            case ")" => 
                  if (leftParenInStack > 0) innerBalance(y, leftParenInStack - 1) 
                  else false
            case z => innerBalance(y, leftParenInStack)
          }
        }
      }
    }

    innerBalance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else {
      if (coins.isEmpty || money < 0) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
}

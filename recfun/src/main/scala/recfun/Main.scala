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
   * @throws java.lang.IndexOutOfBoundsException if column>row
   */
  def pascal(c: Int, r: Int): Int =
    if (c > r) throw new java.lang.IndexOutOfBoundsException("column>row") else if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    {
      def _balance(chars: List[Char], opened: Int): Boolean =
        if (chars.size == 0) (opened == 0) else
          chars.head match {
            case '(' => _balance(chars.tail, opened + 1)
            case ')' => if (opened > 0) _balance(chars.tail, opened - 1) else false
            case _ => _balance(chars.tail, opened)
          }
      _balance(chars, 0)
    }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1 else if (coins.isEmpty) 0 else if (coins.head > money) countChange(money, coins.tail) else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}

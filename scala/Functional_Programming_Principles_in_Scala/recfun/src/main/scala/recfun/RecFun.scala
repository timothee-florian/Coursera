package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c <= 0) 1
    else if (c >= r) 1
    else  pascal(c-1, r-1) + pascal(c, r-1)
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    balance(chars, 0)
  }

  private def balance(chars: List[Char], unbalancedCount: Int): Boolean = {
    if (chars.isEmpty) unbalancedCount == 0
    else if (unbalancedCount < 0) false
    else {
      val count =
        if (chars.head == '(') unbalancedCount + 1
        else if (chars.head == ')') unbalancedCount - 1
        else unbalancedCount
      balance(chars.tail, count)
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0 ) 1
    else if (money > 0 && coins.isEmpty) 0
    else if (money < 0) 0
    else countChange(money-coins.head, coins)  + countChange(money, coins.tail)

  }
}

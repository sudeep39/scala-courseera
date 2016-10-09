package recfun

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
    if (chars.isEmpty) true
    else {
      var result: Int = 0
      for (c <- chars; if result >= 0) {
        if (c == '(')
          result = result + 1
        else if (c == ')')
          result = result - 1
      }
      result == 0
    }
  }

  def balanceParenthesis(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], left: Int): Boolean = {
      if (chars.isEmpty) left == 0
      else if (chars.head == ')') {
        left > 0 && balance(chars.tail, left - 1)
      } else if (chars.head == '(') {
        balance(chars.tail, left + 1)
      } else {
        balance(chars.tail, left)
      }
    }
    balance(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money > 0 && coins.nonEmpty) {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
    else
      0
  }
}
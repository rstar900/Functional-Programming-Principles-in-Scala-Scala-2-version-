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
  def pascal(c: Int, r: Int): Int = 
    if (c == 0 || c == r) 1 else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceHelper(parentheses_list: List[Char], chars_itr: List[Char]): Boolean =
      // Base case
      if (chars_itr.isEmpty) 
        if (parentheses_list.isEmpty)
          true else false
      // recursive case
      // We are only considering '(' and ')'
      else if (parentheses_list.isEmpty && chars_itr.head == ')')
        // Nothing to pop, means already unbalanced
        false
      else if (!parentheses_list.isEmpty && chars_itr.head == ')' && parentheses_list.head =='(')
        // Pop the '('
        balanceHelper(parentheses_list.tail, chars_itr.tail) 
      else if (chars_itr.head =='(')
        // Add the '(' to the head
        balanceHelper('(' +: parentheses_list, chars_itr.tail)
      else
        // Otherwise just ignore and move ahead
       balanceHelper(parentheses_list, chars_itr.tail)

     // Function application  
     balanceHelper("".toList, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeHelper(sum: Int, coins: List[Int]): Int =
      // Base case 1 (Nothing to choose from or we exceeded the amount)
      if (coins.isEmpty || sum > money)
        0 
      // Base case 2 (Sum is exactly the same as the amount)  
      else if (sum == money)
        1
      // Recursive case (total combinations by choosing the coin or not choosing it)
      else
        countChangeHelper(sum + coins.head, coins) + countChangeHelper(sum, coins.tail)

    // Function application
    if (money > 0) countChangeHelper(0, coins) else 0
  }
}

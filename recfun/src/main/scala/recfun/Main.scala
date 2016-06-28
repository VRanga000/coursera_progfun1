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
      if(c < 0 || r < 0) 0
      else if(c > r) 0
      else if(c == 0 ||  c == r) 1
      else {
        pascal(c-1,r-1)+pascal(c,r-1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balance(chars: List[Char], openCount: Int):Boolean = {
        if(chars.isEmpty) true
        else chars.head match {
          case '(' => balance(chars.tail, openCount+1)
          case ')' => openCount > 0 && balance(chars.tail, openCount-1)
          case _ => balance(chars.tail, openCount)
        }
      }
      balance(chars,0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      money match {
        case _ if(!coins.isEmpty && money > 0) =>
          countChange(money - coins.head, coins) + countChange(money, coins.tail)
        case 0 => 1
        case _ => 0
      }
    }
  }

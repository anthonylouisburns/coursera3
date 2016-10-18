def countChange(money: Int, coins: List[Int]): Int = coins match {

  case Nil => {
    println(money)
    println(coins)
    return 0
  }
  case h :: t => {
    if (money == 0) return 1
    else if (money - coins.head < 0)  return 0
    else countChange(money - coins.head, coins)
    + countChange(money, t)
  }


}

countChange(1, List(1, 2))













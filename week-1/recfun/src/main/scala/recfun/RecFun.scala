package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if c == 0 || c == r then 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =

    val scores = Map(
      '(' -> 1,
      ')' -> -1
    )

    @tailrec
    def step(chars: List[Char], imbalancedParentheses: Int): Boolean =
      if imbalancedParentheses < 0 then false
      else if chars.isEmpty then imbalancedParentheses == 0
      else
        val tail = if chars.length == 1 then List.empty else chars.tail
        step(tail, imbalancedParentheses + scores.getOrElse(chars.head, 0))

    step(chars, 0)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if money == 0 then 1
    else if coins.isEmpty || money < 0 then 0
    else
      val tail = if coins.length > 1 then coins.empty else List.empty
      countChange(money, coins.tail) + countChange(money - coins.head, coins)

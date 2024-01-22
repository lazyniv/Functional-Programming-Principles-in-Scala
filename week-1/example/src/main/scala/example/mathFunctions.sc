import scala.annotation.tailrec

def abs(d: Double): Double =
  if d < 0 then -d else d

def square(x: Double) = x * x


def sqrt(x: Double) =
  // This function might be improved because for big numbers (like 1e50)
  // it gets stuck
  def isGoodEnough(guess: Double): Boolean =
    abs(square(guess) - x) < 0.000001

  def improve(guess: Double): Double =
    0.5 * (guess + x / guess)

  @tailrec
  def sqrtIter(guess: Double): Double =
    if isGoodEnough(guess) then guess
    else sqrtIter(improve(guess))

  sqrtIter(1.0)


def factorial(n: Int): Int =
  @tailrec
  def factorialStep(n: Int, currentResult: Int): Int =
    val newResult = currentResult * n
    if n == 1 then newResult else factorialStep(n - 1, newResult)

  factorialStep(n, 1)

factorial(5)
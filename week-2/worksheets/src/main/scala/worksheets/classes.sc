import scala.annotation.targetName

class Rational(x: Int, y: Int):
  require(y > 0, "denominator must be positive")
  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a % b)

  def this(x: Int) = this(x, 1)

  val numerator = x / gcd(x.abs, y)
  val denominator = y / gcd(x.abs, y)

  def add(r: Rational) =
    Rational(numerator * r.denominator + r.numerator * denominator, denominator * r.denominator)

  def less(r: Rational) =
    numerator * r.denominator < r.numerator * denominator

  override def toString: String =
    if numerator % denominator == 0
    then s"${numerator/denominator}"
    else s"$numerator/$denominator"
end Rational

extension (r: Rational)
  def +(y: Rational): Rational = r.add(y)
  def abs: Rational = Rational(r.numerator.abs, r.denominator)
  infix def min(y: Rational) = if r.less(y) then r else y


val r1 = Rational(-1, 2) + Rational(3, 4)
r1.abs
val r2 = Rational(13, 15)

r1 min r2
r1.min(r2)
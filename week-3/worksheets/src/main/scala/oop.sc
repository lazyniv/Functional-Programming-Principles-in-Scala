abstract class IntSet:
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet


class NotEmptySet(root: Int, left: IntSet, right: IntSet) extends IntSet:
  override def incl(x: Int): IntSet =
    if x < root then NotEmptySet(root, left.incl(x), right)
    else if x > root then NotEmptySet(root, left, right.incl(x))
    else this

  override def contains(x: Int): Boolean =
    if x < root then left.contains(x)
    else if x > root then right.contains(x)
    else true

  override def union(other: IntSet): IntSet =
    left.union(right).union(other).incl(root)


object EmptySet extends IntSet:
  override def incl(x: Int): IntSet = NotEmptySet(x, EmptySet, EmptySet)

  override def contains(x: Int): Boolean = false

  override def union(other: IntSet): IntSet = other


extension (x: Boolean)
  // Custom operator
  def ==> (y: Boolean) = if x then y else true

false ==> false


trait NaturalNumber:
  def isZero: Boolean
  def predecessor: NaturalNumber
  def successor: NaturalNumber
  def + (that: NaturalNumber): NaturalNumber
  def - (that: NaturalNumber): NaturalNumber
end NaturalNumber


class Succ(n: NaturalNumber) extends NaturalNumber:
  override def isZero: Boolean = false
  override def predecessor: NaturalNumber = n
  override def successor: NaturalNumber = Succ(this)
  override def +(that: NaturalNumber): NaturalNumber = Succ(n + that)
  override def -(that: NaturalNumber): NaturalNumber = if that.isZero then that else n - that.predecessor

  override def toString: String = s"Succ($n)"


object Zero extends NaturalNumber:
  override def isZero: Boolean = true
  override def predecessor: NaturalNumber = ???
  override def successor: NaturalNumber = Succ(this)
  override def + (that: NaturalNumber): NaturalNumber = that
  override def - (that: NaturalNumber): NaturalNumber = if that.isZero then this else ???
  override def toString: String = "Zero"
end Zero


val one = Succ(Zero)
val two = one + one
Zero

object id:
  def apply[T](x: T) = x

id(10)

new Function1[Int, Int]:
  override def apply(v1: Int): Int = x * x

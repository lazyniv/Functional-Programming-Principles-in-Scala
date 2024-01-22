/**
 * Vectors are immutable, but unlike lists, they are implemented as trees with a high branching factor, providing
 * efficient random access and updates.
 * Vectors have logarithmic time complexity for most operations, making them suitable for scenarios where random access
 * or updates are frequent.
 * Vectors are generally more memory-intensive than lists but become more efficient as the collection size grows.
 * Inheritance hierarchy: {Vector, List} < Seq < Iterable
 */
val v = Vector(1, 2, 3)
v match
  case x1 +: xs => println(s"x1=$x1 xs=$xs")

10 +: v // prepend
v :+ 10 // append

// Array and String can't be directly inherited from Seq trait since they come from Java, but Scala implicitly
// converts them to mimic operations defined in Seq

val a = Array(1, 2, 3)
a.filter(x => x % 2 == 1) // odds

val s = "Hey Scala"
s.map(_.toTitleCase)

// Ranges - represent a sequence of evenly spaced integers
val rangeEx = 1 until 5 // exclusive
val rangeInc = 1 to 5 // inclusive
val rangeByStep = 1 to 5 by 2 // by step

// To list all combinations of numbers x and y where x is drawn from 1..M and y is drawn from 1..N
def combinations(n: Int, m: Int): IndexedSeq[(Int, Int)] =
  (1 to n).flatMap(x => (1 to m).map(y => (x, y)))

combinations(3, 3)

def isPrime(n: Int): Boolean =
  (2 until n).forall(n % _ != 0)

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  xs.zip(ys).map(_ * _).sum

scalarProduct(Vector(1, 2), Vector(3, 4))


// Sets
val fruit = Set("apple", "banana", "pear")
val digits = (1 to 6).toSet

// Most operations on sequences are also available on sets
digits.map(_ + 2).to(List)
fruit.filter(_.startsWith("app"))
s.nonEmpty

// Maps
val capitalOfCountry = Map("US" -> "Washington", "UK" -> "London", "Armenia" -> "Yerevan")

// Map[Key, Value] < Iterable[(Key, Value)] => maps support the same collections operations as other iterables do
val countryOfCapital = capitalOfCountry.map((x, y) => (y, x))

// Map[Key, Value] < Function[Key, Value] => maps can be used everywhere functions can
capitalOfCountry("US")

// map.get(...) returns Option[Value]
def showCapital(country: String) = capitalOfCountry.get(country) match
  case Some(capital) => capital
  case None => "missing data"

showCapital("US")
showCapital("Andorra")

// updating maps
capitalOfCountry + ("Germany" -> "Berlin")





import scala.annotation.tailrec

@tailrec
def last[T](xs: List[T]): T = xs match
  case Nil => throw Error("last of empty list")
  case y :: Nil => y
  case y :: ys => last(ys)

def init[T](xs: List[T]): List[T] = xs match
  case Nil => throw Error("init of empty list")
  case y :: Nil => Nil
  case y :: ys => y :: init(ys)

def removeAt[T](n: Int, xs: List[T]): List[T] = xs match
  case Nil => Nil
  case y :: ys => if n == 0 then ys else y :: removeAt(n - 1, ys)

def flatten(xs: Any): List[Any] = xs match
  case Nil => Nil
  case y :: ys => flatten(y) ::: flatten(ys)
  case _ => xs :: Nil

extension [T](xs: List[T])
  def splitAt(n: Int) = (xs.take(n), xs.drop(n))

def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] =
  val middle = xs.length / 2
  if middle == 0 then xs
  else
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match
      case (xs, Nil) => xs
      case (Nil, ys) => ys
      case (x::xs1, y::ys1) =>
        if lt(x, y) then x :: merge(xs1, ys) else y :: merge(xs, ys1)
    val (ys, zs) = xs.splitAt(middle)
    merge(msort(ys)(lt), msort(zs)(lt))

def pack[T](xs: List[T]): List[List[T]] = xs match
  case Nil => Nil
  case y :: ys =>
    val (duplicates, rest) = ys.span(c => c == y)
    (y :: duplicates) :: pack(rest)

def lengthEncode[T](xs: List[List[T]]): List[(T, Int)] =
  xs.map(duplicates => (duplicates.head, duplicates.length))

val l = 5 :: 4 :: 3 :: List(3, 2, List(6, 5, 0)) :: Nil
last(l)
init(l)
removeAt(1, l)
flatten(l)

val ll = 5 :: 4 :: 3 :: 10 :: 11 :: 1  :: Nil
msort(ll)((x, y) => x < y)

val lll = List("a", "a", "a", "b", "c", "c", "a")
val packed = pack(lll)
lengthEncode(packed)

ll.reduceLeft(_ * _)



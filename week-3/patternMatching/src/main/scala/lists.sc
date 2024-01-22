val l1 = 1 :: 2 :: 3 :: Nil
val l2 = List(1, 2, 3)
l1 == l2

l1 match
  case x :: xs => println(s"l1.head = ${x}; l1.tail = ${xs}")

def insert(x: Int, xs: List[Int]): List[Int] = xs match
  case List() => x :: Nil
  case y :: ys =>
    if x < y then x :: xs else y :: insert(x, ys)


def isort(xs: List[Int]): List[Int] = xs match
  case List() => List()
  case y :: ys => insert(y, isort(ys))


val l3 = 1 :: 5 :: 2 :: 1 :: 9 :: Nil
isort(l3)
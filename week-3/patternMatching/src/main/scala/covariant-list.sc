enum MyList[+T]:
  case Cons(x: T, xs: MyList[T])
  case Empty extends MyList[Nothing]

  def isEmpty = this match
    case Empty => true
    case _ => false

  override def toString: String =
    def recur(prefix: String, xs: MyList[T]): String = xs match
      case Cons(y, ys) => s"$prefix$y${recur(", ", ys)}"
      case Empty => ")"
    recur("List(", this)

//  def prepend[U >: T](elem: U): MyList[U] = Cons(elem, this)

import MyList._

/**
 * Using extensions makes possible to avoid supertype
 * declaration as in the above example
 */
extension[T](xs: MyList[T])
  def prepend(x: T): MyList[T] = Cons(x, xs)

val l1 = Cons(1, Cons(2, Cons(3, Empty))).prepend(1)

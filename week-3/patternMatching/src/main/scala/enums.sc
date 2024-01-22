import java.util.Date

enum Expr:
  case Var(s: String)
  case Number(n: Int)
  case Sum(e1: Expr, e2: Expr)
  case Prod(e1: Expr, e2: Expr)

  def eval: Int = this match
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
    case Prod(e1, e2) => e1.eval * e2.eval

  def show: String = this match
    case Number(n) => n.toString
    case Sum(e1, e2) => s"${e1.show} + ${e2.show}"
    case Prod(e1, e2) => s"(${e1.show}) * (${e2.show})"


enum Color:
  case Red, Green, Blue

enum Direction(val dx: Int, val dy: Int):
  case Right extends Direction(1, 0)
  case Up extends Direction(0, 1)
  case Left extends Direction(-1, 0)
  case Down extends Direction(0, -1)


Direction.values(1)

enum Card:
  case Visa, Mastercard, Amex

enum PaymentMethod:
  case CreditCard(kind: Card, holder: String, number: Long, expires: Date)
  case PayPal(email: String)
  case Cash

//PaymentMethod.values // .values method doesn't exist on enums with parametrized cases
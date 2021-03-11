package cs320

sealed trait Value

object Value {

  type Env = Map[String, Value]
  type Cont = Value => Value

  case class IntV(value: BigInt) extends Value
  case class BooleanV(value: Boolean) extends Value
  case class TupleV(values: List[Value]) extends Value
  case object NilV extends Value
  case class ConsV(head: Value, tail: Value) extends Value
  case class CloV(parameters: List[String], body: Expr, var env: Env) extends Value
  case class ContV(continuation: Cont) extends Value

  def show(value: Value): String = value match {
    case IntV(n) => n.toString
    case BooleanV(b) => b.toString
    case TupleV(vs) => vs.map(show).mkString("(", ", ", ")")
    case NilV => "Nil"
    case ConsV(h, t) => s"(${show(h)} :: ${show(t)})"
    case CloV(_, _, _) => "<function>"
    case ContV(_) => "<continuation>"
  }
}

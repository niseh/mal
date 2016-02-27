package mal.scalapony

sealed trait MalType extends Any

class MalVoid extends MalType

case class MalList(elements: List[Any] = List[Any]()) extends MalType

// symbols: + - * /
sealed trait MalSymbol extends MalType
case object Plus extends MalSymbol
case object Minus extends MalSymbol
case object Multiply extends MalSymbol
case object Divide extends MalSymbol

object MalSymbol {
  def apply(symbol: String): MalSymbol = {
    symbol match {
      case "+" => Plus
      case "-" => Minus
      case "*" => Multiply
      case "/" => Divide
      case _   => throw new Exception("Not a MalSymbol")
    }
  }
}

// Numbers
case class MalInt(value: Int) extends MalType

/*
abstract class MalList(elements: List[T]) extends MalType with List[Any]{
  def isEmpty: Boolean
  def isNotEmpty: Boolean
  def head: T
  def tail: MalList[T]
  def length: Int
}

case object MalNilList extends MalList[MalVoid] {
  def isEmpty = true
  def isNotEmpty = !isEmpty
  def head: MalVoid = throw new Exception("Empty List has no head")
  def tail: MalList[MalVoid] = throw new Exception("Empty List has no tail")
}
*/

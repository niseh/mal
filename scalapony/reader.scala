package mal.scalapony

import scala.util.{ Try, Success, Failure }

case class Reader(tokens: Seq[String], position: Int)

object Reader {

  val synthax = """[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)""".r

  def next(reader: Reader): (String, Reader) = {
    val tokens    = reader.tokens
    val position  = reader.position

    val token = {
      if (tokens.length > position) tokens(position)
      else throw new Exception("[-] Error: Reader position out of bound")
    }

    (token, reader.copy(position=position+1))
  }

  def peak(reader: Reader): String = {
    val tokens    = reader.tokens
    val position  = reader.position

    if (tokens.length > position)
      reader.tokens(reader.position)
    else throw new Exception("[-] Error: Reader position out of bound")

  }

  def read_str(str: String) = {
    val tokens: List[String] = Reader.tokenize(str)
    val reader = Reader(tokens, 0)

    read_form(reader, MalList())
  }

  def tokenize(str: String): List[String] = synthax.findAllIn(str).toList

  def read_form(reader: Reader, soFar: MalList): (MalType, Reader) = {
    val firstToken = Reader.peak(reader)
    val firstChar  = firstToken.charAt(0)

    firstChar match {
      case '(' => Reader.read_list(reader, soFar)
      case _   => Reader.read_atom(reader)
    }
  }

  def read_list(reader: Reader, soFar: MalList): (MalType, Reader) = {
    val listElements   = soFar.elements
    val tokens         = reader.tokens
    val readerPosition = reader.position

    val noMoreTokens   = tokens.length <= readerPosition
    val lastListChar   = listElements.last.asInstanceOf[String].charAt(0)

    if (noMoreTokens && lastListChar != ')') {
      throw new Exception("[-] Error: Missing closing ')'")
    } else {
      val (next, newReader) = Reader.next(reader)
      val newList = MalList(elements = listElements :+ next)

      if (next.charAt(0) == ')') {
        (newList, newReader)
      } else {
        read_form(newReader, newList)
      }
    }
  }

  def read_atom(reader: Reader): (MalType, Reader) = {
    val (next, newReader) = Reader.next(reader)
    val trimmed           = next.trim

    def isInteger(value: String): Try[MalInt] = try {
      val parsed = trimmed.toInt
      Success(MalInt(parsed))
    } catch { case e: Exception =>
      Failure(e)
    }

    def isSymbol(value: String): Try[MalSymbol] = try {
      val parsed = MalSymbol(value)
      Success(parsed)
    } catch { case e: Exception =>
      Failure(e)
    }

    val parsedInteger      = isInteger(trimmed)

    if (isInteger(trimmed).isSuccess) {
      val value = isInteger(trimmed).asInstanceOf[Success[MalInt]].value
      (value, newReader)
    } else if (isSymbol(trimmed).isSuccess) {
      val value = isSymbol(trimmed).asInstanceOf[Success[MalSymbol]].value
      (value, newReader)
    }
    else throw new Exception("Unknown data type")
  }

}

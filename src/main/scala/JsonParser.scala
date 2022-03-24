package io.geekya.sayu

import scala.util.parsing.combinator.*

sealed trait JsonValue

case object JsonNull extends JsonValue

case class JsonBool(value: Boolean) extends JsonValue

case class JsonNumber(value: Double) extends JsonValue

case class JsonString(value: String) extends JsonValue

case class JsonArray(value: List[JsonValue]) extends JsonValue

case class JsonObject(value: Map[String, JsonValue]) extends JsonValue

object JsonParser extends RegexParsers {
  private def _null: Parser[JsonValue] = "null".r ^^ { _ => JsonNull }
  private def _true: Parser[JsonValue] = "true".r ^^ { _ => JsonBool(true) }
  private def _false: Parser[JsonValue] = "false".r ^^ { _ => JsonBool(false) }
  private def _bool: Parser[JsonValue] = _true | _false
  private def _number: Parser[JsonValue] = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[lLdDfF]?""".r ^^ (s => JsonNumber(s.toDouble))
  private def _str: Parser[String]  = "\"" ~> """([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*""".r <~ "\""
  private def _string: Parser[JsonValue] = _str ^^ (s => JsonString(s))
  private def _array: Parser[JsonValue] = "[" ~> repsep(_value, ",") <~ "]" ^^ (l => JsonArray(l))
  private def _object: Parser[JsonValue] = "{" ~> repsep(_pair, ",") <~ "}" ^^ (l => JsonObject(l.toMap))
  private def _pair: Parser[(String, JsonValue)] = _str ~ ":" ~ _value ^^ { case s ~ ":" ~ v => (s, v) }
  private def _value: Parser[JsonValue] = _null | _bool | _number | _string | _array | _object
  def json: Parser[JsonValue] = _value
}

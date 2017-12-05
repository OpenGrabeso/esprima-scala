package esprima
package port

import scala.language.implicitConversions

trait StringOps {
  def fromCharCode(char: Int): String = char.toChar.toString

  case class CharValue(c: Char) {
    override def equals(that: scala.Any) = {
      that match {
        case value: CharValue =>
          value.c == this.c
        case value: Int =>
          value == this.c.toInt
        case value: Char =>
          value == this.c
        case value: String =>
          value == this.c.toString
        case _ =>
          false
      }
    }
  }

  implicit def charValueFromChar(c: Char): CharValue = CharValue(c)
  implicit def charValueToString(c: CharValue): String = c.c.toString
  implicit def charToString(c: Char): String = c.toString
  implicit def charValueToInt(c: CharValue): Int = c.c.toInt

  implicit class StringOps(s: String) {
    def slice(from: Int): String = s.drop(from)
    def charCodeAt(i: Int): CharValue = CharValue(s(i))
    def substr(from: Int, count: Int): String = s.substring(from, from + count)
  }
}
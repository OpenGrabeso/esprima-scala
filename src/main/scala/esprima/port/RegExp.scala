package esprima
package port

// adapted from scala.js.RegExp facade

import RegExp._

object RegExp extends Object {
  def apply(pattern: String, flags: String = ""): RegExp = ???

  trait ExecResult {
    var results: Array[String]
    def apply(i: Int): String = results.apply(i)
    var index: Int
    var input: String
  }
}

// simulate JS RegExp using Scala or Java means
class RegExp(pattern: String, flags: String = "") {
  var lastIndex = -1

  def exec(input: String): ExecResult = ???

  def strictBind = ???

  def strict = ???

  def test(value: String): Boolean = ???

}

package esprima
package port

// adapted from scala.js.RegExp facade


object RegExp extends Object {
  def apply(pattern: String, flags: String = ""): RegExp = new RegExp(pattern, flags)

  trait ExecResult {
    var results: Array[String]
    def apply(i: Int): String = results.apply(i)
    var index: Int
    var input: String
  }
}

import RegExp._

// simulate JS RegExp using Scala or Java means
class RegExp(pattern: String, flags: String = "") {

  def test(value: String): Boolean = ???

}

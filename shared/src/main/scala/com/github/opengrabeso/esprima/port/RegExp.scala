package com.github.opengrabeso.esprima
package port

// adapted from scala.js.RegExp facade


object RegExp extends Object {
  def apply(pattern: String, flags: String = ""): RegExp = {
    // remove starting and ending /
    // remove ending flags
    // if there are any {}, escape them (they have a special meaning in Java)
    val escaped = pattern.replaceAll("\\{", "\\\\{").replaceAll("\\}", "\\\\}")
    new RegExp(escaped, flags)
  }

  trait ExecResult {
    var results: Array[String]
    def apply(i: Int): String = results.apply(i)
    var index: Int
    var input: String
  }
}

import RegExp._

import java.util.regex.Pattern

// simulate JS RegExp using Scala or Java means
class RegExp(pattern: String, flags: String = "") {

  // TODO: parse flags

  // this is lazy, so that expressions are not compiled and validated until needed
  // we do not want errors to be thrown on invalid regexes
  private lazy val pat = Pattern.compile(pattern, 0)

  def test(value: String): Boolean = {
    pat.matcher(value).matches
  }

}

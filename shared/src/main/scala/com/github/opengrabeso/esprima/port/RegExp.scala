package com.github.opengrabeso.esprima
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

import java.util.regex.Pattern

// simulate JS RegExp using Scala or Java means
class RegExp(pattern: String, flags: String = "") {

  // TODO: parse flags
  val pat = Pattern.compile(pattern, 0)

  def test(value: String): Boolean = {
    pat.matcher(value).matches
  }

}

/*
ScalaFromJS: 2017-12-06 21:28:23.723
types.js
*/

package types.js
// Introduce types which can serve as interfaces
/* import {SourceLocation} from "./src/scanner" */
import esprima.SourceLocation
/* import {Token} from "./src/token" */
import esprima.Token

class Node() {
  var `type`: Any = ""
  var range = Array(0, 0)
  var loc: SourceLocation = new SourceLocation()
  var leadingComments = Array.empty[Unit]
  var innerComments = Array.empty[Unit]
  var trailingComments = Array.empty[Unit]
}

class Position() {
  var line: Double = 0
  var column: Double = 0
}

class SourceLocation() {
  var start: Position = new Position()
  var end: Position = new Position()
  var source: String = ""
}

class Comment() {
  var multiLine: Boolean = false
  var slice = Array(0, 0)
  var range = Array(0, 0)
  var loc: SourceLocation = new SourceLocation()
}

class RawToken() {
  var `type`: Double = 0
  var value: String = undefined
  var pattern: String = ""
  var flags: String = ""
  var regex = "/.*/g".r
  var octal: Boolean = false
  var cooked: String = ""
  var head: Boolean = false
  var tail: Boolean = false
  var lineNumber: Double = 0
  var lineStart: Double = 0
  var start: Double = 0
  var end: Double = 0
}

class ScannerState() {
  var index: Double = 0
  var lineNumber: Double = 0
  var lineStart: Double = 0
}


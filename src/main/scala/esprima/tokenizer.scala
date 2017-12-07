/*
ScalaFromJS: 2017-12-06 21:28:23.723
tokenizer.js
*/

package esprima

import esprima.Scanner._
import esprima.port.RegExp

import scala.collection.mutable.ArrayBuffer

class Reader() {
  var paren: Int = -1
  var values = ArrayBuffer.empty[Any]
  var curly: Int = paren
  def beforeFunctionExpression(t: Any) = {
    Array("(", "{", "[", "in", "typeof", "instanceof", "new", "return", "case", "delete", "throw", "void", // assignment operators
    "=", "+=", "-=", "*=", "**=", "/=", "%=", "<<=", ">>=", ">>>=", "&=", "|=", "^=", ",", // binary/unary operators
    "+", "-", "*", "**", "/", "%", "++", "--", "<<", ">>", ">>>", "&", "|", "^", "!", "~", "&&", "||", "?", ":", "===", "==", ">=", "<=", "<", ">", "!=", "!==").indexOf(t) >= 0
  }
  
  def isRegexStart() = {
    val previous = this.values(this.values.length - 1)
    var regex = previous != null
    previous match {
      case "this" | "]" =>
        regex = false
      case ")" =>
        val keyword = this.values(this.paren - 1)
        regex = keyword == "if" || keyword == "while" || keyword == "for" || keyword == "with"
      case "}" =>
        // Dividing a function by anything makes little sense,
        // but we have to check for that.
        regex = true
        if (this.values(this.curly - 3) == "function") {
          // Anonymous function, e.g. function(){} /42
          val check = this.values(this.curly - 4)
          regex = if (check == true) !this.beforeFunctionExpression(check) else false
        } else if (this.values(this.curly - 4) == "function") {
          // Named function, e.g. function f(){} /42/
          val check = this.values(this.curly - 5)
          regex = if (check == true) !this.beforeFunctionExpression(check) else true
        }
      case _ =>
    }
    regex
  }
  
  def push(token: RawToken) = {
    if (token.`type` == 7 || token.`type` == 4) {
      if (token.value === "{") {
        this.curly = this.values.length
      } else if (token.value === "(") {
        this.paren = this.values.length
      }
      this.values.push(token.value)
    } else {
      this.values.push(null)
    }
  }
  
}

class Tokenizer(code: String, config: Parser.Options) {
  self =>
  var errorHandler: ErrorHandler = new ErrorHandler()
  errorHandler.tolerant = config.tolerant
  var scanner: Scanner = new Scanner(code, this.errorHandler)
  scanner.trackComment = config.comment
  var trackRange: Boolean = config.range
  var trackLoc: Boolean = config.loc
  var buffer = ArrayBuffer.empty[Parser.TokenEntry]
  var reader: Reader = new Reader()
  def errors() = {
    this.errorHandler.errors
  }
  
  def getNextToken(): Parser.TokenEntry = {
    if (this.buffer.length == 0) {
      val comments = this.scanner.scanComments()
      if (this.scanner.trackComment) {
        this.buffer ++= comments.map { e =>
          val value_ = this.scanner.source.slice(e.slice._1, e.slice._2)
          object comment extends Parser.TokenEntry {
            var `type` = if (e.multiLine) "BlockComment" else "LineComment"
            var value = value_
          }
          if (this.trackRange) {
            comment.range = e.range
          }
          if (this.trackLoc) {
            comment.loc = e.loc
          }
          comment
        }
      }
      if (!this.scanner.eof()) {
        var loc: SourceLocation = null
        if (this.trackLoc) {
          loc = new SourceLocation {
            var start: Position = new Position {
              override def line = self.scanner.lineNumber
              override def column = self.scanner.index - self.scanner.lineStart
            }
            var end: Position = null
          }
        }
        val maybeRegex = this.scanner.source(this.scanner.index).toString == "/" && this.reader.isRegexStart()
        var token: RawToken = null
        if (maybeRegex) {
          val state = this.scanner.saveState()
          try {
            token = this.scanner.scanRegExp()
          } catch {
            case e: Throwable =>
              this.scanner.restoreState(state)
              token = this.scanner.lex()
          }
        } else {
          token = this.scanner.lex()
        }
        this.reader.push(token)
        object entry extends Parser.TokenEntry {
          var `type` = TokenName(token.`type`)
          var value = self.scanner.source.slice(token.start, token.end)
        }
        if (this.trackRange) {
          entry.range = (token.start, token.end)
        }
        if (this.trackLoc) {
          loc.end = new Position {
            override def line = self.scanner.lineNumber
            override def column = self.scanner.index - self.scanner.lineStart
          }
          entry.loc = loc
        }
        if (token.`type` == 9) {
          val pattern = token.pattern
          val flags = token.flags
          entry.regex = new RegExp (
            pattern = pattern,
            flags = flags
          )
        }
        this.buffer.push(entry)
      }
    }
    if (this.buffer.nonEmpty) this.buffer.shift()
    else null
  }
  
}

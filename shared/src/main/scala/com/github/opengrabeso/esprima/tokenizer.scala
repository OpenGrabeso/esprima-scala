/*
ScalaFromJS: Dev
tokenizer.ts
*/

package com.github.opengrabeso.esprima
/* import { ErrorHandler } from './error-handler' */
/* import { Comment, RawToken, Scanner, SourceLocation } from './scanner' */
/* import { Token, TokenName } from './token' */
type ReaderEntry = String
trait BufferEntry {
  var `type`: String = _
  var value: String = _
  var regex = new {}
  var range = Array.empty[Double]
  var loc: SourceLocation = _
}

class Reader {
  var paren: Double = _
  var values = Array.empty[ReaderEntry]
  var curly: Double = paren = -1
  // A function following one of those tokens is an expression.
  def beforeFunctionExpression(t: String): Boolean = {
    Array("(", "{", "[", "in", "typeof", "instanceof", "new", 
      "return", "case", "delete", "throw", "void", 
      // assignment operators
      "=", "+=", "-=", "*=", "**=", "/=", "%=", "<<=", ">>=", ">>>=", 
      "&=", "|=", "^=", ",", 
      // binary/unary operators
      "+", "-", "*", "**", "/", "%", "++", "--", "<<", ">>", ">>>", "&", 
      "|", "^", "!", "~", "&&", "||", "?", ":", "===", "==", ">=", 
      "<=", "<", ">", "!=", "!==").indexOf(t) >= 0
  }
  
  // Determine if forward slash (/) is an operator or part of a regular expression
  // https://github.com/mozilla/sweet.js/wiki/design
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
          regex = if (check) !this.beforeFunctionExpression(check) else false
        } else if (this.values(this.curly - 4) == "function") {
          // Named function, e.g. function f(){} /42/
          val check = this.values(this.curly - 5)
          regex = if (check) !this.beforeFunctionExpression(check) else true
        }
      case _ =>
    }
    regex
  }
  
  def push(token: RawToken): Unit = {
    if (token.`type` == Token.Punctuator || token.`type` == Token.Keyword) {
      if (token.value == "{") {
        this.curly = this.values.length
      } else if (token.value == "(") {
        this.paren = this.values.length
      }
      this.values.push(token.value)
    } else {
      this.values.push(null)
    }
  }
  
}

/*tslint:disable:max-classes-per-file */

trait Config {
  var tolerant: Boolean = _
  var comment: Boolean = _
  var range: Boolean = _
  var loc: Boolean = _
}

class Tokenizer(code: String, config: Config) {
  var errorHandler: ErrorHandler = new ErrorHandler()
  errorHandler.tolerant = if (config) config.tolerant.getClass == "boolean" && config.tolerant else false
  var scanner: Scanner = new Scanner(code, errorHandler)
  scanner.trackComment = if (config) config.comment.getClass == "boolean" && config.comment else false
  var trackRange: Boolean = if (config) config.range.getClass == "boolean" && config.range else false
  var trackLoc: Boolean = if (config) config.loc.getClass == "boolean" && config.loc else false
  var buffer = Array.empty[BufferEntry]
  var reader: Reader = new Reader()
  def errors() = {
    this.errorHandler.errors
  }
  
  def getNextToken() = {
    if (this.buffer.length == 0) {
      val comments = this.scanner.scanComments()
      if (this.scanner.trackComment) {
        this.buffer ++= comments.map { e =>
          val value = this.scanner.source.slice(e.slice(0), e.slice(1))
          object comment extends BufferEntry {
            var `type` = if (e.multiLine) "BlockComment" else "LineComment"
            var value = value
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
        var loc: SourceLocation = _
        if (this.trackLoc) {
          loc = new {
            var start = new {
              var line = this.scanner.lineNumber
              var column = this.scanner.index - this.scanner.lineStart
            }
            var end = new {}
          }
        }
        val maybeRegex = this.scanner.source(this.scanner.index) == "/" && this.reader.isRegexStart()
        var token: RawToken = _
        if (maybeRegex) {
          val state = this.scanner.saveState()
          try {
            token = this.scanner.scanRegExp()
          } catch {
            case e =>
              this.scanner.restoreState(state)
              token = this.scanner.lex()
          }
        } else {
          token = this.scanner.lex()
        }
        this.reader.push(token)
        object entry extends BufferEntry {
          var `type` = TokenName(token.`type`)
          var value = this.scanner.source.slice(token.start, token.end)
        }
        if (this.trackRange) {
          entry.range = Array(token.start, token.end)
        }
        if (this.trackLoc) {
          loc.end = new {
            var line = this.scanner.lineNumber
            var column = this.scanner.index - this.scanner.lineStart
          }
          entry.loc = loc
        }
        if (token.`type` == Token.RegularExpression) {
          val pattern = token.pattern.asInstanceOf[String]
          val flags = token.flags.asInstanceOf[String]
          entry.regex = new {
            var pattern = pattern
            var flags = flags
          }
        }
        this.buffer.push(entry)
      }
    }
    this.buffer.shift()
  }
  
}


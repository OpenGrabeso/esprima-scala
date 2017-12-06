/*
ScalaFromJS: 2017-12-06 21:28:23.723
tokenizer.js
*/

package esprima
"use strict"
Object.defineProperty(exports, "__esModule", new {
  var value = true
})
val error_handler_1 = require("./error-handler")
val scanner_1 = require("./scanner")
val token_1 = require("./token")

class Reader() {
  var paren: Double = _
  var values = Array.empty[String]
  var curly: Double = paren = -1
  def beforeFunctionExpression(t: String) = {
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
  
  def push(token: RawToken) = {
    if (token.`type` == 7 || token.`type` == 4) {
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

class Tokenizer(code: Any, config: Any) {
  var errorHandler: ErrorHandler = new error_handler_1.ErrorHandler()
  errorHandler.tolerant = if (config) config.tolerant.getClass == "boolean" && config.tolerant else false
  var scanner: Scanner = new scanner_1.Scanner(code, this.errorHandler)
  scanner.trackComment = if (config) config.comment.getClass == "boolean" && config.comment else false
  var trackRange: Boolean = if (config) config.range.getClass == "boolean" && config.range else false
  var trackLoc: Boolean = if (config) config.loc.getClass == "boolean" && config.loc else false
  var buffer = Array.empty[Any]
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
          object comment {
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
        var loc = new {}
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
        object entry {
          var `type` = token_1.TokenName(token.`type`)
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
        if (token.`type` == 9) {
          val pattern = token.pattern
          val flags = token.flags
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

exports.Tokenizer = Tokenizer

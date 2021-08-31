/*
ScalaFromJS: Dev
tokenizer.ts
*/

package com.github.opengrabeso.esprima

import Scanner._
import port.RegExp

import scala.collection.mutable.ArrayBuffer

/* import { ErrorHandler } from './error-handler' */
/* import { Comment, RawToken, Scanner, SourceLocation } from './scanner' */
/* import { Token, TokenName } from './token' */
trait BufferEntry {
  var `type`: String = _
  var value: String = _
  var regex = new {}
  var range: (Int, Int) = _
  var loc: SourceLocation = _
}

class Reader() {
  var paren: Int = -1
  var values = ArrayBuffer.empty[OrType]
  var curly: Int = paren
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
    val previous = this.values(this.values.length - 1).get[String]
    var regex = previous != null
    previous match {
      case "this" | "]" =>
        regex = false
      case ")" =>
        val keyword = this.values(this.paren - 1).get[String]
        regex = keyword == "if" || keyword == "while" || keyword == "for" || keyword == "with"
      case "}" =>
        // Dividing a function by anything makes little sense,
        // but we have to check for that.
        regex = true
        if (this.values(this.curly - 3) === "function") {
          // Anonymous function, e.g. function(){} /42
          val check = this.values(this.curly - 4)
          regex = if (check.is[String]) !this.beforeFunctionExpression(check.get[String]) else false
        } else if (this.values(this.curly - 4) === "function") {
          // Named function, e.g. function f(){} /42/
          val check = this.values(this.curly - 5)
          regex = if (check.is[String]) !this.beforeFunctionExpression(check.get[String]) else true
        }
      case _ =>
    }
    regex
  }

  def push(token: RawToken): Unit = {
    if (token.`type` == Token.Punctuator || token.`type` == Token.Keyword) {
      if (token.value === "{") {
        this.curly = this.values.length
      } else if (token.value === "(") {
        this.paren = this.values.length
      }
      this.values.push(token.value)
    } else {
      this.values.push(OrType(null))
    }
  }
  
}

trait Config {
  var tolerant: Boolean = _
  var comment: Boolean = _
  var range: Boolean = _
  var loc: Boolean = _
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
              override val line = self.scanner.lineNumber
              override val column = self.scanner.index - self.scanner.lineStart
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
            override val line = self.scanner.lineNumber
            override val column = self.scanner.index - self.scanner.lineStart
          }
          entry.loc = loc
        }
        if (token.`type` == Token.RegularExpression) {
          val pattern = token.pattern.asInstanceOf[String]
          val flags = token.flags.asInstanceOf[String]
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

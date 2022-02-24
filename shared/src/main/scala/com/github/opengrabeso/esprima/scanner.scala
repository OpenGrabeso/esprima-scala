/*
ScalaFromJS: Dev
scanner.ts
*/

package com.github.opengrabeso.esprima
import port.RegExp
import Scanner._

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.util.control.Breaks._

object Scanner {
  def hexValue(ch: Int): Int = {
    "0123456789abcdef".indexOf(ch)
  }

  def octalValue(ch: Int): Int = {
    "01234567".indexOf(ch)
  }

  def hexValue(ch: String): Int = {
    "0123456789abcdef".indexOf(ch)
  }

  def octalValue(ch: String): Int = {
    "01234567".indexOf(ch)
  }

  trait Position {
    val line: Int
    val column: Int
    val offset: Int = -1
  }

  trait SourceLocation {
    var start: Position
    var end: Position
    var source: String = null
  }

  trait Comment {
    val multiLine: Boolean
    val slice: (Int, Int)
    val range: (Int, Int)
    val loc: SourceLocation
  }

  type Token = Token.Token

  // See: https://tc39.es/ecma262/#prod-NotEscapeSequence
  type NotEscapeSequenceHead = String

  trait RawToken {
    def `match`(str: String) = {
      // a special case hack: match >> as >
      `type` == Token.Punctuator && (value === str || value === ">>" && str == ">")
    }

    override def toString = `type`.toString + "'" + value.toString

    var `type`: Token = _
    def value: OrType  = ??? // String | Int
    def pattern: String = ??? // UndefOr
    def flags: String = ??? // UndefOr
    def regex: RegExp = ??? // UndefOr
    def octal: Boolean = false // UndefOr
    def cooked: String = ???
    def notEscapeSequenceHead: String = ??? // UndefOr
    def head: Boolean = ??? // UndefOr
    def tail: Boolean = ??? // UndefOr
    def lineNumber: Int = -1
    def lineStart: Int = ???
    def start: Int = ???
    def end: Int = ???
  }

  trait ScannerState {
    def index: Int
    def lineNumber: Int
    def lineStart: Int
    def curlyStack: ArrayBuffer[String]
  }

  // avoid Breaks.breakable in some common loops, it hurts performance too much (esp. when debugging)
  case class Breakable() {
    var broken = false

    def break(): Unit = broken = true
  }
}

//noinspection ComparingUnrelatedTypes,RemoveRedundantReturn
class Scanner(code: String, var errorHandler: ErrorHandler) {
  self =>
  var source = code
  var trackComment: Boolean = false
  var isModule: Boolean = false
  var length: Int = code.length
  var index: Int = 0
  var lineNumber: Int = if (code.length > 0) 1 else 0
  var lineStart: Int = 0
  var curlyStack = ArrayBuffer.empty[String]
  def saveState(): ScannerState = {
    new ScannerState {
      var index = self.index
      var lineNumber = self.lineNumber
      var lineStart = self.lineStart
      var curlyStack = self.curlyStack.clone()
    }
  }
  
  def restoreState(state: ScannerState): Unit = {
    this.index = state.index
    this.lineNumber = state.lineNumber
    this.lineStart = state.lineStart
    this.curlyStack = state.curlyStack
  }
  
  def eof(): Boolean = {
    this.index >= this.length
  }
  
  def throwUnexpectedToken(message: String = Messages.UnexpectedTokenIllegal): Nothing = {
    this.errorHandler.throwError(this.index, this.lineNumber, this.index - this.lineStart + 1, message)
  }
  
  def tolerateUnexpectedToken(message: String = Messages.UnexpectedTokenIllegal) = {
    this.errorHandler.tolerateError(this.index, this.lineNumber, this.index - this.lineStart + 1, message)
  }
  
  def skipSingleLineComment(offset: Int): Array[Comment] = {
    var comments: ArrayBuffer[Comment] = null
    var start: Int = -1
    var loc: SourceLocation = null
    if (this.trackComment) {
      comments = ArrayBuffer()
      start = this.index - offset
      val offset_ = offset
      loc = new SourceLocation {
        var start: Position = new Position {
          val line = self.lineNumber
          val column = self.index - self.lineStart - offset_
        }
        var end: Position = null
      }
    }
    while (!this.eof()) {
      val ch = this.source.charCodeAt(this.index)
      this.index += 1
      if (Character.isLineTerminator(ch)) {
        if (this.trackComment) {
          loc.end = new Position {
            override val line = self.lineNumber
            override val column = self.index - self.lineStart - 1
          }
          val loc_ = loc
          object entry extends Comment {
            override val multiLine = false
            override val slice = (start + offset, self.index - 1)
            val range = (start, self.index - 1)
            val loc = loc_
          }
          comments.push(entry)
        }
        if (ch == 13 && this.source.charCodeAt(this.index) == 10) {
          this.index += 1
        }
        this.lineNumber += 1
        this.lineStart = this.index
        return comments
      }
    }
    if (this.trackComment) {
      loc.end = new Position {
        override val line = self.lineNumber
        override val column = self.index - self.lineStart
      }
      val loc_ = loc
      object entry extends Comment {
        override val multiLine = false
        override val slice = (start + offset, self.index)
        val range = (start, self.index)
        val loc = loc_
      }
      comments.push(entry)
    }
    comments
  }
  
  def skipMultiLineComment(): Array[Comment] = {
    var comments: ArrayBuffer[Comment] = null
    var start: Int = -1
    var loc: SourceLocation = null
    if (this.trackComment) {
      comments = ArrayBuffer()
      start = this.index - 2
      loc = new SourceLocation {
        var start: Position = new Position {
          override val line = self.lineNumber
          override val column = self.index - self.lineStart - 2
        }
        var end: Position = null
      }
    }
    while (!this.eof()) {
      val ch = this.source.charCodeAt(this.index)
      if (Character.isLineTerminator(ch)) {
        if (ch == 0x0D && this.source.charCodeAt(this.index + 1) == 0x0A) {
          this.index += 1
        }
        this.lineNumber += 1
        this.index += 1
        this.lineStart = this.index
      } else if (ch == 0x2A) {
        // Block comment ends with '*/'.
        if (this.source.charCodeAt(this.index + 1) == 0x2F) {
          this.index += 2
          if (this.trackComment) {
            loc.end = new Position{
              override val line = self.lineNumber
              override val column = self.index - self.lineStart
            }
            val loc_ = loc
            object entry extends Comment {
              override val multiLine = true
              override val slice = (start + 2, self.index - 2)
              val range = (start, self.index)
              val loc = loc_
            }
            comments.push(entry)
          }
          return comments
        }
        this.index += 1
      } else {
        this.index += 1
      }
    }
    // Ran off the end of the file - the whole thing is a comment
    if (this.trackComment) {
      loc.end = new Position {
        override val line = self.lineNumber
        override val column = self.index - self.lineStart
      }
      val loc_ = loc
      object entry extends Comment {
        override val multiLine = true
        override val slice = (start + 2, self.index)
        val range = (start, self.index)
        val loc = loc_
      }
      comments.push(entry)
    }
    this.tolerateUnexpectedToken()
    comments
  }

  // needed for Scala 2.11
  implicit def charValueFromChar(c: Char): CharValue = CharValue(c)
  implicit def charValueToInt(c: CharValue): Int = c.c.toInt

  def scanComments() = {
    var comments: ArrayBuffer[Comment] = null
    if (this.trackComment) {
      comments = ArrayBuffer()
    }
    var start = this.index == 0
    val brk = Breakable() // very common, Breaks.breakable was slowing down debugging a lot
    while (!this.eof() && !brk.broken) {
      var ch = this.source.charCodeAt(this.index)
      if (Character.isWhiteSpace(ch)) {
        this.index += 1
      } else if (Character.isLineTerminator(ch)) {
        this.index += 1
        if (ch == 0x0D && this.source.charCodeAt(this.index) == 0x0A) {
          this.index += 1
        }
        this.lineNumber += 1
        this.lineStart = this.index
        start = true
      } else if (ch == 0x2F) { // U+002F is '/'
        ch = this.source.charCodeAt(this.index + 1)
        if (ch == 0x2F) {
          this.index += 2
          val comment = this.skipSingleLineComment(2)
          if (this.trackComment) {
            comments = comments.concat(comment)
          }
          start = true
        } else if (ch == 0x2A) { // U+002A is '*'
          this.index += 2
          val comment = this.skipMultiLineComment()
          if (this.trackComment) {
            comments = comments.concat(comment)
          }
        } else {
          brk.break()
        }
      } else if (start && ch == 0x2D) { // U+002D is '-'
        // U+003E is '>'
        if (this.source.charCodeAt(this.index + 1) == 0x2D && this.source.charCodeAt(this.index + 2) == 0x3E) {
          // '-->' is a single-line comment
          this.index += 3
          val comment = this.skipSingleLineComment(3)
          if (this.trackComment) {
            comments = comments.concat(comment)
          }
        } else {
          brk.break()
        }
      } else if (ch == 0x3C && !this.isModule) { // U+003C is '<'
        if (this.source.slice(this.index + 1, this.index + 4) == "!--") {
          this.index += 4
          // `<!--`
          val comment = this.skipSingleLineComment(4)
          if (this.trackComment) {
            comments = comments.concat(comment)
          }
        } else {
          brk.break()
        }
      } else {
        brk.break()
      }
    }
    comments
  }

  def isFutureReservedWord(id: String): Boolean = {
    id match {
      case "enum" | "export" | "import" | "super" =>
        return true
      case _ =>
        false
    }
  }
  
  def isStrictModeReservedWord(id: String): Boolean = {
    id match {
      case "implements" | "interface" | "package" | "private" | "protected" | "public" | "static" | "yield" | "let" =>
        return true
      case _ =>
        false
    }
  }
  
  def isRestrictedWord(id: String): Boolean = {
    id == "eval" || id == "arguments"
  }

  def isKeyword(id: String): Boolean = {
    id.length match {
      case 2 =>
        return id == "if" || id == "in" || id == "do"
      case 3 =>
        return id == "var" || id == "for" || id == "new" || id == "try" || id == "let"
      case 4 =>
        return id == "this" || id == "else" || id == "case" || id == "void" || id == "with" || id == "enum"
      case 5 =>
        return id == "while" || id == "break" || id == "catch" || id == "throw" || id == "const" || id == "yield" || id == "class" || id == "super"
      case 6 =>
        return id == "return" || id == "typeof" || id == "delete" || id == "switch" || id == "export" || id == "import"
      case 7 =>
        return id == "default" || id == "finally" || id == "extends"
      case 8 =>
        return id == "function" || id == "continue" || id == "debugger"
      case 10 =>
        return id == "instanceof"
      case _ =>
        false
    }
  }
  
  def codePointAt(i: Int): Int = {
    var cp: Int = this.source.charCodeAt(i)
    if (cp >= 0xD800 && cp <= 0xDBFF) {
      val second: Int = this.source.charCodeAt(i + 1)
      if (second >= 0xDC00 && second <= 0xDFFF) {
        val first = cp
        cp = (first - 0xD800) * 0x400 + second - 0xDC00 + 0x10000
      }
    }
    cp
  }
  
  def scanHexEscape(prefix: String): String = {
    val len = if (prefix == "u") 4 else 2
    var code = 0
    for (i <- 0 until len) {
      if (!this.eof() && Character.isHexDigit(this.source.charCodeAt(this.index))) {
        code = code * 16 + hexValue(this.source({
          val temp = this.index
          this.index += 1
          temp
        }).toString)
      } else {
        return null
      }
    }
    fromCharCode(code)
  }
  
  def tryToScanUnicodeCodePointEscape(): String = {
    var ch = this.source(this.index)
    var code = 0
    // At least, one hex digit is required.
    if (ch.toString == "}") {
      return null
    }
    val brk = Breakable()
    while (!this.eof() && !brk.broken) {
      ch = this.source({
        val temp = this.index
        this.index += 1
        temp
      })
      if (!Character.isHexDigit(ch)) {
        brk.break()
      } else {
        code = code * 16 + hexValue(ch)
      }
    }
    if (code > 0x10FFFF || ch.toString != "}") {
      return null
    }
    Character.fromCodePoint(code)
  }
  
  def scanUnicodeCodePointEscape(): String = {
    val result = this.tryToScanUnicodeCodePointEscape()
    if (result == null) {
      return this.throwUnexpectedToken()
    }
    result
  }

  def getIdentifier(): String = {
    val start = {
      val temp = this.index
      this.index += 1
      temp
    }
    val brk = Breakable()
    while (!this.eof() && !brk.broken) {
      val ch = this.source.charCodeAt(this.index)
      if (ch == 0x5C) {
        // Blackslash (U+005C) marks Unicode escape sequence.
        this.index = start
        return this.getComplexIdentifier()
      } else if (ch >= 0xD800 && ch < 0xDFFF) {
        // Need to handle surrogate pairs.
        this.index = start
        return this.getComplexIdentifier()
      }
      if (Character.isIdentifierPart(ch)) {
        this.index += 1
      } else {
        brk.break()
      }
    }
    this.source.slice(start, this.index)
  }
  
  def getComplexIdentifier(): String = {
    var cp = this.codePointAt(this.index)
    var id = Character.fromCodePoint(cp)
    this.index += id.length
    // '\ u' (U+005C, U+0075) denotes an escaped character.
    var ch: String = null
    if (cp == 0x5C) {
      if (this.source.charCodeAt(this.index) != 0x75) {
        this.throwUnexpectedToken()
      }
      this.index += 1
      if (this.source(this.index).toString == "{") {
        this.index += 1
        ch = this.scanUnicodeCodePointEscape()
      } else {
        ch = this.scanHexEscape("u")
        if (ch == null || ch == "\\" || !Character.isIdentifierStart(ch.charCodeAt(0))) {
          this.throwUnexpectedToken()
        }
      }
      id = ch
    }
    breakable {
      while (!this.eof()) {
        cp = this.codePointAt(this.index)
        if (!Character.isIdentifierPart(cp)) {
          break()
        }
        ch = Character.fromCodePoint(cp)
        id += ch
        this.index += ch.length
        // '\ u' (U+005C, U+0075) denotes an escaped character.
        if (cp == 0x5C) {
          id = id.substr(0, id.length - 1)
          if (this.source.charCodeAt(this.index) != 0x75) {
            this.throwUnexpectedToken()
          }
          this.index += 1
          if (this.source(this.index).toString == "{") {
            this.index += 1
            ch = this.scanUnicodeCodePointEscape()
          } else {
            ch = this.scanHexEscape("u")
            if (ch == null || ch == "\\" || !Character.isIdentifierPart(ch.charCodeAt(0))) {
              this.throwUnexpectedToken()
            }
          }
          id += ch
        }
      }
    }
    id
  }
  
  def octalToDecimal(ch: String): (Int, Boolean) = {
    // \0 is not octal escape sequence
    var octal = ch != "0"
    var code = octalValue(ch(0))
    if (!this.eof() && Character.isOctalDigit(this.source.charCodeAt(this.index))) {
      octal = true
      code = code * 8 + octalValue(this.source({
        val temp = this.index
        this.index += 1
        temp
      }))
      // 3 digits are only allowed when string starts
      // with 0, 1, 2, 3
      if ("0123".indexOf(ch) >= 0 && !this.eof() && Character.isOctalDigit(this.source.charCodeAt(this.index))) {
        code = code * 8 + octalValue(this.source({
          val temp = this.index
          this.index += 1
          temp
        }))
      }
    }
    code -> octal
  }

  // https://tc39.github.io/ecma262/#sec-names-and-keywords
  def scanIdentifier(): RawToken = {
    var `type`: Token = Token.Undefined
    val start = this.index
    // Backslash (U+005C) starts an escaped character.
    val id = if (this.source.charCodeAt(start) == 0x5C) this.getComplexIdentifier() else this.getIdentifier()
    // There is no keyword or literal with only one character.
    // Thus, it must be an identifier.
    if (id.length == 1) {
      `type` = Token.Identifier
    } else if (this.isKeyword(id)) {
      `type` = Token.Keyword
    } else if (id == "null") {
      `type` = Token.NullLiteral
    } else if (id == "true" || id == "false") {
      `type` = Token.BooleanLiteral
    } else {
      `type` = Token.Identifier
    }
    if (`type` != Token.Identifier && start + id.length != this.index) {
      val restore = this.index
      this.index = start
      this.tolerateUnexpectedToken(Messages.InvalidEscapedReservedWord)
      this.index = restore
    }
    val _type = `type`
    val _start = start
    new RawToken {
      `type` = `_type`
      override val value = OrType(id)
      override val lineNumber = self.lineNumber
      override val lineStart = self.lineStart
      override val start = _start
      override val end = self.index
    }
  }

  def scanPunctuator(): RawToken = {
    val start = this.index
    // Check for most common single-character punctuators.
    var str = this.source(this.index).toString
    str match {
      case "(" | "{" =>
        if (str == "{") {
          this.curlyStack.push("{")
        }
        this.index += 1
      case "." =>
        this.index += 1
        if (this.source(this.index).toString == "." && this.source(this.index + 1).toString == ".") {
          // Spread operator: ...
          this.index += 2
          str = "..."
        }
      case "}" =>
        this.index += 1
        if (this.curlyStack.nonEmpty) {
          this.curlyStack.pop()
        } else {
          this.throwUnexpectedToken()
        }
      case "?" =>
        this.index += 1
        if (this.source(this.index) == '?') {
          this.index += 1
          str = "??"
        }
        if (this.source(this.index) == '.' && !new RegExp("^\\d$").test(this.source(this.index + 1))) {
          // "?." in "foo?.3:0" should not be treated as optional chaining.
          // See https://github.com/tc39/proposal-optional-chaining#notes
          this.index += 1
          str = "?."
        }
      case ")" | ";" | "," | "[" | "]" | ":" | "~" =>
        this.index += 1
      case _ =>
        // 4-character punctuator.
        str = this.source.substr(this.index, 4)
        if (str == ">>>=") {
          this.index += 4
        } else {
          // 3-character punctuators.
          str = str.substr(0, 3)
          if (str == "===" || str == "!==" || str == ">>>" || str == "<<=" || str == ">>=" || str == "**=") {
            this.index += 3
          } else {
            // 2-character punctuators.
            str = str.substr(0, 2)
            if (str == "&&" || str == "||" || str == "??" || str == "==" || str == "!=" || str == "+=" || str == "-=" || str == "*=" || str == "/=" || str == "++" || str == "--" || str == "<<" || str == ">>" || str == "&=" || str == "|=" || str == "^=" || str == "%=" || str == "<=" || str == ">=" || str == "=>" || str == "**") {
              this.index += 2
            } else {
              // 1-character punctuators.
              str = this.source(this.index).toString
              if ("<>=!+-*%&|^/".indexOf(str) >= 0) {
                this.index += 1
              }
            }
          }
        }
    }
    if (this.index == start) {
      this.throwUnexpectedToken()
    }
    val start_ = start
    new RawToken {
      `type` = Token.Punctuator
      override val value = OrType(str)
      override val lineNumber = self.lineNumber
      override val lineStart = self.lineStart
      override val start = start_
      override val end = self.index
    }
  }
  
  def scanHexLiteral(start: Int): RawToken = {
    var num = ""
    breakable {
      while (!this.eof()) {
        if (!Character.isHexDigit(this.source.charCodeAt(this.index))) {
          break()
        }
        num += this.source({
          val temp = this.index
          this.index += 1
          temp
        })
      }
    }
    if (num.length == 0) {
      this.throwUnexpectedToken()
    }
    if (Character.isIdentifierStart(this.source.charCodeAt(this.index))) {
      this.throwUnexpectedToken()
    }
    val start_ = start
    new RawToken {
      `type` = Token.NumericLiteral
      override val value = OrType(parseInt(num, 16).toDouble)
      override val lineNumber = self.lineNumber
      override val lineStart = self.lineStart
      override val start = start_
      override val end = self.index
    }
  }
  
  def scanBinaryLiteral(start: Int): RawToken = {
    var num = ""
    var ch: CharValue = null
    breakable {
      while (!this.eof()) {
        ch = this.source(this.index)
        if (ch != "0" && ch != "1") {
          break()
        }
        num += this.source({
          val temp = this.index
          this.index += 1
          temp
        })
      }
    }
    if (num.length == 0) {
      // only 0b or 0B
      this.throwUnexpectedToken()
    }
    if (!this.eof()) {
      ch = this.source.charCodeAt(this.index)
      /*istanbul ignore else */
      if (Character.isIdentifierStart(ch) || Character.isDecimalDigit(ch)) {
        this.throwUnexpectedToken()
      }
    }
    val start_ = start
    new RawToken {
      `type` = Token.NumericLiteral
      override val value = OrType(parseInt(num, 2).toDouble)
      override val lineNumber = self.lineNumber
      override val lineStart = self.lineStart
      override val start = start_
      override val end = self.index
    }
  }
  
  def scanOctalLiteral(prefix: String, start: Int): RawToken = {
    var num = ""
    var octal = false
    if (Character.isOctalDigit(prefix.charCodeAt(0))) {
      octal = true
      num = "0" + this.source({
        val temp = this.index
        this.index += 1
        temp
      })
    } else {
      this.index += 1
    }
    breakable {
      while (!this.eof()) {
        if (!Character.isOctalDigit(this.source.charCodeAt(this.index))) {
          break()
        }
        num += this.source({
          val temp = this.index
          this.index += 1
          temp
        })
      }
    }
    if (!octal && num.length == 0) {
      // only 0o or 0O
      this.throwUnexpectedToken()
    }
    if (Character.isIdentifierStart(this.source.charCodeAt(this.index)) || Character.isDecimalDigit(this.source.charCodeAt(this.index))) {
      this.throwUnexpectedToken()
    }
    val start_ = start
    val octal_ = octal
    new RawToken {
      `type` = Token.NumericLiteral
      override val value = OrType(parseInt(num, 8).toDouble)
      override val octal = octal_
      override val lineNumber = self.lineNumber
      override val lineStart = self.lineStart
      override val start = start_
      override val end = self.index
    }
  }
  
  def isImplicitOctalLiteral(): Boolean = {
    // Implicit octal, unless there is a non-octal digit.
    // (Annex B.1.1 on Numeric Literals)
    for (i <- this.index + 1 until this.length) {
      val ch: String = this.source(i)
      if (ch == "8" || ch == "9") {
        return false
      }
      if (!Character.isOctalDigit(ch.charCodeAt(0))) {
        return true
      }
    }
    true
  }
  
  def scanNumericLiteral(): RawToken = {
    val start = this.index
    var ch: String = this.source(start)
    assert(Character.isDecimalDigit(ch.charCodeAt(0)) || ch == ".", "Numeric literal must start with a decimal digit or a decimal point")
    var num = ""
    if (ch != ".") {
      num = this.source({
        val temp = this.index
        this.index += 1
        temp
      })
      ch = this.source(this.index)
      // Hex number starts with '0x'.
      // Octal number starts with '0'.
      // Octal number in ES6 starts with '0o'.
      // Binary number in ES6 starts with '0b'.
      if (num == "0") {
        if (ch == "x" || ch == "X") {
          this.index += 1
          return this.scanHexLiteral(start)
        }
        if (ch == "b" || ch == "B") {
          this.index += 1
          return this.scanBinaryLiteral(start)
        }
        if (ch == "o" || ch == "O") {
          return this.scanOctalLiteral(ch, start)
        }
        if (ch && Character.isOctalDigit(ch.charCodeAt(0))) {
          if (this.isImplicitOctalLiteral()) {
            return this.scanOctalLiteral(ch, start)
          }
        }
      }
      while (Character.isDecimalDigit(this.source.charCodeAt(this.index))) {
        num += this.source.getChar({
          val temp = this.index
          this.index += 1
          temp
        })
      }
      ch = this.source.getChar(this.index)
    }
    if (ch == ".") {
      num += this.source({
        val temp = this.index
        this.index += 1
        temp
      })
      while (Character.isDecimalDigit(this.source.charCodeAt(this.index))) {
        num += this.source({
          val temp = this.index
          this.index += 1
          temp
        })
      }
      ch = this.source(this.index)
    }
    if (ch == "e" || ch == "E") {
      num += this.source({
        val temp = this.index
        this.index += 1
        temp
      })
      ch = this.source(this.index)
      if (ch == "+" || ch == "-") {
        num += this.source({
          val temp = this.index
          this.index += 1
          temp
        })
      }
      if (Character.isDecimalDigit(this.source.charCodeAt(this.index))) {
        while (Character.isDecimalDigit(this.source.charCodeAt(this.index))) {
          num += this.source({
            val temp = this.index
            this.index += 1
            temp
          })
        }
      } else {
        this.throwUnexpectedToken()
      }
    }
    if (Character.isIdentifierStart(this.source.charCodeAt(this.index))) {
      this.throwUnexpectedToken()
    }
    val start_ = start
    new RawToken {
      import OrType._
      `type` = Token.NumericLiteral
      override val value = OrType(parseFloat(num))
      override val lineNumber = self.lineNumber
      override val lineStart = self.lineStart
      override val start = start_
      override val end = self.index
    }
  }

  def scanStringLiteral(): RawToken = {
    val start = this.index
    var quote: String = this.source(start)
    assert(quote == "\'" || quote == "\"", "String literal must starts with a quote")
    this.index += 1
    var octal = false
    var str = ""
    val brk = Breakable()
    while (!this.eof() && !brk.broken) {
      var ch: String = this.source({
        val temp = this.index
        this.index += 1
        temp
      })
      if (ch == quote) {
        quote = ""
        brk.break()
      } else if (ch == "\\") {
        ch = this.source({
          val temp = this.index
          this.index += 1
          temp
        })
        if (!ch || !Character.isLineTerminator(ch.charCodeAt(0))) {
          ch match {
            case "u" =>
              if (this.source(this.index).toString == "{") {
                this.index += 1
                str += this.scanUnicodeCodePointEscape()
              } else {
                val unescapedChar = this.scanHexEscape(ch)
                if (unescapedChar == null) {
                  this.throwUnexpectedToken()
                }
                str += unescapedChar
              }
            case "x" =>
              val unescaped = this.scanHexEscape(ch)
              if (unescaped == null) {
                this.throwUnexpectedToken(Messages.InvalidHexEscapeSequence)
              }
              str += unescaped
            case "n" =>
              str += "\n"
            case "r" =>
              str += "\r"
            case "t" =>
              str += "\t"
            case "b" =>
              str += "\b"
            case "f" =>
              str += "\f"
            case "v" =>
              str += "\u000b"
            case "8" | "9" =>
              str += ch
              this.tolerateUnexpectedToken()
            case _ =>
              if (ch && Character.isOctalDigit(ch.charCodeAt(0))) {
                val octToDec = this.octalToDecimal(ch)
                octal = octToDec._2 || octal
                str += fromCharCode(octToDec._1)
              } else {
                str += ch
              }
          }
        } else {
          this.lineNumber += 1
          if (ch == "\r" && this.source(this.index).toString == "\n") {
            this.index += 1
          }
          this.lineStart = this.index
        }
      } else if (Character.isLineTerminator(ch.charCodeAt(0))) {
        brk.break()
      } else {
        str += ch
      }
    }
    if (quote != "") {
      this.index = start
      this.throwUnexpectedToken()
    }
    val start_ = start
    val octal_ = octal
    new RawToken {
      `type` = Token.StringLiteral
      override val value = OrType(str)
      override val octal = octal_
      override val lineNumber = self.lineNumber
      override val lineStart = self.lineStart
      override val start = start_
      override val end = self.index
    }
  }

  def scanTemplate(): RawToken = {
    var cooked = ""
    var terminated = false
    val start = this.index
    val head = this.source(start).toString == "`"
    var tail = false
    var notEscapeSequenceHead: String = null
    var rawOffset = 2
    this.index += 1
    breakable {
      while (!this.eof()) {
        var ch: String = this.source({
          val temp = this.index
          this.index += 1
          temp
        })
        if (ch == "`") {
          rawOffset = 1
          tail = true
          terminated = true
          break()
        } else if (ch == "$") {
          if (this.source(this.index).toString == "{") {
            this.curlyStack.push("${")
            this.index += 1
            terminated = true
            break()
          }
          cooked += ch
        } else if (notEscapeSequenceHead != null) {
          /* Unsupported: Continue */ ???
        } else if (ch == "\\") {
          ch = this.source({
            val temp = this.index
            this.index += 1
            temp
          })
          if (!Character.isLineTerminator(ch.charCodeAt(0))) {
            ch match {
              case "n" =>
                cooked += "\n"
              case "r" =>
                cooked += "\r"
              case "t" =>
                cooked += "\t"
              case "u" =>
                if (this.source(this.index).toString == "{") {
                  this.index += 1
                  val unicodeCodePointEscape = this.tryToScanUnicodeCodePointEscape()
                if (unicodeCodePointEscape == null) {
                  notEscapeSequenceHead = "u"
                } else {
                  cooked += unicodeCodePointEscape
                }
              } else {
                  val unescapedChar = this.scanHexEscape(ch)
                  if (unescapedChar == null) {
                    notEscapeSequenceHead = "u"
                  } else {
                    cooked += unescapedChar
                  }
                }
              case "x" =>
                val unescaped = this.scanHexEscape(ch)
                if (unescaped == null) {
                  notEscapeSequenceHead = "x"
                }
                cooked += unescaped
              case "b" =>
                cooked += "\b"
              case "f" =>
                cooked += "\f"
              case "v" =>
                cooked += "\u000b"
              case _ =>
                if (ch == "0") {
                  if (Character.isDecimalDigit(this.source.charCodeAt(this.index))) {
                    // NotEscapeSequence: \01 \02 and so on
                    notEscapeSequenceHead = "0"
                  }else {
                  cooked += "\u0000"}
                } else if (Character.isDecimalDigitChar(ch)) {
                  // NotEscapeSequence: \1 \2
                  notEscapeSequenceHead = ch
                } else {
                  cooked += ch
                }
            }
          } else {
            this.lineNumber += 1
            if (ch == "\r" && this.source(this.index).toString == "\n") {
              this.index += 1
            }
            this.lineStart = this.index
          }
        } else if (Character.isLineTerminator(ch.charCodeAt(0))) {
          this.lineNumber += 1
          if (ch == "\r" && this.source(this.index).toString == "\n") {
            this.index += 1
          }
          this.lineStart = this.index
          cooked += "\n"
        } else {
          cooked += ch
        }
      }
    }
    if (!terminated) {
      this.throwUnexpectedToken()
    }
    if (!head) {
      this.curlyStack.pop()
    }
    val start_ = start
    val head_ = head
    val tail_ = tail
    val cooked_ = cooked
    val notEscapeSequenceHead_ = notEscapeSequenceHead
    new RawToken {
      `type` = Token.Template
      override val value = OrType(self.source.slice(start_ + 1, self.index - rawOffset))
      override val cooked = cooked_
      override val head = head_
      override val tail = tail_
      override val notEscapeSequenceHead = notEscapeSequenceHead_
      override val lineNumber = self.lineNumber
      override val lineStart = self.lineStart
      override val start = start_
      override val end = self.index
    }
  }

  // https://tc39.github.io/ecma262/#sec-literals-regular-expression-literals  def testRegExp(pattern: String, flags: String): RegExp = {
  def testRegExp(pattern: String, flags: String): RegExp = {
    // The BMP character to use as a replacement for astral symbols when
    // translating an ES6 "u"-flagged pattern to an ES5-compatible
    // approximation.
    // Note: replacing with '\uFFFF' enables false positives in unlikely
    // scenarios. For example, `[\ u{1044f}-\ u{10440}]` is an invalid
    // pattern that would not be detected by this substitution.
    val astralSubstitute = "￿"
    var tmp = pattern
    if (flags.indexOf("u") >= 0) {
      tmp = """\\u\{([0-9a-fA-F]+)\}|\\u([a-fA-F0-9]{4})""".r.replaceAllIn(tmp, m => {
        val $1 = m.group(1)
        val $2 = m.group(2)
        val codePoint = parseInt($1 || $2, 16)
        if (codePoint > 0x10FFFF) {
          this.throwUnexpectedToken(Messages.InvalidRegExp)
        }
        if (codePoint <= 0xFFFF) {
          return new RegExp(fromCharCode(codePoint))
        }
        astralSubstitute
      }
      ).replace("[\uD800-\uDBFF][\uDC00-\uDFFF]", astralSubstitute)
    }
    // First, detect invalid regular expressions.
    try {
      RegExp(tmp)
    } catch {
      case e: Exception =>
        this.throwUnexpectedToken(Messages.InvalidRegExp)
    }
    // Return a regular expression object for this pattern-flag pair, or
    // `null` in case the current environment doesn't support the flags it
    // uses.
    try {
      new RegExp(pattern, flags)
    } catch {
      case exception: Exception =>
        /*istanbul ignore next */
        return null
    }
  }
  
  def scanRegExpBody(): String = {
    var ch: String = this.source(this.index)
    assert(ch == "/", "Regular expression literal must start with a slash")
    var str: String = this.source({
      val temp = this.index
      this.index += 1
      temp
    })
    var classMarker = false
    var terminated = false
    breakable {
      while (!this.eof()) {
        ch = this.source({
          val temp = this.index
          this.index += 1
          temp
        })
        str += ch
        if (ch == "\\") {
          ch = this.source({
            val temp = this.index
            this.index += 1
            temp
          })
          // https://tc39.github.io/ecma262/#sec-literals-regular-expression-literals
          if (Character.isLineTerminator(ch.charCodeAt(0))) {
            this.throwUnexpectedToken(Messages.UnterminatedRegExp)
          }
          str += ch
        } else if (Character.isLineTerminator(ch.charCodeAt(0))) {
          this.throwUnexpectedToken(Messages.UnterminatedRegExp)
        } else if (classMarker) {
          if (ch == "]") {
            classMarker = false
          }
        } else {
          if (ch == "/") {
            terminated = true
            break()
          } else if (ch == "[") {
            classMarker = true
          }
        }
      }
    }
    if (!terminated) {
      this.throwUnexpectedToken(Messages.UnterminatedRegExp)
    }
    // Exclude leading and trailing slash.
    str.substr(1, str.length - 2)
  }
  
  def scanRegExpFlags(): String = {
    var str = ""
    var flags = ""
    breakable {
      while (!this.eof()) {
        var ch: String = this.source(this.index)
        if (!Character.isIdentifierPart(ch.charCodeAt(0))) {
          break()
        }
        this.index += 1
        if (ch == "\\" && !this.eof()) {
          ch = this.source(this.index)
          if (ch == "u") {
            this.index += 1
            var restore = this.index
            val char = this.scanHexEscape("u")
            if (char != null) {
              flags += char
              str += "\\u"
              while (restore < this.index) {
                {
                  str += this.source(restore)
                }
                restore += 1
              }
            } else {
              this.index = restore
              flags += "u"
              str += "\\u"
            }
            this.tolerateUnexpectedToken()
          } else {
            str += "\\"
            this.tolerateUnexpectedToken()
          }
        } else {
          flags += ch
          str += ch
        }
      }
    }
    flags
  }
  
  def scanRegExp(): RawToken = {
    val start_ = this.index
    val pattern_ = this.scanRegExpBody()
    val flags_ = this.scanRegExpFlags()
    val value_ = this.testRegExp(pattern_, flags_)
    new RawToken {
      import OrType._
      `type` = Token.RegularExpression
      override val value = OrType("")
      override val pattern = pattern_
      override val flags = flags_
      override val regex = value_
      override val lineNumber = self.lineNumber
      override val lineStart = self.lineStart
      override val start = start_
      override val end = self.index
    }
  }
  
  def lex(): RawToken = {
    if (this.eof()) {
      return new RawToken {
        import OrType._
        `type` = Token.EOF
        override val value = OrType("")
        override val lineNumber = self.lineNumber
        override val lineStart = self.lineStart
        override val start = self.index
        override val end = self.index
      }
    }
    val cp = this.source.charCodeAt(this.index)
    if (Character.isIdentifierStart(cp)) {
      return this.scanIdentifier()
    }
    // Very common: ( and ) and ;
    if (cp == 0x28 || cp == 0x29 || cp == 0x3B) {
      return this.scanPunctuator()
    }
    // String literal starts with single quote (U+0027) or double quote (U+0022).
    if (cp == 0x27 || cp == 0x22) {
      return this.scanStringLiteral()
    }
    // Dot (.) U+002E can also start a floating-point number, hence the need
    // to check the next character.
    if (cp == 0x2E) {
      if (Character.isDecimalDigit(this.source.charCodeAt(this.index + 1))) {
        return this.scanNumericLiteral()
      }
      return this.scanPunctuator()
    }
    if (Character.isDecimalDigit(cp)) {
      return this.scanNumericLiteral()
    }
    // Template literals start with ` (U+0060) for template head
    // or } (U+007D) for template middle or template tail.
    if (cp == 0x60 || cp == 0x7D && this.curlyStack.nonEmpty && this.curlyStack(this.curlyStack.length - 1) == "${") {
      return this.scanTemplate()
    }
    // Possible identifier start in a surrogate pair.
    if (cp >= 0xD800 && cp < 0xDFFF) {
      if (Character.isIdentifierStart(this.codePointAt(this.index))) {
        return this.scanIdentifier()
      }
    }
    this.scanPunctuator()
  }
  
}



/*
ScalaFromJS: 2017-12-06 21:28:23.723
scanner.js
*/

package esprima
import port.RegExp
import Scanner._

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.util.control.Breaks._
import Token._

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
    def line: Int
    def column: Int
    def offset: Int = ???
  }

  trait Metadata {
    var start: Position
    var end: Position
  }

  trait SourceLocation {
    var start: Position
    var end: Position
    var source: String = _
  }

  trait Comment {
    def multiLine: Boolean = ???
    def slice: (Int, Int) = ???
    def range: (Int, Int)
    def loc: SourceLocation
  }

  type Token = Token.Token

  trait RawToken {
    override def toString = `type`.toString + "'" + value.toString

    var `type`: Token = _
    def value: OrType  = ??? // String | Int
    def pattern: String = ??? // UndefOr
    def flags: String = ??? // UndefOr
    def regex: RegExp = ??? // UndefOr
    def octal: Boolean = false // UndefOr
    def cooked: String = ??? // UndefOr
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
    }
  }
  
  def restoreState(state: ScannerState) = {
    this.index = state.index
    this.lineNumber = state.lineNumber
    this.lineStart = state.lineStart
  }
  
  def eof() = {
    this.index >= this.length
  }
  
  def throwUnexpectedToken(message: String = Messages.UnexpectedTokenIllegal) = {
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
      loc = new SourceLocation {
        var start: Position = new Position {
          var line = self.lineNumber
          var column = self.index - self.lineStart - offset
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
            override def line = self.lineNumber
            override def column = self.index - self.lineStart - 1
          }
          val loc_ = loc
          object entry extends Comment {
            override def multiLine = false
            override def slice = (start + offset, self.index - 1)
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
        override def line = self.lineNumber
        override def column = self.index - self.lineStart
      }
      val loc_ = loc
      object entry extends Comment {
        override def multiLine = false
        override def slice = (start + offset, self.index)
        var range = (start, self.index)
        var loc = loc_
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
          override def line = self.lineNumber
          override def column = self.index - self.lineStart - 2
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
              override def line = self.lineNumber
              override def column = self.index - self.lineStart
            }
            val loc_ = loc
            object entry extends Comment {
              override def multiLine = true
              override def slice = (start + 2, self.index - 2)
              def range = (start, self.index)
              def loc = loc_
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
        override def line = self.lineNumber
        override def column = self.index - self.lineStart
      }
      val loc_ = loc
      object entry extends Comment {
        override def multiLine = true
        override def slice = (start + 2, self.index)
        def range = (start, self.index)
        def loc = loc_
      }
      comments.push(entry)
    }
    this.tolerateUnexpectedToken()
    comments
  }
  
  def scanComments() = {
    var comments: ArrayBuffer[Comment] = null
    if (this.trackComment) {
      comments = ArrayBuffer()
    }
    var start = this.index == 0
    breakable {
      while (!this.eof()) {
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
        } else if (ch == 0x2F) {
          ch = this.source.charCodeAt(this.index + 1)
          if (ch == 0x2F) {
            this.index += 2
            val comment = this.skipSingleLineComment(2)
            if (this.trackComment) {
              comments = comments.concat(comment)
            }
            start = true
          } else if (ch == 0x2A) {
            this.index += 2
            val comment = this.skipMultiLineComment()
            if (this.trackComment) {
              comments = comments.concat(comment)
            }
          } else {
            break
          }
        } else if (start && ch == 0x2D) {
          // U+003E is '>'
          if (this.source.charCodeAt(this.index + 1) == 0x2D && this.source.charCodeAt(this.index + 2) == 0x3E) {
            // '-->' is a single-line comment
            this.index += 3
            val comment = this.skipSingleLineComment(3)
            if (this.trackComment) {
              comments = comments.concat(comment)
            }
          } else {
            break
          }
        } else if (ch == 0x3C && !this.isModule) {
          if (this.source.slice(this.index + 1, this.index + 4) == "!--") {
            this.index += 4
            // `<!--`
            val comment = this.skipSingleLineComment(4)
            if (this.trackComment) {
              comments = comments.concat(comment)
            }
          } else {
            break
          }
        } else {
          break
        }
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
  
  def isRestrictedWord(id: String) = {
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
  
  def codePointAt(i: Int) = {
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
  
  def scanUnicodeCodePointEscape(): String = {
    var ch = this.source(this.index)
    var code = 0
    // At least, one hex digit is required.
    if (ch.toString == "}") {
      this.throwUnexpectedToken()
    }
    breakable {
      while (!this.eof()) {
        ch = this.source({
          val temp = this.index
          this.index += 1
          temp
        })
        if (!Character.isHexDigit(ch)) {
          break
        }
        code = code * 16 + hexValue(ch)
      }
    }
    if (code > 0x10FFFF || ch.toString != "}") {
      this.throwUnexpectedToken()
    }
    Character.fromCodePoint(code)
  }
  
  def getIdentifier(): String = {
    val start = {
      val temp = this.index
      this.index += 1
      temp
    }
    breakable {
      while (!this.eof()) {
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
          break
        }
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
          break
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
  
  def scanIdentifier() = {
    var `type`: Token = Token.Undefined
    val start = this.index
    // Backslash (U+005C) starts an escaped character.
    val id = if (this.source.charCodeAt(start) == 0x5C) this.getComplexIdentifier() else this.getIdentifier()
    // There is no keyword or literal with only one character.
    // Thus, it must be an identifier.
    if (id.length == 1) {
      `type` = Identifier
    } else if (this.isKeyword(id)) {
      `type` = Keyword
    } else if (id == "null") {
      `type` = NullLiteral
    } else if (id == "true" || id == "false") {
      `type` = BooleanLiteral
    } else {
      `type` = Identifier
    }
    if (`type` != 3 && start + id.length != this.index) {
      val restore = this.index
      this.index = start
      this.tolerateUnexpectedToken(Messages.InvalidEscapedReservedWord)
      this.index = restore
    }
    val _type = `type`
    val _start = start
    new RawToken {
      `type` = `_type`
      override def value = id
      override def lineNumber = self.lineNumber
      override def lineStart = self.lineStart
      override def start = _start
      override def end = self.index
    }
  }
  
  def scanPunctuator() = {
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
        this.curlyStack.pop()
      case ")" | ";" | "," | "[" | "]" | ":" | "?" | "~" =>
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
            if (str == "&&" || str == "||" || str == "==" || str == "!=" || str == "+=" || str == "-=" || str == "*=" || str == "/=" || str == "++" || str == "--" || str == "<<" || str == ">>" || str == "&=" || str == "|=" || str == "^=" || str == "%=" || str == "<=" || str == ">=" || str == "=>" || str == "**") {
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
      `type` = Punctuator
      override def value = str
      override def lineNumber = self.lineNumber
      override def lineStart = self.lineStart
      override def start = start_
      override def end = self.index
    }
  }
  
  def scanHexLiteral(start: Int) = {
    var num = ""
    breakable {
      while (!this.eof()) {
        if (!Character.isHexDigit(this.source.charCodeAt(this.index))) {
          break
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
      `type` = NumericLiteral
      override def value = parseInt(num, 16).toDouble
      override def lineNumber = self.lineNumber
      override def lineStart = self.lineStart
      override def start = start_
      override def end = self.index
    }
  }
  
  def scanBinaryLiteral(start: Int) = {
    var num = ""
    var ch: CharValue = null
    breakable {
      while (!this.eof()) {
        ch = this.source(this.index)
        if (ch != "0" && ch != "1") {
          break
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
      `type` = NumericLiteral
      override def value = parseInt(num, 2).toDouble
      override def lineNumber = self.lineNumber
      override def lineStart = self.lineStart
      override def start = start_
      override def end = self.index
    }
  }
  
  def scanOctalLiteral(prefix: String, start: Int) = {
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
          break
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
      `type` = NumericLiteral
      override def value = parseInt(num, 8).toDouble
      override def octal = octal_
      override def lineNumber = self.lineNumber
      override def lineStart = self.lineStart
      override def start = start_
      override def end = self.index
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
      `type` = NumericLiteral
      override def value = parseFloat(num)
      override def lineNumber = self.lineNumber
      override def lineStart = self.lineStart
      override def start = start_
      override def end = self.index
    }
  }
  
  def scanStringLiteral(): RawToken = {
    val start = this.index
    var quote: String = this.source(start)
    assert(quote == "\'" || quote == "\"", "String literal must starts with a quote")
    this.index += 1
    var octal = false
    var str = ""
    breakable {
      while (!this.eof()) {
        var ch: String = this.source({
          val temp = this.index
          this.index += 1
          temp
        })
        if (ch == quote) {
          quote = ""
          break
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
                  val unescaped = this.scanHexEscape(ch)
                  if (unescaped == null) {
                    this.throwUnexpectedToken()
                  }
                  str += unescaped
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
                str += "\013"
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
          break
        } else {
          str += ch
        }
      }
    }
    if (quote != "") {
      this.index = start
      this.throwUnexpectedToken()
    }
    val start_ = start
    val octal_ = octal
    new RawToken {
      `type` = StringLiteral
      override def value = str
      override def octal = octal_
      override def lineNumber = self.lineNumber
      override def lineStart = self.lineStart
      override def start = start_
      override def end = self.index
    }
  }
  
  def scanTemplate(): RawToken = {
    var cooked = ""
    var terminated = false
    val start = this.index
    val head = this.source(start).toString == "`"
    var tail = false
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
          break
        } else if (ch == "$") {
          if (this.source(this.index).toString == "{") {
            this.curlyStack.push("${")
            this.index += 1
            terminated = true
            break
          }
          cooked += ch
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
                  cooked += this.scanUnicodeCodePointEscape()
                } else {
                  val restore = this.index
                  val unescaped = this.scanHexEscape(ch)
                  if (unescaped != null) {
                    cooked += unescaped
                  } else {
                    this.index = restore
                    cooked += ch
                  }
                }
              case "x" =>
                val unescaped = this.scanHexEscape(ch)
                if (unescaped == null) {
                  this.throwUnexpectedToken(Messages.InvalidHexEscapeSequence)
                }
                cooked += unescaped
              case "b" =>
                cooked += "\b"
              case "f" =>
                cooked += "\f"
              case "v" =>
                cooked += "\013"
              case _ =>
                if (ch == "0") {
                  if (Character.isDecimalDigit(this.source.charCodeAt(this.index))) {
                    // Illegal: \01 \02 and so on
                    this.throwUnexpectedToken(Messages.TemplateOctalLiteral)
                  }
                  cooked += "\00"
                } else if (Character.isOctalDigit(ch.charCodeAt(0))) {
                  // Illegal: \1 \2
                  this.throwUnexpectedToken(Messages.TemplateOctalLiteral)
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
    new RawToken {
      `type` = Template
      override def value = self.source.slice(start + 1, self.index - rawOffset)
      override def cooked = cooked_
      override def head = head_
      override def tail = tail_
      override def lineNumber = self.lineNumber
      override def lineStart = self.lineStart
      override def start = start_
      override def end = self.index
    }
  }
  
  def testRegExp(pattern: String, flags: String): RegExp = {
    // The BMP character to use as a replacement for astral symbols when
    // translating an ES6 "u"-flagged pattern to an ES5-compatible
    // approximation.
    // Note: replacing with '\uFFFF' enables false positives in unlikely
    // scenarios. For example, `[\ u{1044f}-\ u{10440}]` is an invalid
    // pattern that would not be detected by this substitution.
    val astralSubstitute = "￿"
    var tmp = pattern
    val self = this
    if (flags.indexOf("u") >= 0) {
      tmp = """\\u\{([0-9a-fA-F]+)\}|\\u([a-fA-F0-9]{4})""".r.replaceAllIn(tmp, m => {
        val $1 = m.group(1)
        val $2 = m.group(2)
        val codePoint = parseInt($1 || $2, 16)
        if (codePoint > 0x10FFFF) {
          self.throwUnexpectedToken(Messages.InvalidRegExp)
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
  
  def scanRegExpBody() = {
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
            break
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
  
  def scanRegExpFlags() = {
    var str = ""
    var flags = ""
    breakable {
      while (!this.eof()) {
        var ch: String = this.source(this.index)
        if (!Character.isIdentifierPart(ch.charCodeAt(0))) {
          break
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
  
  def scanRegExp() = {
    val start_ = this.index
    val pattern_ = this.scanRegExpBody()
    val flags_ = this.scanRegExpFlags()
    val value_ = this.testRegExp(pattern_, flags_)
    new RawToken {
      import OrType._
      `type` = RegularExpression
      override def value = ""
      override def pattern = pattern_
      override def flags = flags_
      override def regex = value_
      override def lineNumber = self.lineNumber
      override def lineStart = self.lineStart
      override def start = start_
      override def end = self.index
    }
  }
  
  def lex(): RawToken = {
    if (this.eof()) {
      return new RawToken {
        import OrType._
        `type` = EOF
        override def value = ""
        override def lineNumber = self.lineNumber
        override def lineStart = self.lineStart
        override def start = self.index
        override def end = self.index
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
    if (cp == 0x60 || cp == 0x7D && this.curlyStack(this.curlyStack.length - 1) == "${") {
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



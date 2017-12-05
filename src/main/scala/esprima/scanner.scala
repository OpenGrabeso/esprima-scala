/*
ScalaFromJS: 2017-12-05 14:48:54.460
scanner.js
*/

package esprima

import Scanner._
object Scanner {
  def hexValue(ch: String) = {
    "0123456789abcdef".indexOf(ch.toLowerCase())
  }

  def octalValue(ch: String) = {
    "01234567".indexOf(ch)
  }
}

class Scanner(code: String, var errorHandler: ErrorHandler) {
  self =>
  var source = code
  var trackComment: Boolean = false
  var isModule: Boolean = false
  var length: Int = code.length
  var index: Int = 0
  var lineNumber: Int = if (code.length > 0) 1 else 0
  var lineStart: Int = 0
  var curlyStack = Array.empty[String]
  def saveState() = {
    new {
      var index = this.index
      var lineNumber = this.lineNumber
      var lineStart = this.lineStart
    }
  }
  
  def restoreState(state: Scanner) = {
    this.index = state.index
    this.lineNumber = state.lineNumber
    this.lineStart = state.lineStart
  }
  
  def eof() = {
    this.index >= this.length
  }
  
  def throwUnexpectedToken(message: Any = Messages.UnexpectedTokenIllegal) = {
    this.errorHandler.throwError(this.index, this.lineNumber, this.index - this.lineStart + 1, message)
  }
  
  def tolerateUnexpectedToken(message: Any = Messages.UnexpectedTokenIllegal) = {
    this.errorHandler.tolerateError(this.index, this.lineNumber, this.index - this.lineStart + 1, message)
  }
  
  def skipSingleLineComment(offset: Int) = {
    var comments = Array.empty[Any]
    var start: Double = _
    var loc = new {}
    if (this.trackComment) {
      comments = Array()
      start = this.index - offset
      loc = new {
        var start = new {
          var line = this.lineNumber
          var column = this.index - this.lineStart - offset
        }
        var end = new {}
      }
    }
    while (!this.eof()) {
      val ch = this.source.charCodeAt(this.index)
      this.index += 1
      if (Character.isLineTerminator(ch)) {
        if (this.trackComment) {
          loc.end = new {
            var line = this.lineNumber
            var column = this.index - this.lineStart - 1
          }
          object entry {
            var multiLine = false
            var slice = Array(start + offset, this.index - 1)
            var range = Array(start, this.index - 1)
            var loc = loc
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
      loc.end = new {
        var line = this.lineNumber
        var column = this.index - this.lineStart
      }
      object entry {
        var multiLine = false
        var slice = Array(start + offset, this.index)
        var range = Array(start, this.index)
        var loc = loc
      }
      comments.push(entry)
    }
    comments
  }
  
  def skipMultiLineComment() = {
    var comments = Array.empty[Any]
    var start: Double = _
    var loc = new {}
    if (this.trackComment) {
      comments = Array()
      start = this.index - 2
      loc = new {
        var start = new {
          var line = self.lineNumber
          var column = self.index - self.lineStart - 2
        }
        var end = new {}
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
            loc.end = new {
              var line = self.lineNumber
              var column = self.index - self.lineStart
            }
            object entry {
              var multiLine = true
              var slice = Array(start + 2, self.index - 2)
              var range = Array(start, self.index)
              var loc = loc
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
      loc.end = new {
        var line = self.lineNumber
        var column = self.index - self.lineStart
      }
      object entry {
        var multiLine = true
        var slice = Array(start + 2, self.index)
        var range = Array(start, self.index)
        var loc = loc
      }
      comments.push(entry)
    }
    this.tolerateUnexpectedToken()
    comments
  }
  
  def scanComments() = {
    var comments = Array.empty[Unit]
    if (this.trackComment) {
      comments = Array()
    }
    var start = this.index == 0
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
          /* Unsupported: Break */ break;
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
          /* Unsupported: Break */ break;
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
          /* Unsupported: Break */ break;
        }
      } else {
        /* Unsupported: Break */ break;
      }
    }
    comments
  }
  
  def isFutureReservedWord(id: String) = {
    id match {
      case "enum" | "export" | "import" | "super" =>
        return true
      case _ =>
        false
    }
  }
  
  def isStrictModeReservedWord(id: String) = {
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
  
  def isKeyword(id: String) = {
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
  
  def codePointAt(i: Double) = {
    var cp = this.source.charCodeAt(i)
    if (cp >= 0xD800 && cp <= 0xDBFF) {
      val second = this.source.charCodeAt(i + 1)
      if (second >= 0xDC00 && second <= 0xDFFF) {
        val first = cp
        cp = (first - 0xD800) * 0x400 + second - 0xDC00 + 0x10000
      }
    }
    cp
  }
  
  def scanHexEscape(prefix: String) = {
    val len = if (prefix == "u") 4 else 2
    var code = 0
    for (i <- 0 until len) {
      if (!this.eof() && Character.isHexDigit(this.source.charCodeAt(this.index))) {
        code = code * 16 + hexValue(this.source({
          val temp = this.index
          this.index += 1
          temp
        }))
      } else {
        return null
      }
    }
    String.fromCharCode(code)
  }
  
  def scanUnicodeCodePointEscape() = {
    var ch = this.source(this.index)
    var code = 0
    // At least, one hex digit is required.
    if (ch == "}") {
      this.throwUnexpectedToken()
    }
    while (!this.eof()) {
      ch = this.source({
        val temp = this.index
        this.index += 1
        temp
      })
      if (!Character.isHexDigit(ch.charCodeAt(0))) {
        /* Unsupported: Break */ break;
      }
      code = code * 16 + hexValue(ch)
    }
    if (code > 0x10FFFF || ch != "}") {
      this.throwUnexpectedToken()
    }
    Character.fromCodePoint(code)
  }
  
  def getIdentifier() = {
    val start = {
      val temp = this.index
      this.index += 1
      temp
    }
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
        /* Unsupported: Break */ break;
      }
    }
    this.source.slice(start, this.index)
  }
  
  def getComplexIdentifier() = {
    var cp = this.codePointAt(this.index)
    var id = Character.fromCodePoint(cp)
    this.index += id.length
    // '\u' (U+005C, U+0075) denotes an escaped character.
    var ch: String = _
    if (cp == 0x5C) {
      if (this.source.charCodeAt(this.index) != 0x75) {
        this.throwUnexpectedToken()
      }
      this.index += 1
      if (this.source(this.index) == "{") {
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
    while (!this.eof()) {
      cp = this.codePointAt(this.index)
      if (!Character.isIdentifierPart(cp)) {
        /* Unsupported: Break */ break;
      }
      ch = Character.fromCodePoint(cp)
      id += ch
      this.index += ch.length
      // '\u' (U+005C, U+0075) denotes an escaped character.
      if (cp == 0x5C) {
        id = id.substr(0, id.length - 1)
        if (this.source.charCodeAt(this.index) != 0x75) {
          this.throwUnexpectedToken()
        }
        this.index += 1
        if (this.source(this.index) == "{") {
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
    id
  }
  
  def octalToDecimal(ch: String) = {
    // \0 is not octal escape sequence
    var octal = ch != "0"
    var code = octalValue(ch)
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
    new {
      var code = code
      var octal = octal
    }
  }
  
  def scanIdentifier() = {
    var `type`: Double = _
    val start = this.index
    // Backslash (U+005C) starts an escaped character.
    val id = if (this.source.charCodeAt(start) == 0x5C) this.getComplexIdentifier() else this.getIdentifier()
    // There is no keyword or literal with only one character.
    // Thus, it must be an identifier.
    if (id.length == 1) {
      `type` = 3
    } else if (this.isKeyword(id)) {
      `type` = 4
    } else if (id == "null") {
      `type` = 5
    } else if (id == "true" || id == "false") {
      `type` = 1
    } else {
      `type` = 3
    }
    if (`type` != 3 && start + id.length != this.index) {
      val restore = this.index
      this.index = start
      this.tolerateUnexpectedToken(Messages.InvalidEscapedReservedWord)
      this.index = restore
    }
    new {
      var `type` = `type`
      var value = id
      var lineNumber = this.lineNumber
      var lineStart = this.lineStart
      var start = start
      var end = this.index
    }
  }
  
  def scanPunctuator() = {
    val start = this.index
    // Check for most common single-character punctuators.
    var str = this.source(this.index)
    str match {
      case "(" | "{" =>
        if (str == "{") {
          this.curlyStack.push("{")
        }
        this.index += 1
      case "." =>
        this.index += 1
        if (this.source(this.index) == "." && this.source(this.index + 1) == ".") {
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
              str = this.source(this.index)
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
    new {
      var `type` = 7
      var value = str
      var lineNumber = this.lineNumber
      var lineStart = this.lineStart
      var start = start
      var end = this.index
    }
  }
  
  def scanHexLiteral(start: Double) = {
    var num = ""
    while (!this.eof()) {
      if (!Character.isHexDigit(this.source.charCodeAt(this.index))) {
        /* Unsupported: Break */ break;
      }
      num += this.source({
        val temp = this.index
        this.index += 1
        temp
      })
    }
    if (num.length == 0) {
      this.throwUnexpectedToken()
    }
    if (Character.isIdentifierStart(this.source.charCodeAt(this.index))) {
      this.throwUnexpectedToken()
    }
    new {
      var `type` = 6
      var value = parseInt("0x" + num, 16)
      var lineNumber = this.lineNumber
      var lineStart = this.lineStart
      var start = start
      var end = this.index
    }
  }
  
  def scanBinaryLiteral(start: Double) = {
    var num = ""
    var ch: String = _
    while (!this.eof()) {
      ch = this.source(this.index)
      if (ch != "0" && ch != "1") {
        /* Unsupported: Break */ break;
      }
      num += this.source({
        val temp = this.index
        this.index += 1
        temp
      })
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
    new {
      var `type` = 6
      var value = parseInt(num, 2)
      var lineNumber = this.lineNumber
      var lineStart = this.lineStart
      var start = start
      var end = this.index
    }
  }
  
  def scanOctalLiteral(prefix: String, start: Double) = {
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
    while (!this.eof()) {
      if (!Character.isOctalDigit(this.source.charCodeAt(this.index))) {
        /* Unsupported: Break */ break;
      }
      num += this.source({
        val temp = this.index
        this.index += 1
        temp
      })
    }
    if (!octal && num.length == 0) {
      // only 0o or 0O
      this.throwUnexpectedToken()
    }
    if (Character.isIdentifierStart(this.source.charCodeAt(this.index)) || Character.isDecimalDigit(this.source.charCodeAt(this.index))) {
      this.throwUnexpectedToken()
    }
    new {
      var `type` = 6
      var value = parseInt(num, 8)
      var octal = octal
      var lineNumber = this.lineNumber
      var lineStart = this.lineStart
      var start = start
      var end = this.index
    }
  }
  
  def isImplicitOctalLiteral() = {
    // Implicit octal, unless there is a non-octal digit.
    // (Annex B.1.1 on Numeric Literals)
    for (i <- this.index + 1 until this.length) {
      val ch = this.source(i)
      if (ch == "8" || ch == "9") {
        return false
      }
      if (!Character.isOctalDigit(ch.charCodeAt(0))) {
        return true
      }
    }
    true
  }
  
  def scanNumericLiteral() = {
    val start = this.index
    var ch = this.source(start)
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
        num += this.source({
          val temp = this.index
          this.index += 1
          temp
        })
      }
      ch = this.source(this.index)
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
    new {
      var `type` = 6
      var value = parseFloat(num)
      var lineNumber = this.lineNumber
      var lineStart = this.lineStart
      var start = start
      var end = this.index
    }
  }
  
  def scanStringLiteral() = {
    val start = this.index
    var quote = this.source(start)
    assert(quote == "\'" || quote == "\"", "String literal must starts with a quote")
    this.index += 1
    var octal = false
    var str = ""
    while (!this.eof()) {
      var ch = this.source({
        val temp = this.index
        this.index += 1
        temp
      })
      if (ch == quote) {
        quote = ""
        /* Unsupported: Break */ break;
      } else if (ch == "\\") {
        ch = this.source({
          val temp = this.index
          this.index += 1
          temp
        })
        if (!ch || !Character.isLineTerminator(ch.charCodeAt(0))) {
          ch match {
            case "u" =>
              if (this.source(this.index) == "{") {
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
                octal = octToDec.octal || octal
                str += String.fromCharCode(octToDec.code)
              } else {
                str += ch
              }
          }
        } else {
          this.lineNumber += 1
          if (ch == "\r" && this.source(this.index) == "\n") {
            this.index += 1
          }
          this.lineStart = this.index
        }
      } else if (Character.isLineTerminator(ch.charCodeAt(0))) {
        /* Unsupported: Break */ break;
      } else {
        str += ch
      }
    }
    if (quote != "") {
      this.index = start
      this.throwUnexpectedToken()
    }
    new {
      var `type` = 8
      var value = str
      var octal = octal
      var lineNumber = this.lineNumber
      var lineStart = this.lineStart
      var start = start
      var end = this.index
    }
  }
  
  def scanTemplate() = {
    var cooked = ""
    var terminated = false
    val start = this.index
    val head = this.source(start) == "`"
    var tail = false
    var rawOffset = 2
    this.index += 1
    while (!this.eof()) {
      var ch = this.source({
        val temp = this.index
        this.index += 1
        temp
      })
      if (ch == "`") {
        rawOffset = 1
        tail = true
        terminated = true
        /* Unsupported: Break */ break;
      } else if (ch == "$") {
        if (this.source(this.index) == "{") {
          this.curlyStack.push("${")
          this.index += 1
          terminated = true
          /* Unsupported: Break */ break;
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
              if (this.source(this.index) == "{") {
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
          if (ch == "\r" && this.source(this.index) == "\n") {
            this.index += 1
          }
          this.lineStart = this.index
        }
      } else if (Character.isLineTerminator(ch.charCodeAt(0))) {
        this.lineNumber += 1
        if (ch == "\r" && this.source(this.index) == "\n") {
          this.index += 1
        }
        this.lineStart = this.index
        cooked += "\n"
      } else {
        cooked += ch
      }
    }
    if (!terminated) {
      this.throwUnexpectedToken()
    }
    if (!head) {
      this.curlyStack.pop()
    }
    new {
      var `type` = 10
      var value = this.source.slice(start + 1, this.index - rawOffset)
      var cooked = cooked
      var head = head
      var tail = tail
      var lineNumber = this.lineNumber
      var lineStart = this.lineStart
      var start = start
      var end = this.index
    }
  }
  
  def testRegExp(pattern: Any, flags: String) = {
    // The BMP character to use as a replacement for astral symbols when
    // translating an ES6 "u"-flagged pattern to an ES5-compatible
    // approximation.
    // Note: replacing with '\uFFFF' enables false positives in unlikely
    // scenarios. For example, `[\u{1044f}-\u{10440}]` is an invalid
    // pattern that would not be detected by this substitution.
    val astralSubstitute = "￿"
    var tmp = pattern
    val self = this
    if (flags.indexOf("u") >= 0) {
      tmp = tmp.replace("/\\u\{([0-9a-fA-F]+)\}|\\u([a-fA-F0-9]{4})/g".r, ($0, $1, $2) => {
        val codePoint = parseInt($1 || $2, 16)
        if (codePoint > 0x10FFFF) {
          self.throwUnexpectedToken(Messages.InvalidRegExp)
        }
        if (codePoint <= 0xFFFF) {
          return String.fromCharCode(codePoint)
        }
        astralSubstitute
      }
      ).replace("/[\uD800-\uDBFF][\uDC00-\uDFFF]/g".r, astralSubstitute)
    }
    // First, detect invalid regular expressions.
    try {
      RegExp(tmp)
    } catch {
      case e =>
        this.throwUnexpectedToken(Messages.InvalidRegExp)
    }
    // Return a regular expression object for this pattern-flag pair, or
    // `null` in case the current environment doesn't support the flags it
    // uses.
    try {
      new RegExp(pattern, flags)
    } catch {
      case exception =>
        /*istanbul ignore next */
        return null
    }
  }
  
  def scanRegExpBody() = {
    var ch = this.source(this.index)
    assert(ch == "/", "Regular expression literal must start with a slash")
    var str = this.source({
      val temp = this.index
      this.index += 1
      temp
    })
    var classMarker = false
    var terminated = false
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
          /* Unsupported: Break */ break;
        } else if (ch == "[") {
          classMarker = true
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
    while (!this.eof()) {
      var ch = this.source(this.index)
      if (!Character.isIdentifierPart(ch.charCodeAt(0))) {
        /* Unsupported: Break */ break;
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
    flags
  }
  
  def scanRegExp() = {
    val start = this.index
    val pattern = this.scanRegExpBody()
    val flags = this.scanRegExpFlags()
    val value = this.testRegExp(pattern, flags)
    new {
      var `type` = 9
      var value = ""
      var pattern = pattern
      var flags = flags
      var regex = value
      var lineNumber = this.lineNumber
      var lineStart = this.lineStart
      var start = start
      var end = this.index
    }
  }
  
  def lex() = {
    if (this.eof()) {
      return new {
        var `type` = 2
        var value = ""
        var lineNumber = this.lineNumber
        var lineStart = this.lineStart
        var start = this.index
        var end = this.index
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



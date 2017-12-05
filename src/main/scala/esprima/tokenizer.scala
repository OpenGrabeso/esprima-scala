/*
ScalaFromJS: 2017-12-05 14:48:54.460
tokenizer.js
*/

package esprima

class Reader() {
  var paren: Int = -1
  var values = Array.empty[String]
  var curly: Int = paren
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
  
  def push(token: TemplateElement) = {
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

class Tokenizer(code: String, config: Any) {
  var errorHandler: ErrorHandler = new ErrorHandler()
  errorHandler.tolerant = config.tolerant
  var scanner: Scanner = new Scanner(code, this.errorHandler)
  scanner.trackComment = config.comment
  var trackRange: Boolean = config.range
  var trackLoc: Boolean = config.loc
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
        var token: TemplateElement = _
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

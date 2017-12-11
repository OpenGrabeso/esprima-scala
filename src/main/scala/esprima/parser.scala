/*
ScalaFromJS: 2017-12-06 21:28:23.723
parser.js
*/

package esprima

import Parser._
import esprima.Scanner.{Position, RawToken, SourceLocation}
import esprima.port.RegExp

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._
import Token._
import esprima.Node.{ArrayPattern, AssignmentPattern, RestElement}

object Parser {

  class Options {

    var range: Boolean = true
    var loc: Boolean = false // default loc is true because that allows us to perform symbol matching
    var source: String = null
    var tokens: Boolean = false
    var comment: Boolean = false
    var tolerant : Boolean = false
    var attachComment: Boolean = false
    var sourceType: String = _
  }
  object DefaultOptions extends Options

  class Config extends Options

  trait Context {
    var isModule: Boolean
    var allowIn: Boolean
    var allowStrictDirective: Boolean
    var allowYield: Boolean
    var await: Boolean
    var firstCoverInitializedNameError: RawToken
    var isAssignmentTarget: Boolean
    var isBindingElement: Boolean
    var inFunctionBody: Boolean
    var inIteration: Boolean
    var inSwitch: Boolean
    var labelSet: mutable.Map[String, Boolean]
    var strict: Boolean
  }

  trait Marker {
    var index: Int
    var line: Int
    var column: Int
  }

  trait TokenEntry {
    def `type`: String
    def value: String
    var regex: RegExp = _
    var range: (Int, Int) = _
    var loc: SourceLocation = _
  }

  trait ParameterOptions {
    var simple: Boolean = _
    var params: ArrayBuffer[Node.FunctionParameter] = _
    var paramSet: mutable.Map[String, Boolean] = _
    var stricted: RawToken = _
    var firstRestricted: RawToken = _
    var message: String = _
  }

  trait VariableOptions {
    var inFor: Boolean = _
  }

}

class Parser(code: String, options: Options, var delegate: (Node.Node, Scanner.Metadata) => Unit) {
  self =>
  var config = new Config {
    range = options.range
    loc = options.loc
    source = null
    tokens = options.tokens
    comment = options.comment
    tolerant = options.tolerant
  }
  if (config.loc && options.source && options.source != null) {
    config.source = options.source
  }
  var errorHandler: ErrorHandler = new ErrorHandler()
  errorHandler.tolerant = config.tolerant
  var scanner: Scanner = new Scanner(code, this.errorHandler)
  scanner.trackComment = config.comment
  object operatorPrecedence {
    object list {
      var `)` = 0
      var `;` = 0
      var `,` = 0
      var `=` = 0
      var `]` = 0
      var || = 1
      var && = 2
      var | = 3
      var ^ = 4
      var & = 5
      var == = 6
      var != = 6
      var === = 6
      var !== = 6
      var < = 7
      var > = 7
      var <= = 7
      var >= = 7
      var << = 8
      var >> = 8
      var >>> = 8
      var + = 9
      var - = 9
      var * = 11
      var / = 11
      var % = 11
    }
    val map = port.MapFromObject.mapFromObject[Int](list)
    def apply(op: String): Int = map.getOrElse(op, 0)
  }
  var lookahead: RawToken = new RawToken {
    import OrType._
    `type` = EOF
    override def value = ""
    override def lineNumber = scanner.lineNumber
    override def lineStart = 0
    override def start = 0
    override def end = 0
  }
  var hasLineTerminator: Boolean = false
  var context: Context = new Context {
    var isModule = false
    var await = false
    var allowIn = true
    var allowStrictDirective = true
    var allowYield = true
    var firstCoverInitializedNameError: RawToken = null
    var isAssignmentTarget = false
    var isBindingElement = false
    var inFunctionBody = false
    var inIteration = false
    var inSwitch = false
    var labelSet = mutable.Map.empty
    var strict = false
  }
  var tokens = ArrayBuffer.empty[Parser.TokenEntry]
  var startMarker: Marker = new Marker {
    var index = 0
    var line = scanner.lineNumber
    var column = 0
  }
  var lastMarker: Marker = new Marker {
    var index = 0
    var line = scanner.lineNumber
    var column = 0
  }
  this.nextToken()
  lastMarker = new Marker {
    var index = scanner.index
    var line = scanner.lineNumber
    var column = scanner.index - scanner.lineStart
  }
  def throwError(messageFormat: String, args: String*) = {
    val msg = "%(\\d)".r.replaceAllIn(messageFormat, m => {
      val idx = m.source.toString.toInt
      assert(idx < args.length, "Message reference must be in range")
      args(idx)
    }
    )
    val index = this.lastMarker.index
    val line = this.lastMarker.line
    val column = this.lastMarker.column + 1
    throw this.errorHandler.createError(index, line, column, msg)
  }
  
  def tolerateError(messageFormat: String, args: String*) = {
    val msg = "%(\\d)".r.replaceAllIn(messageFormat, m => {
      val idx = m.source.toString.toInt
      assert(idx < args.length, "Message reference must be in range")
      args(idx)
    }
    )
    val index = this.lastMarker.index
    val line = this.scanner.lineNumber
    val column = this.lastMarker.column + 1
    this.errorHandler.tolerateError(index, line, column, msg)
  }
  
  def unexpectedTokenError(token: RawToken, message: String = null) = {
    var msg = message || Messages.UnexpectedToken
    var value: String = null
    if (token) {
      if (!message) {
        msg = if (token.`type` == EOF) Messages.UnexpectedEOS else if (token.`type` == Identifier) Messages.UnexpectedIdentifier else if (token.`type` == NumericLiteral) Messages.UnexpectedNumber else if (token.`type` == StringLiteral) Messages.UnexpectedString else if (token.`type` == Template) Messages.UnexpectedTemplate else Messages.UnexpectedToken
        if (token.`type` == Keyword) {
          if (this.scanner.isFutureReservedWord(token.value)) {
            msg = Messages.UnexpectedReserved
          } else if (this.context.strict && this.scanner.isStrictModeReservedWord(token.value)) {
            msg = Messages.StrictReservedWord
          }
        }
      }
      value = token.value
    } else {
      value = "ILLEGAL"
    }
    msg = msg.replace("%0", value)
    if (token && token.lineNumber >= 0) {
      val index = token.start
      val line = token.lineNumber
      val lastMarkerLineStart = this.lastMarker.index - this.lastMarker.column
      val column = token.start - lastMarkerLineStart + 1
      this.errorHandler.createError(index, line, column, msg)
    } else {
      val index = this.lastMarker.index
      val line = this.lastMarker.line
      val column = this.lastMarker.column + 1
      this.errorHandler.createError(index, line, column, msg)
    }
  }
  
  def throwUnexpectedToken(token: RawToken, message: String = null) = {
    throw this.unexpectedTokenError(token, message)
  }
  
  def tolerateUnexpectedToken(token: RawToken, message: String = null) = {
    this.errorHandler.tolerate(this.unexpectedTokenError(token, message))
  }
  
  def collectComments() = {
    if (!this.config.comment) {
      this.scanner.scanComments()
    } else {
      val comments = this.scanner.scanComments()
      if (comments.length > 0 && this.delegate) {
        for (e <- comments) {
          object node extends Node.CommentNode {
            var `type` = if (e.multiLine) "BlockComment" else "LineComment"
            value = self.scanner.source.slice(e.slice._1, e.slice._2)
          }
          if (this.config.range) {
            node.range = e.range
          }
          if (this.config.loc) {
            node.loc = e.loc
          }
          object metadata extends Scanner.Metadata {
            var start: Position = new Position {
              override def line = e.loc.start.line
              override def column = e.loc.start.column
              override def offset = e.range._1
            }
            var end: Position = new Position {
              override def line = e.loc.end.line
              override def column = e.loc.end.column
              override def offset = e.range._2
            }
          }
          this.delegate(node, metadata)
        }
      }
    }
  }
  
  def getTokenRaw(token: RawToken): String = {
    this.scanner.source.slice(token.start, token.end)
  }
  
  def convertToken(token: RawToken): TokenEntry = {
    object t extends TokenEntry {
      var `type` = TokenName(token.`type`)
      var value = self.getTokenRaw(token)
    }
    if (this.config.range) {
      t.range = (token.start, token.end)
    }
    if (this.config.loc) {
      t.loc = new SourceLocation {
        var start: Position = new Position {
          override def line = self.startMarker.line
          override def column = self.startMarker.column
        }
        var end: Position = new Position {
          override def line = self.scanner.lineNumber
          override def column = self.scanner.index - self.scanner.lineStart
        }
      }
    }
    if (token.`type` == RegularExpression) {
      val pattern = token.pattern
      val flags = token.flags
      t.regex = new RegExp (
        pattern = pattern,
        flags = flags
      )
    }
    t
  }
  
  def nextToken() = {
    val token = this.lookahead
    this.lastMarker.index = this.scanner.index
    this.lastMarker.line = this.scanner.lineNumber
    this.lastMarker.column = this.scanner.index - this.scanner.lineStart
    this.collectComments()
    if (this.scanner.index != this.startMarker.index) {
      this.startMarker.index = this.scanner.index
      this.startMarker.line = this.scanner.lineNumber
      this.startMarker.column = this.scanner.index - this.scanner.lineStart
    }
    val next = this.scanner.lex()
    this.hasLineTerminator = token.lineNumber != next.lineNumber
    if (next && this.context.strict && next.`type` == Identifier) {
      if (this.scanner.isStrictModeReservedWord(next.value)) {
        next.`type` = Keyword
      }
    }
    this.lookahead = next
    if (this.config.tokens && next.`type` != EOF) {
      this.tokens.push(this.convertToken(next))
    }
    token
  }
  
  def nextRegexToken() = {
    this.collectComments()
    val token = this.scanner.scanRegExp()
    if (this.config.tokens) {
      // Pop the previous token, '/' or '/='
      // This is added from the lookahead token.
      this.tokens.pop()
      this.tokens.push(this.convertToken(token))
    }
    // Prime the next lookahead.
    this.lookahead = token
    this.nextToken()
    token
  }
  
  def createNode(): Marker = {
    new Marker {
      var index = self.startMarker.index
      var line = self.startMarker.line
      var column = self.startMarker.column
    }
  }
  
  def startNode(token: RawToken, lastLineStart: Int = 0): Marker = {
    var column_ = token.start - token.lineStart
    var line_ = token.lineNumber
    if (column_ < 0) {
      column_ += lastLineStart
      line_ -= 1
    }
    new Marker {
      var index = token.start
      var line = line_
      var column = column_
    }
  }
  
  def finalize[T <: Node.Node](marker: Marker, node: T): T = {
    if (this.config.range) {
      node.range = (marker.index, this.lastMarker.index)
    }
    if (this.config.loc) {
      node.loc = new SourceLocation {
        var start: Position = new Position {
          override def line = marker.line
          override def column = marker.column
        }
        var end: Position = new Position {
          override def line = self.lastMarker.line
          override def column = self.lastMarker.column
        }
      }
      if (this.config.source) {
        node.loc.source = this.config.source
      }
    }
    if (this.delegate) {
      object metadata extends Scanner.Metadata {
        var start: Position = new Position {
          override def line = marker.line
          override def column = marker.column
          override def offset = marker.index
        }
        var end: Position = new Position {
          override def line = self.lastMarker.line
          override def column = self.lastMarker.column
          override def offset = self.lastMarker.index
        }
      }
      this.delegate(node, metadata)
    }
    node
  }
  
  def expect(value: String) = {
    val token = this.nextToken()
    if (token.`type` != Punctuator || (token.value !== value)) {
      this.throwUnexpectedToken(token)
    }
  }
  
  def expectCommaSeparator() = {
    if (this.config.tolerant) {
      val token = this.lookahead
      if (token.`type` == Punctuator && token.value === ",") {
        this.nextToken()
      } else if (token.`type` == Punctuator && token.value === ";") {
        this.nextToken()
        this.tolerateUnexpectedToken(token)
      } else {
        this.tolerateUnexpectedToken(token, Messages.UnexpectedToken)
      }
    } else {
      this.expect(",")
    }
  }
  
  def expectKeyword(keyword: String) = {
    val token = this.nextToken()
    if (token.`type` != Keyword || (token.value !== keyword)) {
      this.throwUnexpectedToken(token)
    }
  }
  
  def `match`(value: String) = {
    this.lookahead.`type` == Punctuator && this.lookahead.value === value
  }
  
  def matchKeyword(keyword: String) = {
    this.lookahead.`type` == Keyword && this.lookahead.value === keyword
  }
  
  def matchContextualKeyword(keyword: String) = {
    this.lookahead.`type` == Identifier && this.lookahead.value === keyword
  }
  
  def matchAssign(): Boolean = {
    if (this.lookahead.`type` != Punctuator) {
      return false
    }
    val op: String = this.lookahead.value
    op == "=" || op == "*=" || op == "**=" || op == "/=" || op == "%=" || op == "+=" || op == "-=" || op == "<<=" || op == ">>=" || op == ">>>=" || op == "&=" || op == "^=" || op == "|="
  }
  
  def isolateCoverGrammar[T <:Node.Node](parseFunction: () => T): T = {
    val previousIsBindingElement = this.context.isBindingElement
    val previousIsAssignmentTarget = this.context.isAssignmentTarget
    val previousFirstCoverInitializedNameError = this.context.firstCoverInitializedNameError
    this.context.isBindingElement = true
    this.context.isAssignmentTarget = true
    this.context.firstCoverInitializedNameError = null
    val result = parseFunction()
    if (this.context.firstCoverInitializedNameError != null) {
      this.throwUnexpectedToken(this.context.firstCoverInitializedNameError)
    }
    this.context.isBindingElement = previousIsBindingElement
    this.context.isAssignmentTarget = previousIsAssignmentTarget
    this.context.firstCoverInitializedNameError = previousFirstCoverInitializedNameError
    result
  }
  
  def inheritCoverGrammar[T <:Node.Node](parseFunction: () => T): T = {
    val previousIsBindingElement = this.context.isBindingElement
    val previousIsAssignmentTarget = this.context.isAssignmentTarget
    val previousFirstCoverInitializedNameError = this.context.firstCoverInitializedNameError
    this.context.isBindingElement = true
    this.context.isAssignmentTarget = true
    this.context.firstCoverInitializedNameError = null
    val result = parseFunction()
    this.context.isBindingElement = this.context.isBindingElement && previousIsBindingElement
    this.context.isAssignmentTarget = this.context.isAssignmentTarget && previousIsAssignmentTarget
    this.context.firstCoverInitializedNameError = previousFirstCoverInitializedNameError || this.context.firstCoverInitializedNameError
    result
  }
  
  def consumeSemicolon() = {
    if (this.`match`(";")) {
      this.nextToken()
    } else if (!this.hasLineTerminator) {
      if (this.lookahead.`type` != EOF && !this.`match`("}")) {
        this.throwUnexpectedToken(this.lookahead)
      }
      this.lastMarker.index = this.startMarker.index
      this.lastMarker.line = this.startMarker.line
      this.lastMarker.column = this.startMarker.column
    }
  }
  
  def parsePrimaryExpression(): Node.Expression = {
    val node = this.createNode()
    var expr: Node.Expression = null
    var token: RawToken = null
    var raw: String = null
    this.lookahead.`type` match {
      case Identifier =>
        if ((this.context.isModule || this.context.await) && this.lookahead.value === "await") {
          this.tolerateUnexpectedToken(this.lookahead)
        }
        expr = if (this.matchAsyncFunction()) this.parseFunctionExpression() else this.finalize(node, new Node.Identifier(this.nextToken().value))
      case NumericLiteral | StringLiteral =>
        if (this.context.strict && this.lookahead.octal) {
          this.tolerateUnexpectedToken(this.lookahead, Messages.StrictOctalLiteral)
        }
        this.context.isAssignmentTarget = false
        this.context.isBindingElement = false
        token = this.nextToken()
        raw = this.getTokenRaw(token)
        expr = this.finalize(node, new Node.Literal(token.value, raw))
      case BooleanLiteral =>
        this.context.isAssignmentTarget = false
        this.context.isBindingElement = false
        token = this.nextToken()
        raw = this.getTokenRaw(token)
        expr = this.finalize(node, new Node.Literal(token.value === "true", raw))
      case NullLiteral =>
        this.context.isAssignmentTarget = false
        this.context.isBindingElement = false
        token = this.nextToken()
        raw = this.getTokenRaw(token)
        expr = this.finalize(node, new Node.Literal(null, raw))
      case Template =>
        expr = this.parseTemplateLiteral()
      case Punctuator =>
        this.lookahead.value.get[String] match {
          case "(" =>
            this.context.isBindingElement = false
            expr = this.inheritCoverGrammar(this.parseGroupExpression)
          case "[" =>
            expr = this.inheritCoverGrammar(this.parseArrayInitializer)
          case "{" =>
            expr = this.inheritCoverGrammar(this.parseObjectInitializer)
          case "/" | "/=" =>
            this.context.isAssignmentTarget = false
            this.context.isBindingElement = false
            this.scanner.index = this.startMarker.index
            token = this.nextRegexToken()
            raw = this.getTokenRaw(token)
            expr = this.finalize(node, new Node.RegexLiteral(token.regex, raw, token.pattern, token.flags))
          case _ =>
            expr = this.throwUnexpectedToken(this.nextToken())
        }
      case Keyword =>
        if (!this.context.strict && this.context.allowYield && this.matchKeyword("yield")) {
          expr = this.parseIdentifierName()
        } else if (!this.context.strict && this.matchKeyword("let")) {
          expr = this.finalize(node, new Node.Identifier(this.nextToken().value))
        } else {
          this.context.isAssignmentTarget = false
          this.context.isBindingElement = false
          if (this.matchKeyword("function")) {
            expr = this.parseFunctionExpression()
          } else if (this.matchKeyword("this")) {
            this.nextToken()
            expr = this.finalize(node, new Node.ThisExpression())
          } else if (this.matchKeyword("class")) {
            expr = this.parseClassExpression()
          } else if (this.matchImportCall()) {
            expr = this.parseImportCall().asInstanceOf[Node.Expression] // PORT: fix incorrect type
          } else {
            expr = this.throwUnexpectedToken(this.nextToken())
          }
        }
      case _ =>
        expr = this.throwUnexpectedToken(this.nextToken())
    }
    expr
  }
  
  def parseSpreadElement(): Node.SpreadElement = {
    val node = this.createNode()
    this.expect("...")
    val arg = this.inheritCoverGrammar(this.parseAssignmentExpression)
    this.finalize(node, new Node.SpreadElement(arg))
  }
  
  def parseArrayInitializer() = {
    val node = this.createNode()
    val elements = ArrayBuffer.empty[Node.ArrayExpressionElement]
    this.expect("[")
    while (!this.`match`("]")) {
      if (this.`match`(",")) {
        this.nextToken()
        elements.push(null)
      } else if (this.`match`("...")) {
        val element = this.parseSpreadElement()
        if (!this.`match`("]")) {
          this.context.isAssignmentTarget = false
          this.context.isBindingElement = false
          this.expect(",")
        }
        elements.push(element)
      } else {
        elements.push(this.inheritCoverGrammar(this.parseAssignmentExpression))
        if (!this.`match`("]")) {
          this.expect(",")
        }
      }
    }
    this.expect("]")
    this.finalize(node, new Node.ArrayExpression(elements))
  }
  
  def parsePropertyMethod(params: ParameterOptions) = {
    this.context.isAssignmentTarget = false
    this.context.isBindingElement = false
    val previousStrict = this.context.strict
    val previousAllowStrictDirective = this.context.allowStrictDirective
    this.context.allowStrictDirective = params.simple
    val body = this.isolateCoverGrammar(this.parseFunctionSourceElements)
    if (this.context.strict && params.firstRestricted) {
      this.tolerateUnexpectedToken(params.firstRestricted, params.message)
    }
    if (this.context.strict && params.stricted) {
      this.tolerateUnexpectedToken(params.stricted, params.message)
    }
    this.context.strict = previousStrict
    this.context.allowStrictDirective = previousAllowStrictDirective
    body
  }
  
  def parsePropertyMethodFunction() = {
    val isGenerator = false
    val node = this.createNode()
    val previousAllowYield = this.context.allowYield
    this.context.allowYield = true
    val params = this.parseFormalParameters()
    val method = this.parsePropertyMethod(params)
    this.context.allowYield = previousAllowYield
    this.finalize(node, new Node.FunctionExpression(null, params.params, method, isGenerator))
  }
  
  def parsePropertyMethodAsyncFunction() = {
    val node = this.createNode()
    val previousAllowYield = this.context.allowYield
    val previousAwait = this.context.await
    this.context.allowYield = false
    this.context.await = true
    val params = this.parseFormalParameters()
    val method = this.parsePropertyMethod(params)
    this.context.allowYield = previousAllowYield
    this.context.await = previousAwait
    this.finalize(node, new Node.AsyncFunctionExpression(null, params.params, method))
  }
  
  def parseObjectPropertyKey(): Node.PropertyKey = {
    val node = this.createNode()
    val token = this.nextToken()
    var key: Node.PropertyKey = null
    token.`type` match {
      case StringLiteral | NumericLiteral =>
        if (this.context.strict && token.octal) {
          this.tolerateUnexpectedToken(token, Messages.StrictOctalLiteral)
        }
        val raw = this.getTokenRaw(token)
        key = this.finalize(node, new Node.Literal(token.value, raw))
      case Identifier | BooleanLiteral | NullLiteral | Keyword =>
        key = this.finalize(node, new Node.Identifier(token.value))
      case Punctuator =>
        if (token.value === "[") {
          key = this.isolateCoverGrammar(this.parseAssignmentExpression).asInstanceOf[Node.PropertyKey] // PORT: fix incorrect type
          this.expect("]")
        } else {
          key = this.throwUnexpectedToken(token)
        }
      case _ =>
        key = this.throwUnexpectedToken(token)
    }
    key
  }
  
  def isPropertyKey(key: Node.Node, value: String) = {
    key.isInstanceOf[Node.Identifier] && key.asInstanceOf[Node.Identifier].name == value || key.isInstanceOf[Node.Literal] && key.asInstanceOf[Node.Literal].value === value
  }
  
  def parseObjectProperty(hasProto: ByRef[Boolean]): Node.Property = {
    val node = this.createNode()
    val token = this.lookahead
    var kind: String = ""
    var key: Node.PropertyKey = null
    var value: Node.PropertyValue = null
    var computed = false
    var method = false
    var shorthand = false
    var isAsync = false
    if (token.`type` == Identifier) {
      val id: String = token.value
      this.nextToken()
      computed = this.`match`("[")
      isAsync = !this.hasLineTerminator && id == "async" && !this.`match`(":") && !this.`match`("(") && !this.`match`("*") && !this.`match`(",")
      key = if (isAsync) this.parseObjectPropertyKey() else this.finalize(node, new Node.Identifier(id))
    } else if (this.`match`("*")) {
      this.nextToken()
    } else {
      computed = this.`match`("[")
      key = this.parseObjectPropertyKey()
    }
    val lookaheadPropertyKey = this.qualifiedPropertyName(this.lookahead)
    if (token.`type` == Identifier && !isAsync && token.value === "get" && lookaheadPropertyKey) {
      kind = "get"
      computed = this.`match`("[")
      key = this.parseObjectPropertyKey()
      this.context.allowYield = false
      value = this.parseGetterMethod()
    } else if (token.`type` == Identifier && !isAsync && token.value === "set" && lookaheadPropertyKey) {
      kind = "set"
      computed = this.`match`("[")
      key = this.parseObjectPropertyKey()
      value = this.parseSetterMethod()
    } else if (token.`type` == Punctuator && token.value === "*" && lookaheadPropertyKey) {
      kind = "init"
      computed = this.`match`("[")
      key = this.parseObjectPropertyKey()
      value = this.parseGeneratorMethod()
      method = true
    } else {
      if (!key) {
        this.throwUnexpectedToken(this.lookahead)
      }
      kind = "init"
      if (this.`match`(":") && !isAsync) {
        if (!computed && this.isPropertyKey(key, "__proto__")) {
          if (hasProto.value) {
            this.tolerateError(Messages.DuplicateProtoProperty)
          }
          hasProto.value = true
        }
        this.nextToken()
        value = this.inheritCoverGrammar(this.parseAssignmentExpression)
      } else if (this.`match`("(")) {
        value = if (isAsync) this.parsePropertyMethodAsyncFunction() else this.parsePropertyMethodFunction()
        method = true
      } else if (token.`type` == Identifier) {
        val id = this.finalize(node, new Node.Identifier(token.value))
        if (this.`match`("=")) {
          this.context.firstCoverInitializedNameError = this.lookahead
          this.nextToken()
          shorthand = true
          val init = this.isolateCoverGrammar(this.parseAssignmentExpression)
          value = this.finalize(node, new Node.AssignmentPattern(id, init))
        } else {
          shorthand = true
          value = id
        }
      } else {
        this.throwUnexpectedToken(this.nextToken())
      }
    }
    this.finalize(node, new Node.Property(kind, key, computed, value, method, shorthand))
  }
  
  def parseObjectInitializer() = {
    val node = this.createNode()
    this.expect("{")
    val properties = ArrayBuffer.empty[Node.ObjectExpressionProperty]
    object hasProto extends ByRef[Boolean](false)
    while (!this.`match`("}")) {
      properties.push(if (this.`match`("...")) this.parseSpreadElement() else this.parseObjectProperty(hasProto))
      if (!this.`match`("}")) {
        this.expectCommaSeparator()
      }
    }
    this.expect("}")
    this.finalize(node, new Node.ObjectExpression(properties))
  }
  
  def parseTemplateHead(): Node.TemplateElement = {
    assert(this.lookahead.head, "Template literal must start with a template head")
    val node = this.createNode()
    val token = this.nextToken()
    val raw_ = token.value
    val cooked_ = token.cooked
    this.finalize(node, new Node.TemplateElement(new Node.TemplateElementValue {
      var raw = raw_
      var cooked = cooked_
    }, token.tail))
  }
  
  def parseTemplateElement() = {
    if (this.lookahead.`type` != Template) {
      this.throwUnexpectedToken(this.lookahead)
    }
    val node = this.createNode()
    val token = this.nextToken()
    val raw_ = token.value
    val cooked_ = token.cooked
    this.finalize(node, new Node.TemplateElement(new Node.TemplateElementValue {
      var raw = raw_
      var cooked = cooked_
    }, token.tail))
  }
  
  def parseTemplateLiteral(): Node.TemplateLiteral = {
    val node = this.createNode()
    val expressions = ArrayBuffer.empty[Node.Expression]
    val quasis = ArrayBuffer.empty[Node.TemplateElement]
    var quasi = this.parseTemplateHead()
    quasis.push(quasi)
    while (!quasi.tail) {
      expressions.push(this.parseExpression())
      quasi = this.parseTemplateElement()
      quasis.push(quasi)
    }
    this.finalize(node, new Node.TemplateLiteral(quasis, expressions))
  }

  def reinterpretExpressionAsArrayPattern(expr: Node.Node): Node.ArrayPatternElement = {
    // TODO: reallocation needed
    (expr: @unchecked) match {
      case ex: Node.Identifier => ex
      case ex: Node.ComputedMemberExpression => ex
      case ex: Node.RestElement => ex
      case ex: Node.AssignmentPattern => ex

      case expr: Node.SpreadElement =>
        val retypedArg =this.reinterpretExpressionAsArrayPattern(expr.argument)
        new Node.RestElement(retypedArg)
      case expr: Node.ArrayExpression =>
        val elementsResult = expr.elements.flatMap { i =>
          Option(i).map { i =>
            this.reinterpretExpressionAsArrayPattern(i)
          }
        }
        new ArrayPattern(elementsResult)
      case expr: Node.AssignmentExpression =>
        new AssignmentPattern (this.reinterpretExpressionAsArrayPattern(expr.left).asInstanceOf[Node.BindingIdentifierOrPattern], expr.right)
      case expr: Node.ArrayPatternElement =>
        expr
    }
  }


  def reinterpretExpressionAsObjectPattern(expr: Node.Node): Node.ObjectPatternProperty = {
    // TODO: reallocation needed
    (expr: @unchecked) match {
      case expr: Node.SpreadElement =>

        new RestElement(this.reinterpretExpressionAsObjectPattern(expr.argument))

      case expr: Node.ObjectExpression =>
        val elementsResult = for (property <- expr.properties) yield {
          this.reinterpretExpressionAsObjectPattern(if (property.isInstanceOf[Node.SpreadElement]) property else property.asInstanceOf[Node.Property].value)
        }
        new Node.ObjectPattern(elementsResult)
      case expr: Node.AssignmentExpression =>
        new Node.AssignmentPattern(this.reinterpretExpressionAsObjectPattern(expr.left), expr.right)
      case expr: Node.ObjectPatternProperty =>
        expr
    }
  }
  
  def parseGroupExpression() = {
    var expr: Node.Expression = null
    this.expect("(")
    if (this.`match`(")")) {
      this.nextToken()
      if (!this.`match`("=>")) {
        this.expect("=>")
      }
      expr = new Node.ArrowParameterPlaceHolder {
        params = ArrayBuffer()
        async = false
      }
    } else {
      val startToken = this.lookahead
      val params = ArrayBuffer.empty[Scanner.RawToken]
      if (this.`match`("...")) {
        val exprP = this.parseRestElement(params)
        this.expect(")")
        if (!this.`match`("=>")) {
          this.expect("=>")
        }
        expr = new Node.ArrowParameterPlaceHolder {
          params = ArrayBuffer(exprP)
          async = false
        }
      } else {
        var arrow = false
        this.context.isBindingElement = true
        expr = this.inheritCoverGrammar(this.parseAssignmentExpression)
        if (this.`match`(",")) {
          val expressions = ArrayBuffer.empty[Node.ArgumentListElement]
          this.context.isAssignmentTarget = false
          expressions.push(expr)
          breakable {
            while (this.lookahead.`type` != EOF) {
              if (!this.`match`(",")) {
                break
              }
              this.nextToken()
              if (this.`match`(")")) {
                this.nextToken()
                val expressionsRe = for (i <- expressions) yield {
                  this.reinterpretExpressionAsArrayPattern(i).asInstanceOf[Node.ArgumentListElement]
                }
                arrow = true
                expr = new Node.ArrowParameterPlaceHolder {
                  params = expressionsRe
                  async = false
                }
              } else if (this.`match`("...")) {
                if (!this.context.isBindingElement) {
                  this.throwUnexpectedToken(this.lookahead)
                }
                expressions.push(this.parseRestElement(params))
                this.expect(")")
                if (!this.`match`("=>")) {
                  this.expect("=>")
                }
                this.context.isBindingElement = false
                val expressionsRe = for (i <- expressions) yield {
                  this.reinterpretExpressionAsArrayPattern(i).asInstanceOf[Node.ArgumentListElement]
                }
                arrow = true
                expr = new Node.ArrowParameterPlaceHolder {
                  params = expressionsRe
                  async = false
                }
              } else {
                expressions.push(this.inheritCoverGrammar(this.parseAssignmentExpression))
              }
              if (arrow) {
                break
              }
            }
          }
          if (!arrow) {
            expr = this.finalize(this.startNode(startToken), new Node.SequenceExpression(expressions.asInstanceOf[Seq[Node.Expression]]))
          }
        }
        if (!arrow) {
          this.expect(")")
          if (this.`match`("=>")) {
            if (expr.isInstanceOf[Node.Identifier] && expr.asInstanceOf[Node.Identifier].name == "yield") {
              var expr_cast = expr.asInstanceOf[Node.Identifier]
              arrow = true
              expr = new Node.ArrowParameterPlaceHolder {
                params = ArrayBuffer(expr_cast)
                async = false
              }
            }
            if (!arrow) {
              if (!this.context.isBindingElement) {
                this.throwUnexpectedToken(this.lookahead)
              }
              val parameters = expr match {
                case expr_cast: Node.SequenceExpression =>
                  for (i <- expr_cast.expressions) yield {
                    this.reinterpretExpressionAsArrayPattern(i).asInstanceOf[Node.ArgumentListElement]
                  }
                case _ =>
                  Seq(this.reinterpretExpressionAsArrayPattern(expr).asInstanceOf[Node.ArgumentListElement])
              }
              expr = new Node.ArrowParameterPlaceHolder {
                params = parameters
                async = false
              }
            }
          }
          this.context.isBindingElement = false
        }
      }
    }
    expr
  }
  
  def parseArguments(): Array[Node.ArgumentListElement] = {
    this.expect("(")
    val args = ArrayBuffer.empty[Node.ArgumentListElement]
    if (!this.`match`(")")) {
      breakable {
        while (true) {
          val expr = if (this.`match`("...")) this.parseSpreadElement() else this.isolateCoverGrammar(this.parseAssignmentExpression)
          args.push(expr)
          if (this.`match`(")")) {
            break
          }
          this.expectCommaSeparator()
          if (this.`match`(")")) {
            break
          }
        }
      }
    }
    this.expect(")")
    args
  }
  
  def isIdentifierName(token: RawToken) = {
    token.`type` == Identifier || token.`type` == Keyword || token.`type` == BooleanLiteral || token.`type` == NullLiteral
  }
  
  def parseIdentifierName() = {
    val node = this.createNode()
    val token = this.nextToken()
    if (!this.isIdentifierName(token)) {
      this.throwUnexpectedToken(token)
    }
    import OrType._
    this.finalize(node, new Node.Identifier(token.value))
  }
  
  def parseNewExpression(): Node.Expression = {
    val node = this.createNode()
    val id = this.parseIdentifierName()
    assert(id.name == "new", "New expression must start with `new`")
    var expr: Node.Expression = null
    if (this.`match`(".")) {
      this.nextToken()
      if (this.lookahead.`type` == Identifier && this.context.inFunctionBody && this.lookahead.value === "target") {
        val property = this.parseIdentifierName()
        expr = new Node.MetaProperty(id, property)
      } else {
        this.throwUnexpectedToken(this.lookahead)
      }
    } else if (this.matchKeyword("import")) {
      this.throwUnexpectedToken(this.lookahead)
    } else {
      val callee = this.isolateCoverGrammar(this.parseLeftHandSideExpression)
      val args = if (this.`match`("(")) this.parseArguments() else Array()
      expr = new Node.NewExpression(callee, args)
      this.context.isAssignmentTarget = false
      this.context.isBindingElement = false
    }
    this.finalize(node, expr)
  }
  
  def parseAsyncArgument(): Node.ArgumentListElement = {
    val arg = this.parseAssignmentExpression()
    this.context.firstCoverInitializedNameError = null
    arg
  }
  
  def parseAsyncArguments(): Array[Node.ArgumentListElement] = {
    this.expect("(")
    val args = ArrayBuffer.empty[Node.ArgumentListElement]
    if (!this.`match`(")")) {
      breakable {
        while (true) {
          val expr = if (this.`match`("...")) this.parseSpreadElement() else this.isolateCoverGrammar(this.parseAsyncArgument)
          args.push(expr)
          if (this.`match`(")")) {
            break
          }
          this.expectCommaSeparator()
          if (this.`match`(")")) {
            break
          }
        }
      }
    }
    this.expect(")")
    args
  }
  
  def matchImportCall() = {
    var `match` = this.matchKeyword("import")
    if (`match`) {
      val state = this.scanner.saveState()
      this.scanner.scanComments()
      val next = this.scanner.lex()
      this.scanner.restoreState(state)
      `match` = next.`type` == Punctuator && next.value === "("
    }
    `match`
  }
  
  def parseImportCall(): Node.Import = {
    val node = this.createNode()
    this.expectKeyword("import")
    this.finalize(node, new Node.Import())
  }
  
  def parseLeftHandSideExpressionAllowCall(): Node.Expression = {
    val startToken = this.lookahead
    val maybeAsync = this.matchContextualKeyword("async")
    val previousAllowIn = this.context.allowIn
    this.context.allowIn = true
    var expr: Marker = null
    var exprNode: Node.Expression = null
    if (this.matchKeyword("super") && this.context.inFunctionBody) {
      expr = this.createNode()
      this.nextToken()
      exprNode = this.finalize(expr, new Node.Super())
      if (!this.`match`("(") && !this.`match`(".") && !this.`match`("[")) {
        this.throwUnexpectedToken(this.lookahead)
      }
    } else {
      exprNode = this.inheritCoverGrammar(if (this.matchKeyword("new")) this.parseNewExpression else this.parsePrimaryExpression)
    }
    breakable {
      while (true) {
        if (this.`match`(".")) {
          this.context.isBindingElement = false
          this.context.isAssignmentTarget = true
          this.expect(".")
          val property = this.parseIdentifierName()
          exprNode = this.finalize(this.startNode(startToken), new Node.StaticMemberExpression(exprNode, property))
        } else if (this.`match`("(")) {
          val asyncArrow = maybeAsync && startToken.lineNumber == this.lookahead.lineNumber
          this.context.isBindingElement = false
          this.context.isAssignmentTarget = false
          val args = if (asyncArrow) this.parseAsyncArguments() else this.parseArguments()
          if (expr.isInstanceOf[Node.Import] && args.length != 1) {
            this.tolerateError(Messages.BadImportCallArity)
          }
          exprNode = this.finalize(this.startNode(startToken), new Node.CallExpression(exprNode, args))
          if (asyncArrow && this.`match`("=>")) {
            val argsReinterpreted = for (i <- args) yield {
              this.reinterpretExpressionAsArrayPattern(i).asInstanceOf[Node.ArgumentListElement]
            }
            exprNode = new Node.ArrowParameterPlaceHolder {
              params = ArrayBuffer(argsReinterpreted: _*)
              async = true
            }
          }
        } else if (this.`match`("[")) {
          this.context.isBindingElement = false
          this.context.isAssignmentTarget = true
          this.expect("[")
          val property = this.isolateCoverGrammar(this.parseExpression)
          this.expect("]")
          exprNode = this.finalize(this.startNode(startToken), new Node.ComputedMemberExpression(exprNode, property))
        } else if (this.lookahead.`type` == Template && this.lookahead.head) {
          val quasi = this.parseTemplateLiteral()
          exprNode = this.finalize(this.startNode(startToken), new Node.TaggedTemplateExpression(exprNode, quasi))
        } else {
          break
        }
      }
    }
    this.context.allowIn = previousAllowIn
    exprNode
  }
  
  def parseSuper(): Node.Super = {
    val node = this.createNode()
    this.expectKeyword("super")
    if (!this.`match`("[") && !this.`match`(".")) {
      this.throwUnexpectedToken(this.lookahead)
    }
    this.finalize(node, new Node.Super())
  }
  
  def parseLeftHandSideExpression(): Node.Expression = {
    assert(this.context.allowIn, "callee of new expression always allow in keyword.")
    val node = this.startNode(this.lookahead)
    var expr = if (this.matchKeyword("super") && this.context.inFunctionBody) this.parseSuper() else this.inheritCoverGrammar(if (this.matchKeyword("new")) this.parseNewExpression else this.parsePrimaryExpression)
    breakable {
      while (true) {
        if (this.`match`("[")) {
          this.context.isBindingElement = false
          this.context.isAssignmentTarget = true
          this.expect("[")
          val property = this.isolateCoverGrammar(this.parseExpression)
          this.expect("]")
          expr = this.finalize(node, new Node.ComputedMemberExpression(expr, property))
        } else if (this.`match`(".")) {
          this.context.isBindingElement = false
          this.context.isAssignmentTarget = true
          this.expect(".")
          val property = this.parseIdentifierName()
          expr = this.finalize(node, new Node.StaticMemberExpression(expr, property))
        } else if (this.lookahead.`type` == Template && this.lookahead.head) {
          val quasi = this.parseTemplateLiteral()
          expr = this.finalize(node, new Node.TaggedTemplateExpression(expr, quasi))
        } else {
          break
        }
      }
    }
    expr
  }
  
  def parseUpdateExpression() = {
    var expr: Node.Expression = null
    val startToken = this.lookahead
    if (this.`match`("++") || this.`match`("--")) {
      val node = this.startNode(startToken)
      val token = this.nextToken()
      expr = this.inheritCoverGrammar(this.parseUnaryExpression)
      if (this.context.strict && expr.isInstanceOf[Node.Identifier] && this.scanner.isRestrictedWord(expr.asInstanceOf[Node.Identifier].name)) {
        this.tolerateError(Messages.StrictLHSPrefix)
      }
      if (!this.context.isAssignmentTarget) {
        this.tolerateError(Messages.InvalidLHSInAssignment)
      }
      val prefix = true
      expr = this.finalize(node, new Node.UpdateExpression(token.value, expr, prefix))
      this.context.isAssignmentTarget = false
      this.context.isBindingElement = false
    } else {
      expr = this.inheritCoverGrammar(this.parseLeftHandSideExpressionAllowCall)
      if (!this.hasLineTerminator && this.lookahead.`type` == Punctuator) {
        if (this.`match`("++") || this.`match`("--")) {
          if (this.context.strict && expr.isInstanceOf[Node.Identifier] && this.scanner.isRestrictedWord(expr.asInstanceOf[Node.Identifier].name)) {
            this.tolerateError(Messages.StrictLHSPostfix)
          }
          if (!this.context.isAssignmentTarget) {
            this.tolerateError(Messages.InvalidLHSInAssignment)
          }
          this.context.isAssignmentTarget = false
          this.context.isBindingElement = false
          val operator = this.nextToken().value
          val prefix = false
          expr = this.finalize(this.startNode(startToken), new Node.UpdateExpression(operator, expr, prefix))
        }
      }
    }
    expr
  }
  
  def parseAwaitExpression(): Node.AwaitExpression = {
    val node = this.createNode()
    this.nextToken()
    val argument = this.parseUnaryExpression()
    this.finalize(node, new Node.AwaitExpression(argument))
  }
  
  def parseUnaryExpression(): Node.Expression = {
    var expr: Node.Expression = null
    if (this.`match`("+") || this.`match`("-") || this.`match`("~") || this.`match`("!") || this.matchKeyword("delete") || this.matchKeyword("void") || this.matchKeyword("typeof")) {
      val node = this.startNode(this.lookahead)
      val token = this.nextToken()
      expr = this.inheritCoverGrammar(this.parseUnaryExpression)
      val exprOp = this.finalize(node, new Node.UnaryExpression(token.value, expr))
      expr = exprOp
      if (this.context.strict && exprOp.operator == "delete" && exprOp.argument.isInstanceOf[Node.Identifier]) {
        this.tolerateError(Messages.StrictDelete)
      }
      this.context.isAssignmentTarget = false
      this.context.isBindingElement = false
    } else if (this.context.await && this.matchContextualKeyword("await")) {
      expr = this.parseAwaitExpression()
    } else {
      expr = this.parseUpdateExpression()
    }
    expr
  }
  
  def parseExponentiationExpression(): Node.Expression = {
    val startToken = this.lookahead
    var expr = this.inheritCoverGrammar(this.parseUnaryExpression)
    if (expr.`type` != Syntax.UnaryExpression && this.`match`("**")) {
      this.nextToken()
      this.context.isAssignmentTarget = false
      this.context.isBindingElement = false
      val left = expr
      val right = this.isolateCoverGrammar(this.parseExponentiationExpression)
      expr = this.finalize(this.startNode(startToken), new Node.BinaryExpression("**", left, right))
    }
    expr
  }
  
  def binaryPrecedence(token: RawToken): Int = {
    val op: String = token.value
    var precedence: Int = 0
    if (token.`type` == Punctuator) {
      precedence = this.operatorPrecedence(op)
    } else if (token.`type` == Keyword) {
      precedence = if (op == "instanceof" || this.context.allowIn && op == "in") 7 else 0
    } else {
      precedence = 0
    }
    precedence
  }
  
  def parseBinaryExpression(): Node.Expression = {
    val startToken = this.lookahead
    var expr = this.inheritCoverGrammar(this.parseExponentiationExpression)
    val token = this.lookahead
    var prec = this.binaryPrecedence(token)
    if (prec > 0) {
      this.nextToken()
      this.context.isAssignmentTarget = false
      this.context.isBindingElement = false
      val markers = ArrayBuffer(startToken, this.lookahead)
      var left = expr
      var right = this.isolateCoverGrammar(this.parseExponentiationExpression)
      val stack = ArrayBuffer(left, token.value, right)
      val precedences = ArrayBuffer(prec)
      breakable {
        while (true) {
          prec = this.binaryPrecedence(this.lookahead)
          if (prec <= 0) {
            break
          }
          // Reduce: make a binary expression from the three topmost entries.
          while (stack.length > 2 && prec <= precedences(precedences.length - 1)) {
            right = stack.pop().asInstanceOf[Node.Expression]
            val operator: String = stack.pop().asInstanceOf[OrType]
            precedences.pop()
            left = stack.pop().asInstanceOf[Node.Expression]
            markers.pop()
            val node = this.startNode(markers(markers.length - 1))
            stack.push(this.finalize(node, new Node.BinaryExpression(operator, left, right)))
          }
          // Shift.
          stack.push(this.nextToken().value)
          precedences.push(prec)
          markers.push(this.lookahead)
          stack.push(this.isolateCoverGrammar(this.parseExponentiationExpression))
        }
      }
      // Final reduce to clean-up the stack.
      var i = stack.length - 1
      expr = stack(i).asInstanceOf[Node.Expression]
      var lastMarker = markers.pop()
      while (i > 1) {
        val marker = markers.pop()
        val lastLineStart = if (lastMarker) lastMarker.lineStart else 0
        val node = this.startNode(marker, lastLineStart)
        val operator: String = stack(i - 1).asInstanceOf[OrType]
        expr = this.finalize(node, new Node.BinaryExpression(operator, stack(i - 2).asInstanceOf[Node.Expression], expr))
        i -= 2
        lastMarker = marker
      }
    }
    expr
  }
  
  def parseConditionalExpression(): Node.Expression = {
    val startToken = this.lookahead
    var expr = this.inheritCoverGrammar(this.parseBinaryExpression)
    if (this.`match`("?")) {
      this.nextToken()
      val previousAllowIn = this.context.allowIn
      this.context.allowIn = true
      val consequent = this.isolateCoverGrammar(this.parseAssignmentExpression)
      this.context.allowIn = previousAllowIn
      this.expect(":")
      val alternate = this.isolateCoverGrammar(this.parseAssignmentExpression)
      expr = this.finalize(this.startNode(startToken), new Node.ConditionalExpression(expr, consequent, alternate))
      this.context.isAssignmentTarget = false
      this.context.isBindingElement = false
    }
    expr
  }
  
  def checkPatternParam(options: ParameterOptions, param: Node.Node): Unit = {
    param match {
      case param: Node.Identifier =>
        this.validateParam(options, ??? /* param*/, param.name)
      case param: Node.RestElement =>
        this.checkPatternParam(options, param.argument)
      case param: Node.AssignmentPattern =>
        this.checkPatternParam(options, param.left)
      case param: Node.ArrayPattern =>
        for (i <- param.elements) {
          if (i != null) {
            this.checkPatternParam(options, i)
          }
        }
      case param: Node.ObjectPattern =>
        for (property <- param.properties) {
          this.checkPatternParam(options, if (property.isInstanceOf[Node.RestElement]) property else property.asInstanceOf[Node.Property].value)
        }
      case _ =>
    }
    options.simple = options.simple && param.isInstanceOf[Node.Identifier]
  }
  
  def reinterpretAsCoverFormalsList(expr: Node.ArrowParameterPlaceHolder): ParameterOptions = {
    var paramsSource = ArrayBuffer(expr.params:_*)
    var paramsTarget = new ArrayBuffer[Node.FunctionParameter](paramsSource.size)
    var asyncArrow = expr.async
    object options extends ParameterOptions {
      simple = true
      paramSet = mutable.Map.empty
    }
    for (i <- paramsSource.indices) {
      val param = paramsSource(i)
      val paramReinterpreted = (param: @unchecked) match {
        case param_cast: Node.AssignmentPattern =>
          if (param_cast.right.isInstanceOf[Node.YieldExpression]) {
            val right_cast = param_cast.right.asInstanceOf[Node.YieldExpression]
            if (right_cast.argument) {
              this.throwUnexpectedToken(this.lookahead)
            }
            val rightAdapted = new Node.Identifier("yield")
            param_cast.right = rightAdapted
          }
          param_cast
        case param_cast: Node.FunctionParameter =>
          if (asyncArrow && param.isInstanceOf[Node.Identifier] && param.asInstanceOf[Node.Identifier].name == "await") {
            this.throwUnexpectedToken(this.lookahead)
          }
          param_cast
        //case _ =>
      }
      this.checkPatternParam(options, paramReinterpreted)
      paramsTarget.push(paramReinterpreted)
    }
    if (this.context.strict || !this.context.allowYield) {
      for (param <- paramsTarget) {
        param match {
          case param_cast: Node.YieldExpression =>
            this.throwUnexpectedToken(this.lookahead)
          case _ =>
        }
      }
    }
    if (options.message == Messages.StrictParamDupe) {
      val token = if (this.context.strict) options.stricted else options.firstRestricted
      this.throwUnexpectedToken(token, options.message)
    }
    val _params = paramsTarget
    new ParameterOptions {
      simple = options.simple
      params = _params
      stricted = options.stricted
      firstRestricted = options.firstRestricted
      message = options.message
    }
  }

  /* called when result may be a normal expression or assignment expression */
  def parseAssignmentExpression(): Node.Expression = {
    var exprTemp: Node.Expression = null
    if (!this.context.allowYield && this.matchKeyword("yield")) {
      this.parseYieldExpression()
    } else {
      val startToken = this.lookahead
      var token = startToken
      exprTemp = this.parseConditionalExpression()
      if (token.`type` == Identifier && token.lineNumber == this.lookahead.lineNumber && token.value === "async") {
        if (this.lookahead.`type` == Identifier || this.matchKeyword("yield")) {
          val arg = this.parsePrimaryExpression()
          val argT = this.reinterpretExpressionAsArrayPattern(arg).asInstanceOf[Node.ArgumentListElement]
          exprTemp = new Node.ArrowParameterPlaceHolder {
            params = ArrayBuffer(argT)
            async = true
          }
        }
      }
      if (exprTemp.isInstanceOf[Node.ArrowParameterPlaceHolder] || this.`match`("=>")) {
        val expr_cast = exprTemp.asInstanceOf[Node.ArrowParameterPlaceHolder]
        // https://tc39.github.io/ecma262/#sec-arrow-function-definitions
        this.context.isAssignmentTarget = false
        this.context.isBindingElement = false
        val isAsync = expr_cast.async
        val list = this.reinterpretAsCoverFormalsList(expr_cast)
        if (list) {
          if (this.hasLineTerminator) {
            this.tolerateUnexpectedToken(this.lookahead)
          }
          this.context.firstCoverInitializedNameError = null
          val previousStrict = this.context.strict
          val previousAllowStrictDirective = this.context.allowStrictDirective
          this.context.allowStrictDirective = list.simple
          val previousAllowYield = this.context.allowYield
          val previousAwait = this.context.await
          this.context.allowYield = true
          this.context.await = isAsync
          val node = this.startNode(startToken)
          this.expect("=>")
          var body: Node.BlockStatementOrExpression = null
          if (this.`match`("{")) {
            val previousAllowIn = this.context.allowIn
            this.context.allowIn = true
            body = this.parseFunctionSourceElements()
            this.context.allowIn = previousAllowIn
          } else {
            body = this.isolateCoverGrammar(this.parseAssignmentExpression)
          }
          val expression = body.`type` != Syntax.BlockStatement
          if (this.context.strict && list.firstRestricted) {
            this.throwUnexpectedToken(list.firstRestricted, list.message)
          }
          if (this.context.strict && list.stricted) {
            this.tolerateUnexpectedToken(list.stricted, list.message)
          }
          this.context.strict = previousStrict
          this.context.allowStrictDirective = previousAllowStrictDirective
          this.context.allowYield = previousAllowYield
          this.context.await = previousAwait
          if (isAsync) this.finalize(node, new Node.AsyncArrowFunctionExpression(list.params, body, expression)) else this.finalize(node, new Node.ArrowFunctionExpression(list.params, body, expression))
        } else {
          exprTemp
        }
      } else {
        if (this.matchAssign()) {
          if (!this.context.isAssignmentTarget) {
            this.tolerateError(Messages.InvalidLHSInAssignment)
          }
          if (this.context.strict && exprTemp.isInstanceOf[Node.Identifier]) {
            val expr_cast = exprTemp.asInstanceOf[Node.Identifier]
            val id = expr_cast
            if (this.scanner.isRestrictedWord(id.name)) {
              this.tolerateUnexpectedToken(token, Messages.StrictLHSAssignment)
            }
            if (this.scanner.isStrictModeReservedWord(id.name)) {
              this.tolerateUnexpectedToken(token, Messages.StrictReservedWord)
            }
          }
          if (!this.`match`("=")) {
            this.context.isAssignmentTarget = false
            this.context.isBindingElement = false
          } else {
            exprTemp = this.reinterpretExpressionAsArrayPattern(exprTemp).asInstanceOf[Node.Expression]
          }
          token = this.nextToken()
          val operator = token.value
          val right = this.isolateCoverGrammar(this.parseAssignmentExpression)
          this.context.firstCoverInitializedNameError = null
          this.finalize(this.startNode(startToken), new Node.AssignmentExpression(operator, exprTemp, right))
        } else {
          exprTemp
        }
      }
    }
  }

  def parseExpression(): Node.Expression = {
    val startToken = this.lookahead
    var expr: Node.Expression = this.isolateCoverGrammar(this.parseAssignmentExpression)
    if (this.`match`(",")) {
      val expressions = ArrayBuffer.empty[Node.Expression]
      expressions.push(expr)
      breakable {
        while (this.lookahead.`type` != EOF) {
          if (!this.`match`(",")) {
            break
          }
          this.nextToken()
          expressions.push(this.isolateCoverGrammar(this.parseAssignmentExpression))
        }
      }
      expr = this.finalize(this.startNode(startToken), new Node.SequenceExpression(expressions))
    }
    expr
  }
  
  def parseStatementListItem(): Node.StatementListItem = {
    var statement: Node.StatementListItem = null
    this.context.isAssignmentTarget = true
    this.context.isBindingElement = true
    if (this.lookahead.`type` == Keyword) {
      this.lookahead.value.get[String] match {
        case "export" =>
          if (!this.context.isModule) {
            this.tolerateUnexpectedToken(this.lookahead, Messages.IllegalExportDeclaration)
          }
          statement = this.parseExportDeclaration()
        case "import" =>
          if (this.matchImportCall()) {
            statement = this.parseExpressionStatement()
          } else {
            if (!this.context.isModule) {
              this.tolerateUnexpectedToken(this.lookahead, Messages.IllegalImportDeclaration)
            }
            statement = this.parseImportDeclaration()
          }
        case "const" =>
          statement = this.parseLexicalDeclaration(new VariableOptions {
            inFor = false
          })
        case "function" =>
          statement = this.parseFunctionDeclaration()
        case "class" =>
          statement = this.parseClassDeclaration()
        case "let" =>
          statement = if (this.isLexicalDeclaration()) this.parseLexicalDeclaration(new VariableOptions {
            inFor = false
          }) else this.parseStatement()
        case _ =>
          statement = this.parseStatement()
      }
    } else {
      statement = this.parseStatement()
    }
    statement
  }
  
  def parseBlock(): Node.BlockStatement = {
    val node = this.createNode()
    this.expect("{")
    val block = ArrayBuffer.empty[Node.StatementListItem]
    breakable {
      while (true) {
        if (this.`match`("}")) {
          break
        }
        block.push(this.parseStatementListItem())
      }
    }
    this.expect("}")
    this.finalize(node, new Node.BlockStatement(block))
  }
  
  def parseLexicalBinding(kind: String, options: VariableOptions): Node.VariableDeclarator = {
    val node = this.createNode()
    val params = ArrayBuffer.empty[RawToken]
    val id = this.parsePattern(params, kind)
    if (this.context.strict && id.isInstanceOf[Node.Identifier]) {
      val id_cast = id.asInstanceOf[Node.Identifier]
      if (this.scanner.isRestrictedWord(id_cast.name)) {
        this.tolerateError(Messages.StrictVarName)
      }
    }
    var init: Node.Expression = null
    if (kind == "const") {
      if (!this.matchKeyword("in") && !this.matchContextualKeyword("of")) {
        if (this.`match`("=")) {
          this.nextToken()
          init = this.isolateCoverGrammar(this.parseAssignmentExpression)
        } else {
          this.throwError(Messages.DeclarationMissingInitializer, "const")
        }
      }
    } else if (!options.inFor && id.`type` != Syntax.Identifier || this.`match`("=")) {
      this.expect("=")
      init = this.isolateCoverGrammar(this.parseAssignmentExpression)
    }
    this.finalize(node, new Node.VariableDeclarator(id, init))
  }
  
  def parseBindingList(kind: String, options: VariableOptions): Array[Node.VariableDeclarator] = {
    val list = ArrayBuffer(this.parseLexicalBinding(kind, options))
    while (this.`match`(",")) {
      this.nextToken()
      list.push(this.parseLexicalBinding(kind, options))
    }
    list
  }
  
  def isLexicalDeclaration(): Boolean = {
    val state = this.scanner.saveState()
    this.scanner.scanComments()
    val next = this.scanner.lex()
    this.scanner.restoreState(state)
    next.`type` == Identifier || next.`type` == Punctuator && next.value === "[" || next.`type` == Punctuator && next.value === "{" || next.`type` == Keyword && next.value === "let" || next.`type` == Keyword && next.value === "yield"
  }
  
  def parseLexicalDeclaration(options: VariableOptions): Node.VariableDeclaration = {
    val node = this.createNode()
    val kind: String = this.nextToken().value
    assert(kind == "let" || kind == "const", "Lexical declaration must be either let or const")
    val declarations = this.parseBindingList(kind, options)
    this.consumeSemicolon()
    this.finalize(node, new Node.VariableDeclaration(declarations, kind))
  }
  
  def parseBindingRestElement(params: ArrayBuffer[RawToken], kind: String): Node.RestElement = {
    val node = this.createNode()
    this.expect("...")
    val arg = this.parsePattern(params, kind)
    this.finalize(node, new Node.RestElement(arg))
  }
  
  def parseArrayPattern(params: ArrayBuffer[RawToken], kind: String): Node.ArrayPattern = {
    val node = this.createNode()
    this.expect("[")
    val elements = ArrayBuffer.empty[Node.ArrayPatternElement]
    breakable {
      while (!this.`match`("]")) {
        if (this.`match`(",")) {
          this.nextToken()
          elements.push(null)
        } else {
          if (this.`match`("...")) {
            elements.push(this.parseBindingRestElement(params, kind))
            break
          } else {
            elements.push(this.parsePatternWithDefault(params, kind).asInstanceOf[Node.ArrayPatternElement])
          }
          if (!this.`match`("]")) {
            this.expect(",")
          }
        }
      }
    }
    this.expect("]")
    this.finalize(node, new Node.ArrayPattern(elements))
  }
  
  def parsePropertyPattern(params: ArrayBuffer[RawToken], kind: String): Node.Property = {
    val node = this.createNode()
    var computed = false
    var shorthand = false
    val method = false
    var key: Node.PropertyKey = null
    var value: Node.PropertyValue = null
    if (this.lookahead.`type` == Identifier) {
      val keyToken = this.lookahead
      key = this.parseVariableIdentifier()
      val init = this.finalize(node, new Node.Identifier(keyToken.value))
      if (this.`match`("=")) {
        params.push(keyToken)
        shorthand = true
        this.nextToken()
        val expr = this.parseAssignmentExpression()
        value = this.finalize(this.startNode(keyToken), new Node.AssignmentPattern(init, expr))
      } else if (!this.`match`(":")) {
        params.push(keyToken)
        shorthand = true
        value = init
      } else {
        this.expect(":")
        value = this.parsePatternWithDefault(params, kind).asInstanceOf[Node.PropertyValue]
      }
    } else {
      computed = this.`match`("[")
      key = this.parseObjectPropertyKey()
      this.expect(":")
      value = this.parsePatternWithDefault(params, kind).asInstanceOf[Node.PropertyValue]
    }
    this.finalize(node, new Node.Property("init", key, computed, value, method, shorthand))
  }
  
  def parseRestProperty(params: ArrayBuffer[RawToken], kind: String): Node.RestElement = {
    val node = this.createNode()
    this.expect("...")
    val arg = this.parsePattern(params)
    if (this.`match`("=")) {
      this.throwError(Messages.DefaultRestProperty)
    }
    if (!this.`match`("}")) {
      this.throwError(Messages.PropertyAfterRestProperty)
    }
    this.finalize(node, new Node.RestElement(arg))
  }
  
  def parseObjectPattern(params: ArrayBuffer[RawToken], kind: String): Node.ObjectPattern = {
    val node = this.createNode()
    val properties = ArrayBuffer.empty[Node.ObjectPatternProperty]
    this.expect("{")
    while (!this.`match`("}")) {
      properties.push(if (this.`match`("...")) this.parseRestProperty(params, kind) else this.parsePropertyPattern(params, kind))
      if (!this.`match`("}")) {
        this.expect(",")
      }
    }
    this.expect("}")
    this.finalize(node, new Node.ObjectPattern(properties))
  }
  
  def parsePattern(params: ArrayBuffer[RawToken], kind: String = ""): Node.BindingIdentifierOrPattern = {
    var pattern: Node.BindingIdentifierOrPattern = null
    if (this.`match`("[")) {
      pattern = this.parseArrayPattern(params, kind)
    } else if (this.`match`("{")) {
      pattern = this.parseObjectPattern(params, kind)
    } else {
      if (this.matchKeyword("let") && (kind == "const" || kind == "let")) {
        this.tolerateUnexpectedToken(this.lookahead, Messages.LetInLexicalBinding)
      }
      params.push(this.lookahead)
      pattern = this.parseVariableIdentifier(kind)
    }
    pattern
  }
  
  def parsePatternWithDefault(params: ArrayBuffer[RawToken], kind: String = ""): Node.BindingIdentifierOrPattern = {
    val startToken = this.lookahead
    var pattern = this.parsePattern(params, kind)
    if (this.`match`("=")) {
      this.nextToken()
      val previousAllowYield = this.context.allowYield
      this.context.allowYield = true
      val right = this.isolateCoverGrammar(this.parseAssignmentExpression)
      this.context.allowYield = previousAllowYield
      this.finalize[Node.BindingIdentifierOrPattern](this.startNode(startToken), new Node.AssignmentPattern(pattern, right))
    } else {
      pattern.asInstanceOf[Node.BindingIdentifier]
    }
  }
  
  def parseVariableIdentifier(kind: String = ""): Node.Identifier = {
    val node = this.createNode()
    val token = this.nextToken()
    if (token.`type` == Keyword && token.value === "yield") {
      if (this.context.strict) {
        this.tolerateUnexpectedToken(token, Messages.StrictReservedWord)
      } else if (!this.context.allowYield) {
        this.throwUnexpectedToken(token)
      }
    } else if (token.`type` != Identifier) {
      if (this.context.strict && token.`type` == Keyword && this.scanner.isStrictModeReservedWord(token.value)) {
        this.tolerateUnexpectedToken(token, Messages.StrictReservedWord)
      } else {
        if (this.context.strict || (token.value !== "let") || kind != "var") {
          this.throwUnexpectedToken(token)
        }
      }
    } else if ((this.context.isModule || this.context.await) && token.`type` == Identifier && token.value === "await") {
      this.tolerateUnexpectedToken(token)
    }
    this.finalize(node, new Node.Identifier(token.value))
  }
  
  def parseVariableDeclaration(options: VariableOptions): Node.VariableDeclarator = {
    val node = this.createNode()
    val params = ArrayBuffer.empty[RawToken]
    val id = this.parsePattern(params, "var")
    if (this.context.strict && id.isInstanceOf[Node.Identifier]) {
      val id_cast = id.asInstanceOf[Node.Identifier]
      if (this.scanner.isRestrictedWord(id_cast.name)) {
        this.tolerateError(Messages.StrictVarName)
      }
    }
    var init: Node.Expression = null
    if (this.`match`("=")) {
      this.nextToken()
      init = this.isolateCoverGrammar(this.parseAssignmentExpression)
    } else if (id.`type` != Syntax.Identifier && !options.inFor) {
      this.expect("=")
    }
    this.finalize(node, new Node.VariableDeclarator(id, init))
  }
  
  def parseVariableDeclarationList(options: VariableOptions) = {
    object opt extends VariableOptions{
      inFor = options.inFor
    }
    val list = ArrayBuffer.empty[Node.VariableDeclarator]
    list.push(this.parseVariableDeclaration(opt))
    while (this.`match`(",")) {
      this.nextToken()
      list.push(this.parseVariableDeclaration(opt))
    }
    list
  }
  
  def parseVariableStatement(): Node.VariableDeclaration = {
    val node = this.createNode()
    this.expectKeyword("var")
    val declarations = this.parseVariableDeclarationList(new VariableOptions {
      inFor = false
    })
    this.consumeSemicolon()
    this.finalize(node, new Node.VariableDeclaration(declarations, "var"))
  }
  
  def parseEmptyStatement(): Node.EmptyStatement = {
    val node = this.createNode()
    this.expect(";")
    this.finalize(node, new Node.EmptyStatement())
  }
  
  def parseExpressionStatement(): Node.ExpressionStatement = {
    val node = this.createNode()
    val expr = this.parseExpression()
    this.consumeSemicolon()
    this.finalize(node, new Node.ExpressionStatement(expr))
  }
  
  def parseIfClause(): Node.Statement = {
    if (this.context.strict && this.matchKeyword("function")) {
      this.tolerateError(Messages.StrictFunction)
    }
    this.parseStatement()
  }
  
  def parseIfStatement(): Node.IfStatement = {
    val node = this.createNode()
    var consequent: Node.Statement = null
    var alternate: Node.Statement = null
    this.expectKeyword("if")
    this.expect("(")
    val test = this.parseExpression()
    if (!this.`match`(")") && this.config.tolerant) {
      this.tolerateUnexpectedToken(this.nextToken())
      consequent = this.finalize(this.createNode(), new Node.EmptyStatement())
    } else {
      this.expect(")")
      consequent = this.parseIfClause()
      if (this.matchKeyword("else")) {
        this.nextToken()
        alternate = this.parseIfClause()
      }
    }
    this.finalize(node, new Node.IfStatement(test, consequent, alternate))
  }
  
  def parseDoWhileStatement() = {
    val node = this.createNode()
    this.expectKeyword("do")
    val previousInIteration = this.context.inIteration
    this.context.inIteration = true
    val body = this.parseStatement()
    this.context.inIteration = previousInIteration
    this.expectKeyword("while")
    this.expect("(")
    val test = this.parseExpression()
    if (!this.`match`(")") && this.config.tolerant) {
      this.tolerateUnexpectedToken(this.nextToken())
    } else {
      this.expect(")")
      if (this.`match`(";")) {
        this.nextToken()
      }
    }
    this.finalize(node, new Node.DoWhileStatement(body, test))
  }
  
  def parseWhileStatement() = {
    val node = this.createNode()
    var body: Node.Statement = null
    this.expectKeyword("while")
    this.expect("(")
    val test = this.parseExpression()
    if (!this.`match`(")") && this.config.tolerant) {
      this.tolerateUnexpectedToken(this.nextToken())
      body = this.finalize(this.createNode(), new Node.EmptyStatement())
    } else {
      this.expect(")")
      val previousInIteration = this.context.inIteration
      this.context.inIteration = true
      body = this.parseStatement()
      this.context.inIteration = previousInIteration
    }
    this.finalize(node, new Node.WhileStatement(test, body))
  }
  
  def parseForStatement() = {
    var init: Marker = null
    var test: Marker = null
    var update: Marker = null
    var initNode: Node.ExpressionOrStatement = null
    var testNode: Node.Expression = null
    var updateNode: Node.Expression = null
    var forIn = true
    var left: Node.ExpressionOrStatement = null
    var right: Node.Expression = null
    val node = this.createNode()
    this.expectKeyword("for")
    this.expect("(")
    if (this.`match`(";")) {
      this.nextToken()
    } else {
      if (this.matchKeyword("var")) {
        init = this.createNode()
        this.nextToken()
        val previousAllowIn = this.context.allowIn
        this.context.allowIn = false
        val declarations = this.parseVariableDeclarationList(new VariableOptions {
          inFor = true
        })
        this.context.allowIn = previousAllowIn
        if (declarations.length == 1 && this.matchKeyword("in")) {
          val decl = declarations(0)
          if (decl.init && (decl.id.isInstanceOf[Node.ArrayPattern] || decl.id.isInstanceOf[Node.ObjectPattern] || this.context.strict)) {
            this.tolerateError(Messages.ForInOfLoopInitializer, "for-in")
          }
          initNode = this.finalize(init, new Node.VariableDeclaration(declarations, "var"))
          this.nextToken()
          left = initNode
          right = this.parseExpression()
          init = null
        } else if (declarations.length == 1 && declarations(0).init == null && this.matchContextualKeyword("of")) {
          initNode = this.finalize(init, new Node.VariableDeclaration(declarations, "var"))
          this.nextToken()
          left = initNode
          right = this.parseAssignmentExpression()
          init = null
          forIn = false
        } else {
          initNode = this.finalize(init, new Node.VariableDeclaration(declarations, "var"))
          this.expect(";")
        }
      } else if (this.matchKeyword("const") || this.matchKeyword("let")) {
        init = this.createNode()
        val kind = this.nextToken().value
        if (!this.context.strict && this.lookahead.value === "in") {
          initNode = this.finalize(init, new Node.Identifier(kind))
          this.nextToken()
          left = initNode
          right = this.parseExpression()
          init = null
        } else {
          val previousAllowIn = this.context.allowIn
          this.context.allowIn = false
          val declarations = this.parseBindingList(kind, new VariableOptions {
            inFor = true
          })
          this.context.allowIn = previousAllowIn
          if (declarations.length == 1 && declarations(0).init == null && this.matchKeyword("in")) {
            initNode = this.finalize(init, new Node.VariableDeclaration(declarations, kind))
            this.nextToken()
            left = initNode
            right = this.parseExpression()
            init = null
          } else if (declarations.length == 1 && declarations(0).init == null && this.matchContextualKeyword("of")) {
            initNode = this.finalize(init, new Node.VariableDeclaration(declarations, kind))
            this.nextToken()
            left = initNode
            right = this.parseAssignmentExpression()
            init = null
            forIn = false
          } else {
            this.consumeSemicolon()
            initNode = this.finalize(init, new Node.VariableDeclaration(declarations, kind))
          }
        }
      } else {
        val initStartToken = this.lookahead
        val previousAllowIn = this.context.allowIn
        this.context.allowIn = false
        initNode = this.inheritCoverGrammar(this.parseAssignmentExpression)
        this.context.allowIn = previousAllowIn
        if (this.matchKeyword("in")) {
          if (!this.context.isAssignmentTarget || init.isInstanceOf[Node.AssignmentExpression]) {
            this.tolerateError(Messages.InvalidLHSInForIn)
          }
          this.nextToken()
          initNode = this.reinterpretExpressionAsArrayPattern(initNode).asInstanceOf[Node.ExpressionOrStatement]
          left = initNode
          right = this.parseExpression()
          init = null
        } else if (this.matchContextualKeyword("of")) {
          if (!this.context.isAssignmentTarget || init.isInstanceOf[Node.AssignmentExpression]) {
            this.tolerateError(Messages.InvalidLHSInForLoop)
          }
          this.nextToken()
          initNode = this.reinterpretExpressionAsArrayPattern(initNode).asInstanceOf[Node.ExpressionOrStatement]
          left = initNode
          right = this.parseAssignmentExpression()
          init = null
          forIn = false
        } else {
          if (this.`match`(",")) {
            val initSeq = ArrayBuffer[Node.Expression](initNode.asInstanceOf[Node.Expression])
            while (this.`match`(",")) {
              this.nextToken()
              initSeq.push(this.isolateCoverGrammar(this.parseAssignmentExpression))
            }
            initNode = this.finalize(this.startNode(initStartToken), new Node.SequenceExpression(initSeq))
          }
          this.expect(";")
        }
      }
    }
    if (left == null) {
      if (!this.`match`(";")) {
        testNode = this.parseExpression()
      }
      this.expect(";")
      if (!this.`match`(")")) {
        updateNode = this.parseExpression()
      }
    }
    var body: Node.Statement = null
    if (!this.`match`(")") && this.config.tolerant) {
      this.tolerateUnexpectedToken(this.nextToken())
      body = this.finalize(this.createNode(), new Node.EmptyStatement())
    } else {
      this.expect(")")
      val previousInIteration = this.context.inIteration
      this.context.inIteration = true
      body = this.isolateCoverGrammar(this.parseStatement)
      this.context.inIteration = previousInIteration
    }
    if (left == null) this.finalize(node, new Node.ForStatement(initNode, testNode, updateNode, body)) else if (forIn) this.finalize(node, new Node.ForInStatement(left, right, body)) else this.finalize(node, new Node.ForOfStatement(left, right, body))
  }
  
  def parseContinueStatement(): Node.ContinueStatement = {
    val node = this.createNode()
    this.expectKeyword("continue")
    var label: Node.Identifier = null
    if (this.lookahead.`type` == Identifier && !this.hasLineTerminator) {
      val id = this.parseVariableIdentifier()
      label = id
      val key = "$" + id.name
      if (!(this.context.labelSet contains key)) {
        this.throwError(Messages.UnknownLabel, id.name)
      }
    }
    this.consumeSemicolon()
    if (label == null && !this.context.inIteration) {
      this.throwError(Messages.IllegalContinue)
    }
    this.finalize(node, new Node.ContinueStatement(label))
  }
  
  def parseBreakStatement(): Node.BreakStatement = {
    val node = this.createNode()
    this.expectKeyword("break")
    var label: Node.Identifier = null
    if (this.lookahead.`type` == Identifier && !this.hasLineTerminator) {
      val id = this.parseVariableIdentifier()
      val key = "$" + id.name
      if (!(this.context.labelSet contains key)) {
        this.throwError(Messages.UnknownLabel, id.name)
      }
      label = id
    }
    this.consumeSemicolon()
    if (label == null && !this.context.inIteration && !this.context.inSwitch) {
      this.throwError(Messages.IllegalBreak)
    }
    this.finalize(node, new Node.BreakStatement(label))
  }
  
  def parseReturnStatement(): Node.ReturnStatement = {
    if (!this.context.inFunctionBody) {
      this.tolerateError(Messages.IllegalReturn)
    }
    val node = this.createNode()
    this.expectKeyword("return")
    val hasArgument = !this.`match`(";") && !this.`match`("}") && !this.hasLineTerminator && this.lookahead.`type` != EOF || this.lookahead.`type` == Template
    val argument = if (hasArgument) this.parseExpression() else null
    this.consumeSemicolon()
    this.finalize(node, new Node.ReturnStatement(argument))
  }
  
  def parseWithStatement() = {
    if (this.context.strict) {
      this.tolerateError(Messages.StrictModeWith)
    }
    val node = this.createNode()
    var body: Node.Statement = null
    this.expectKeyword("with")
    this.expect("(")
    val `object` = this.parseExpression()
    if (!this.`match`(")") && this.config.tolerant) {
      this.tolerateUnexpectedToken(this.nextToken())
      body = this.finalize(this.createNode(), new Node.EmptyStatement())
    } else {
      this.expect(")")
      body = this.parseStatement()
    }
    this.finalize(node, new Node.WithStatement(`object`, body))
  }
  
  def parseSwitchCase(): Node.SwitchCase = {
    val node = this.createNode()
    var test: Node.Expression = null
    if (this.matchKeyword("default")) {
      this.nextToken()
      test = null
    } else {
      this.expectKeyword("case")
      test = this.parseExpression()
    }
    this.expect(":")
    val consequent = ArrayBuffer.empty[Node.Statement]
    breakable {
      while (true) {
        if (this.`match`("}") || this.matchKeyword("default") || this.matchKeyword("case")) {
          break
        }
        consequent.push(this.parseStatementListItem().asInstanceOf[Node.Statement])
      }
    }
    this.finalize(node, new Node.SwitchCase(test, consequent))
  }
  
  def parseSwitchStatement(): Node.SwitchStatement = {
    val node = this.createNode()
    this.expectKeyword("switch")
    this.expect("(")
    val discriminant = this.parseExpression()
    this.expect(")")
    val previousInSwitch = this.context.inSwitch
    this.context.inSwitch = true
    val cases = ArrayBuffer.empty[Node.SwitchCase]
    var defaultFound = false
    this.expect("{")
    breakable {
      while (true) {
        if (this.`match`("}")) {
          break
        }
        val clause = this.parseSwitchCase()
        if (clause.test == null) {
          if (defaultFound) {
            this.throwError(Messages.MultipleDefaultsInSwitch)
          }
          defaultFound = true
        }
        cases.push(clause)
      }
    }
    this.expect("}")
    this.context.inSwitch = previousInSwitch
    this.finalize(node, new Node.SwitchStatement(discriminant, cases))
  }
  
  def parseLabelledStatement(): Node.Statement = {
    val node = this.createNode()
    val expr = this.parseExpression()
    var statement: Node.Statement = null
    if (expr.isInstanceOf[Node.Identifier] && this.`match`(":")) {
      val expr_cast = expr.asInstanceOf[Node.Identifier]
      this.nextToken()
      val id = expr_cast
      val key = "$" + id.name
      if (this.context.labelSet contains key) {
        this.throwError(Messages.Redeclaration, "Label", id.name)
      }
      this.context.labelSet(key) = true
      var body: Node.Statement = null
      if (this.matchKeyword("class")) {
        this.tolerateUnexpectedToken(this.lookahead)
        body = this.parseClassDeclaration()
      } else if (this.matchKeyword("function")) {
        val token = this.lookahead
        val declaration = this.parseFunctionDeclaration()
        if (this.context.strict) {
          this.tolerateUnexpectedToken(token, Messages.StrictFunction)
        } else if (declaration.generator) {
          this.tolerateUnexpectedToken(token, Messages.GeneratorInLegacyContext)
        }
        body = declaration
      } else {
        body = this.parseStatement()
      }
      this.context.labelSet -= key
      statement = new Node.LabeledStatement(id, body)
    } else {
      this.consumeSemicolon()
      statement = new Node.ExpressionStatement(expr)
    }
    this.finalize(node, statement)
  }
  
  def parseThrowStatement(): Node.ThrowStatement = {
    val node = this.createNode()
    this.expectKeyword("throw")
    if (this.hasLineTerminator) {
      this.throwError(Messages.NewlineAfterThrow)
    }
    val argument = this.parseExpression()
    this.consumeSemicolon()
    this.finalize(node, new Node.ThrowStatement(argument))
  }
  
  def parseCatchClause(): Node.CatchClause = {
    val node = this.createNode()
    this.expectKeyword("catch")
    this.expect("(")
    if (this.`match`(")")) {
      this.throwUnexpectedToken(this.lookahead)
    }
    val params = ArrayBuffer.empty[RawToken]
    val param = this.parsePattern(params)
    val paramMap = mutable.Map.empty[String, Boolean]
    for (i <- params) {
      val key = "$" + i.value
      if (paramMap contains key) {
        this.tolerateError(Messages.DuplicateBinding, i.value)
      }
      paramMap(key) = true
    }
    if (this.context.strict && param.isInstanceOf[Node.Identifier]) {
      val param_cast = param.asInstanceOf[Node.Identifier]
      if (this.scanner.isRestrictedWord(param_cast.name)) {
        this.tolerateError(Messages.StrictCatchVariable)
      }
    }
    this.expect(")")
    val body = this.parseBlock()
    this.finalize(node, new Node.CatchClause(param, body))
  }
  
  def parseFinallyClause(): Node.BlockStatement = {
    this.expectKeyword("finally")
    this.parseBlock()
  }
  
  def parseTryStatement(): Node.TryStatement = {
    val node = this.createNode()
    this.expectKeyword("try")
    val block = this.parseBlock()
    val handler = if (this.matchKeyword("catch")) this.parseCatchClause() else null
    val finalizer = if (this.matchKeyword("finally")) this.parseFinallyClause() else null
    if (!handler && !finalizer) {
      this.throwError(Messages.NoCatchOrFinally)
    }
    this.finalize(node, new Node.TryStatement(block, handler, finalizer))
  }
  
  def parseDebuggerStatement(): Node.DebuggerStatement = {
    val node = this.createNode()
    this.expectKeyword("debugger")
    this.consumeSemicolon()
    this.finalize(node, new Node.DebuggerStatement())
  }
  
  def parseStatement(): Node.Statement = {
    var statement: Node.Statement = null
    this.lookahead.`type` match {
      case BooleanLiteral | NullLiteral | NumericLiteral | StringLiteral | Template | RegularExpression =>
        statement = this.parseExpressionStatement()
      case Punctuator =>
        val value: String = this.lookahead.value
        if (value == "{") {
          statement = this.parseBlock()
        } else if (value == "(") {
          statement = this.parseExpressionStatement()
        } else if (value == ";") {
          statement = this.parseEmptyStatement()
        } else {
          statement = this.parseExpressionStatement()
        }
      case Identifier =>
        statement = if (this.matchAsyncFunction()) this.parseFunctionDeclaration() else this.parseLabelledStatement()
      case Keyword =>
        this.lookahead.value.get[String] match {
          case "break" =>
            statement = this.parseBreakStatement()
          case "continue" =>
            statement = this.parseContinueStatement()
          case "debugger" =>
            statement = this.parseDebuggerStatement()
          case "do" =>
            statement = this.parseDoWhileStatement()
          case "for" =>
            statement = this.parseForStatement()
          case "function" =>
            statement = this.parseFunctionDeclaration()
          case "if" =>
            statement = this.parseIfStatement()
          case "return" =>
            statement = this.parseReturnStatement()
          case "switch" =>
            statement = this.parseSwitchStatement()
          case "throw" =>
            statement = this.parseThrowStatement()
          case "try" =>
            statement = this.parseTryStatement()
          case "var" =>
            statement = this.parseVariableStatement()
          case "while" =>
            statement = this.parseWhileStatement()
          case "with" =>
            statement = this.parseWithStatement()
          case _ =>
            statement = this.parseExpressionStatement()
        }
      case _ =>
        statement = this.throwUnexpectedToken(this.lookahead)
    }
    statement
  }
  
  def parseFunctionSourceElements() = {
    val node = this.createNode()
    this.expect("{")
    val body = this.parseDirectivePrologues()
    val previousLabelSet = this.context.labelSet
    val previousInIteration = this.context.inIteration
    val previousInSwitch = this.context.inSwitch
    val previousInFunctionBody = this.context.inFunctionBody
    this.context.labelSet = mutable.Map.empty
    this.context.inIteration = false
    this.context.inSwitch = false
    this.context.inFunctionBody = true
    breakable {
      while (this.lookahead.`type` != EOF) {
        if (this.`match`("}")) {
          break
        }
        body.push(this.parseStatementListItem())
      }
    }
    this.expect("}")
    this.context.labelSet = previousLabelSet
    this.context.inIteration = previousInIteration
    this.context.inSwitch = previousInSwitch
    this.context.inFunctionBody = previousInFunctionBody
    this.finalize(node, new Node.BlockStatement(body))
  }
  
  def validateParam(options: ParameterOptions, param: RawToken, name: String) = {
    val key = "$" + name
    if (this.context.strict) {
      if (this.scanner.isRestrictedWord(name)) {
        options.stricted = param
        options.message = Messages.StrictParamName
      }
      if (options.paramSet contains key) {
        options.stricted = param
        options.message = Messages.StrictParamDupe
      }
    } else if (!options.firstRestricted) {
      if (this.scanner.isRestrictedWord(name)) {
        options.firstRestricted = param
        options.message = Messages.StrictParamName
      } else if (this.scanner.isStrictModeReservedWord(name)) {
        options.firstRestricted = param
        options.message = Messages.StrictReservedWord
      } else if (options.paramSet contains key) {
        options.stricted = param
        options.message = Messages.StrictParamDupe
      }
    }
    options.paramSet(key) = true
  }
  
  def parseRestElement(params: ArrayBuffer[RawToken]): Node.RestElement = {
    val node = this.createNode()
    this.expect("...")
    val arg = this.parsePattern(params)
    if (this.`match`("=")) {
      this.throwError(Messages.DefaultRestParameter)
    }
    if (!this.`match`(")")) {
      this.throwError(Messages.ParameterAfterRestParameter)
    }
    this.finalize(node, new Node.RestElement(arg))
  }
  
  def parseFormalParameter(options: ParameterOptions) = {
    val params = ArrayBuffer.empty[RawToken]
    val param = if (this.`match`("...")) this.parseRestElement(params) else this.parsePatternWithDefault(params)
    for (i <- params) {
      this.validateParam(options, i, i.value)
    }
    options.simple = options.simple && param.isInstanceOf[Node.Identifier]
    options.params.push(param.asInstanceOf[Node.FunctionParameter])
  }

  def parseFormalParameters(firstRestricted_ : RawToken = null): ParameterOptions = {
    object options extends ParameterOptions {
      simple = true
      params = ArrayBuffer()
      firstRestricted = firstRestricted_
    }
    this.expect("(")
    if (!this.`match`(")")) {
      options.paramSet = mutable.Map.empty
      breakable {
        while (this.lookahead.`type` != EOF) {
          this.parseFormalParameter(options)
          if (this.`match`(")")) {
            break
          }
          this.expect(",")
          if (this.`match`(")")) {
            break
          }
        }
      }
    }
    this.expect(")")
    new ParameterOptions {
      simple = options.simple
      params = options.params
      stricted = options.stricted
      firstRestricted = options.firstRestricted
      message = options.message
    }
  }
  
  def matchAsyncFunction() = {
    var `match` = this.matchContextualKeyword("async")
    if (`match`) {
      val state = this.scanner.saveState()
      this.scanner.scanComments()
      val next = this.scanner.lex()
      this.scanner.restoreState(state)
      `match` = state.lineNumber == next.lineNumber && next.`type` == Keyword && next.value === "function"
    }
    `match`
  }
  
  def parseFunctionDeclaration(identifierIsOptional: Boolean = false): Node.AFunctionDeclaration = {
    val node = this.createNode()
    val isAsync = this.matchContextualKeyword("async")
    if (isAsync) {
      this.nextToken()
    }
    this.expectKeyword("function")
    val isGenerator = if (isAsync) false else this.`match`("*")
    if (isGenerator) {
      this.nextToken()
    }
    var message: String = null
    var id: Node.Identifier = null
    var firstRestricted: RawToken = null
    if (!identifierIsOptional || !this.`match`("(")) {
      val token = this.lookahead
      id = this.parseVariableIdentifier()
      if (this.context.strict) {
        if (this.scanner.isRestrictedWord(token.value)) {
          this.tolerateUnexpectedToken(token, Messages.StrictFunctionName)
        }
      } else {
        if (this.scanner.isRestrictedWord(token.value)) {
          firstRestricted = token
          message = Messages.StrictFunctionName
        } else if (this.scanner.isStrictModeReservedWord(token.value)) {
          firstRestricted = token
          message = Messages.StrictReservedWord
        }
      }
    }
    val previousAllowAwait = this.context.await
    val previousAllowYield = this.context.allowYield
    this.context.await = isAsync
    this.context.allowYield = !isGenerator
    val formalParameters = this.parseFormalParameters(firstRestricted)
    val params = formalParameters.params
    val stricted = formalParameters.stricted
    firstRestricted = formalParameters.firstRestricted
    if (formalParameters.message) {
      message = formalParameters.message
    }
    val previousStrict = this.context.strict
    val previousAllowStrictDirective = this.context.allowStrictDirective
    this.context.allowStrictDirective = formalParameters.simple
    val body = this.parseFunctionSourceElements()
    if (this.context.strict && firstRestricted) {
      this.throwUnexpectedToken(firstRestricted, message)
    }
    if (this.context.strict && stricted) {
      this.tolerateUnexpectedToken(stricted, message)
    }
    this.context.strict = previousStrict
    this.context.allowStrictDirective = previousAllowStrictDirective
    this.context.await = previousAllowAwait
    this.context.allowYield = previousAllowYield
    if (isAsync) this.finalize(node, new Node.AsyncFunctionDeclaration(id, params, body)) else this.finalize(node, new Node.FunctionDeclaration(id, params, body, isGenerator))
  }
  
  def parseFunctionExpression() = {
    val node = this.createNode()
    val isAsync = this.matchContextualKeyword("async")
    if (isAsync) {
      this.nextToken()
    }
    this.expectKeyword("function")
    val isGenerator = if (isAsync) false else this.`match`("*")
    if (isGenerator) {
      this.nextToken()
    }
    var message: String = null
    var id: Node.Identifier = null
    var firstRestricted: RawToken = null
    val previousAllowAwait = this.context.await
    val previousAllowYield = this.context.allowYield
    this.context.await = isAsync
    this.context.allowYield = !isGenerator
    if (!this.`match`("(")) {
      val token = this.lookahead
      id = if (!this.context.strict && !isGenerator && this.matchKeyword("yield")) this.parseIdentifierName() else this.parseVariableIdentifier()
      if (this.context.strict) {
        if (this.scanner.isRestrictedWord(token.value)) {
          this.tolerateUnexpectedToken(token, Messages.StrictFunctionName)
        }
      } else {
        if (this.scanner.isRestrictedWord(token.value)) {
          firstRestricted = token
          message = Messages.StrictFunctionName
        } else if (this.scanner.isStrictModeReservedWord(token.value)) {
          firstRestricted = token
          message = Messages.StrictReservedWord
        }
      }
    }
    val formalParameters = this.parseFormalParameters(firstRestricted)
    val params = formalParameters.params
    val stricted = formalParameters.stricted
    firstRestricted = formalParameters.firstRestricted
    if (formalParameters.message) {
      message = formalParameters.message
    }
    val previousStrict = this.context.strict
    val previousAllowStrictDirective = this.context.allowStrictDirective
    this.context.allowStrictDirective = formalParameters.simple
    val body = this.parseFunctionSourceElements()
    if (this.context.strict && firstRestricted) {
      this.throwUnexpectedToken(firstRestricted, message)
    }
    if (this.context.strict && stricted) {
      this.tolerateUnexpectedToken(stricted, message)
    }
    this.context.strict = previousStrict
    this.context.allowStrictDirective = previousAllowStrictDirective
    this.context.await = previousAllowAwait
    this.context.allowYield = previousAllowYield
    if (isAsync) this.finalize(node, new Node.AsyncFunctionExpression(id, params, body)) else this.finalize(node, new Node.FunctionExpression(id, params, body, isGenerator))
  }
  
  def parseDirective(): Node.StatementListItem = {
    val token = this.lookahead
    val node = this.createNode()
    val expr = this.parseExpression()
    val directive = if (expr.isInstanceOf[Node.Literal]) this.getTokenRaw(token).slice(1, -1) else null
    this.consumeSemicolon()
    this.finalize(node, if (directive) new Node.Directive(expr, directive) else new Node.ExpressionStatement(expr))
  }
  
  def parseDirectivePrologues(): ArrayBuffer[Node.StatementListItem] = {
    var firstRestricted: RawToken = null
    val body = ArrayBuffer.empty[Node.StatementListItem]
    breakable {
      while (true) {
        val token = this.lookahead
        if (token.`type` != StringLiteral) {
          break
        }
        val statement = this.parseDirective()
        body.push(statement)
        val directive = statement match {
          case d: Node.Directive =>
            d.directive
          case _ =>
            break
        }
        if (directive == "use strict") {
          this.context.strict = true
          if (firstRestricted) {
            this.tolerateUnexpectedToken(firstRestricted, Messages.StrictOctalLiteral)
          }
          if (!this.context.allowStrictDirective) {
            this.tolerateUnexpectedToken(token, Messages.IllegalLanguageModeDirective)
          }
        } else {
          if (!firstRestricted && token.octal) {
            firstRestricted = token
          }
        }
      }
    }
    body
  }
  
  def qualifiedPropertyName(token: RawToken): Boolean = {
    token.`type` match {
      case Identifier | StringLiteral | BooleanLiteral | NullLiteral | NumericLiteral | Keyword =>
        return true
      case Punctuator =>
        return token.value === "["
      case _ =>
    }
    false
  }
  
  def parseGetterMethod(): Node.FunctionExpression = {
    val node = this.createNode()
    val isGenerator = false
    val previousAllowYield = this.context.allowYield
    this.context.allowYield = !isGenerator
    val formalParameters = this.parseFormalParameters()
    if (formalParameters.params.length > 0) {
      this.tolerateError(Messages.BadGetterArity)
    }
    val method = this.parsePropertyMethod(formalParameters)
    this.context.allowYield = previousAllowYield
    this.finalize(node, new Node.FunctionExpression(null, formalParameters.params, method, isGenerator))
  }
  
  def parseSetterMethod() = {
    val node = this.createNode()
    val isGenerator = false
    val previousAllowYield = this.context.allowYield
    this.context.allowYield = !isGenerator
    val formalParameters = this.parseFormalParameters()
    if (formalParameters.params.length != 1) {
      this.tolerateError(Messages.BadSetterArity)
    } else if (formalParameters.params(0).isInstanceOf[Node.RestElement]) {
      this.tolerateError(Messages.BadSetterRestParameter)
    }
    val method = this.parsePropertyMethod(formalParameters)
    this.context.allowYield = previousAllowYield
    this.finalize(node, new Node.FunctionExpression(null, formalParameters.params, method, isGenerator))
  }
  
  def parseGeneratorMethod() = {
    val node = this.createNode()
    val isGenerator = true
    val previousAllowYield = this.context.allowYield
    this.context.allowYield = true
    val params = this.parseFormalParameters()
    this.context.allowYield = false
    val method = this.parsePropertyMethod(params)
    this.context.allowYield = previousAllowYield
    this.finalize(node, new Node.FunctionExpression(null, params.params, method, isGenerator))
  }
  
  def isStartOfExpression() = {
    var start = true
    val value: String = this.lookahead.value
    this.lookahead.`type` match {
      case Punctuator =>
        start = value == "[" || value == "(" || value == "{" || value == "+" || value == "-" || value == "!" || value == "~" || value == "++" || value == "--" || value == "/" || value == "/="
      case Keyword =>
        start = value == "class" || value == "delete" || value == "function" || value == "let" || value == "new" || value == "super" || value == "this" || value == "typeof" || value == "void" || value == "yield"
      case _ =>
    }
    start
  }
  
  def parseYieldExpression(): Node.YieldExpression = {
    val node = this.createNode()
    this.expectKeyword("yield")
    var argument: Node.Expression = null
    var delegate = false
    if (!this.hasLineTerminator) {
      val previousAllowYield = this.context.allowYield
      this.context.allowYield = false
      delegate = this.`match`("*")
      if (delegate) {
        this.nextToken()
        argument = this.parseAssignmentExpression()
      } else if (this.isStartOfExpression()) {
        argument = this.parseAssignmentExpression()
      }
      this.context.allowYield = previousAllowYield
    }
    this.finalize(node, new Node.YieldExpression(argument, delegate))
  }
  
  def parseClassElement(hasConstructor: ByRef[Boolean]): Node.MethodDefinition = {
    var token = this.lookahead
    val node = this.createNode()
    var kind: String = null
    var key: Node.PropertyKey = null
    var value: Node.PropertyValue = null
    var computed = false
    var method = false
    var isStatic = false
    var isAsync = false
    if (this.`match`("*")) {
      this.nextToken()
    } else {
      computed = this.`match`("[")
      key = this.parseObjectPropertyKey()
      val id = key.asInstanceOf[Node.Identifier]
      if (id.name == "static" && (this.qualifiedPropertyName(this.lookahead) || this.`match`("*"))) {
        token = this.lookahead
        isStatic = true
        computed = this.`match`("[")
        if (this.`match`("*")) {
          this.nextToken()
        } else {
          key = this.parseObjectPropertyKey()
        }
      }
      if (token.`type` == Identifier && !this.hasLineTerminator && token.value === "async") {
        val punctuator: String = this.lookahead.value
        if (punctuator != ":" && punctuator != "(" && punctuator != "*") {
          isAsync = true
          token = this.lookahead
          key = this.parseObjectPropertyKey()
          if (token.`type` == Identifier) {
            if (token.value === "get" || token.value === "set") {
              this.tolerateUnexpectedToken(token)
            } else if (token.value === "constructor") {
              this.tolerateUnexpectedToken(token, Messages.ConstructorIsAsync)
            }
          }
        }
      }
    }
    val lookaheadPropertyKey = this.qualifiedPropertyName(this.lookahead)
    if (token.`type` == Identifier) {
      if (token.value === "get" && lookaheadPropertyKey) {
        kind = "get"
        computed = this.`match`("[")
        key = this.parseObjectPropertyKey()
        this.context.allowYield = false
        value = this.parseGetterMethod()
      } else if (token.value === "set" && lookaheadPropertyKey) {
        kind = "set"
        computed = this.`match`("[")
        key = this.parseObjectPropertyKey()
        value = this.parseSetterMethod()
      }
    } else if (token.`type` == Punctuator && token.value === "*" && lookaheadPropertyKey) {
      kind = "init"
      computed = this.`match`("[")
      key = this.parseObjectPropertyKey()
      value = this.parseGeneratorMethod()
      method = true
    }
    if (!kind && key && this.`match`("(")) {
      kind = "init"
      value = if (isAsync) this.parsePropertyMethodAsyncFunction() else this.parsePropertyMethodFunction()
      method = true
    }
    if (!kind) {
      this.throwUnexpectedToken(this.lookahead)
    }
    if (kind == "init") {
      kind = "method"
    }
    if (!computed) {
      if (isStatic && this.isPropertyKey(key, "prototype")) {
        this.throwUnexpectedToken(token, Messages.StaticPrototype)
      }
      if (!isStatic && this.isPropertyKey(key, "constructor")) {
        if (kind != "method" || !method || value.isInstanceOf[Node.HasGenerator] && value.asInstanceOf[Node.HasGenerator].generator) {
          this.throwUnexpectedToken(token, Messages.ConstructorSpecialMethod)
        }
        if (hasConstructor.value) {
          this.throwUnexpectedToken(token, Messages.DuplicateConstructor)
        } else {
          hasConstructor.value = true
        }
        kind = "constructor"
      }
    }
    this.finalize(node, new Node.MethodDefinition(key, computed, value, kind, isStatic))
  }
  
  def parseClassElementList(): Array[Node.MethodDefinition] = {
    val body = ArrayBuffer.empty[Node.MethodDefinition]
    object hasConstructor extends ByRef[Boolean](false)
    this.expect("{")
    while (!this.`match`("}")) {
      if (this.`match`(";")) {
        this.nextToken()
      } else {
        body.push(this.parseClassElement(hasConstructor))
      }
    }
    this.expect("}")
    body
  }
  
  def parseClassBody(): Node.ClassBody = {
    val node = this.createNode()
    val elementList = this.parseClassElementList()
    this.finalize(node, new Node.ClassBody(elementList))
  }
  
  def parseClassDeclaration(identifierIsOptional: Boolean = false): Node.ClassDeclaration = {
    val node = this.createNode()
    val previousStrict = this.context.strict
    this.context.strict = true
    this.expectKeyword("class")
    val id = if (identifierIsOptional && this.lookahead.`type` != Identifier) null else this.parseVariableIdentifier()
    var superClass: Node.Identifier = null
    if (this.matchKeyword("extends")) {
      this.nextToken()
      superClass = this.isolateCoverGrammar(this.parseLeftHandSideExpressionAllowCall).asInstanceOf[Node.Identifier]
    }
    val classBody = this.parseClassBody()
    this.context.strict = previousStrict
    this.finalize(node, new Node.ClassDeclaration(id, superClass, classBody))
  }
  
  def parseClassExpression(): Node.ClassExpression = {
    val node = this.createNode()
    val previousStrict = this.context.strict
    this.context.strict = true
    this.expectKeyword("class")
    val id = if (this.lookahead.`type` == Identifier) this.parseVariableIdentifier() else null
    var superClass: Node.Identifier = null
    if (this.matchKeyword("extends")) {
      this.nextToken()
      superClass = this.isolateCoverGrammar(this.parseLeftHandSideExpressionAllowCall).asInstanceOf[Node.Identifier]
    }
    val classBody = this.parseClassBody()
    this.context.strict = previousStrict
    this.finalize(node, new Node.ClassExpression(id, superClass, classBody))
  }
  
  def parseModule(): Node.Module = {
    this.context.strict = true
    this.context.isModule = true
    this.scanner.isModule = true
    val node = this.createNode()
    val body = this.parseDirectivePrologues()
    while (this.lookahead.`type` != EOF) {
      body.push(this.parseStatementListItem())
    }
    this.finalize(node, new Node.Module(body))
  }
  
  def parseScript(): Node.Script = {
    val node = this.createNode()
    val body = this.parseDirectivePrologues()
    while (this.lookahead.`type` != EOF) {
      body.push(this.parseStatementListItem())
    }
    this.finalize(node, new Node.Script(body))
  }
  
  def parseModuleSpecifier(): Node.Literal = {
    val node = this.createNode()
    if (this.lookahead.`type` != StringLiteral) {
      this.throwError(Messages.InvalidModuleSpecifier)
    }
    val token = this.nextToken()
    val raw = this.getTokenRaw(token)
    this.finalize(node, new Node.Literal(token.value, raw))
  }
  
  def parseImportSpecifier(): Node.ImportSpecifier = {
    val node = this.createNode()
    var imported: Node.Identifier = null
    var local: Node.Identifier = null
    if (this.lookahead.`type` == Identifier) {
      imported = this.parseVariableIdentifier()
      local = imported
      if (this.matchContextualKeyword("as")) {
        this.nextToken()
        local = this.parseVariableIdentifier()
      }
    } else {
      imported = this.parseIdentifierName()
      local = imported
      if (this.matchContextualKeyword("as")) {
        this.nextToken()
        local = this.parseVariableIdentifier()
      } else {
        this.throwUnexpectedToken(this.nextToken())
      }
    }
    this.finalize(node, new Node.ImportSpecifier(local, imported))
  }
  
  def parseNamedImports(): ArrayBuffer[Node.ImportSpecifier] = {
    this.expect("{")
    val specifiers = ArrayBuffer.empty[Node.ImportSpecifier]
    while (!this.`match`("}")) {
      specifiers.push(this.parseImportSpecifier())
      if (!this.`match`("}")) {
        this.expect(",")
      }
    }
    this.expect("}")
    specifiers
  }
  
  def parseImportDefaultSpecifier() = {
    val node = this.createNode()
    val local = this.parseIdentifierName()
    this.finalize(node, new Node.ImportDefaultSpecifier(local))
  }
  
  def parseImportNamespaceSpecifier() = {
    val node = this.createNode()
    this.expect("*")
    if (!this.matchContextualKeyword("as")) {
      this.throwError(Messages.NoAsAfterImportNamespace)
    }
    this.nextToken()
    val local = this.parseIdentifierName()
    this.finalize(node, new Node.ImportNamespaceSpecifier(local))
  }
  
  def parseImportDeclaration(): Node.ImportDeclaration = {
    if (this.context.inFunctionBody) {
      this.throwError(Messages.IllegalImportDeclaration)
    }
    val node = this.createNode()
    this.expectKeyword("import")
    var src: Node.Literal = null
    var specifiers = ArrayBuffer.empty[Node.ImportDeclarationSpecifier]
    if (this.lookahead.`type` == StringLiteral) {
      // import 'foo';
      src = this.parseModuleSpecifier()
    } else {
      if (this.`match`("{")) {
        // import {bar}
        specifiers = specifiers.concat(this.parseNamedImports())
      } else if (this.`match`("*")) {
        // import * as foo
        specifiers.push(this.parseImportNamespaceSpecifier())
      } else if (this.isIdentifierName(this.lookahead) && !this.matchKeyword("default")) {
        // import foo
        specifiers.push(this.parseImportDefaultSpecifier())
        if (this.`match`(",")) {
          this.nextToken()
          if (this.`match`("*")) {
            // import foo, * as foo
            specifiers.push(this.parseImportNamespaceSpecifier())
          } else if (this.`match`("{")) {
            // import foo, {bar}
            specifiers = specifiers.concat(this.parseNamedImports())
          } else {
            this.throwUnexpectedToken(this.lookahead)
          }
        }
      } else {
        this.throwUnexpectedToken(this.nextToken())
      }
      if (!this.matchContextualKeyword("from")) {
        val message = if (this.lookahead.value) Messages.UnexpectedToken else Messages.MissingFromClause
        this.throwError(message, this.lookahead.value)
      }
      this.nextToken()
      src = this.parseModuleSpecifier()
    }
    this.consumeSemicolon()
    this.finalize(node, new Node.ImportDeclaration(specifiers, src))
  }
  
  def parseExportSpecifier(): Node.ExportSpecifier = {
    val node = this.createNode()
    val local = this.parseIdentifierName()
    var exported = local
    if (this.matchContextualKeyword("as")) {
      this.nextToken()
      exported = this.parseIdentifierName()
    }
    this.finalize(node, new Node.ExportSpecifier(local, exported))
  }
  
  def parseExportDeclaration(): Node.ExportDeclaration = {
    if (this.context.inFunctionBody) {
      this.throwError(Messages.IllegalExportDeclaration)
    }
    val node = this.createNode()
    this.expectKeyword("export")
    var exportDeclaration: Node.ExportDeclaration = null
    if (this.matchKeyword("default")) {
      // export default ...
      this.nextToken()
      if (this.matchKeyword("function")) {
        // export default function foo () {}
        // export default function () {}
        val declaration = this.parseFunctionDeclaration(true)
        exportDeclaration = this.finalize(node, new Node.ExportDefaultDeclaration(declaration))
      } else if (this.matchKeyword("class")) {
        // export default class foo {}
        val declaration = this.parseClassDeclaration(true)
        exportDeclaration = this.finalize(node, new Node.ExportDefaultDeclaration(declaration))
      } else if (this.matchContextualKeyword("async")) {
        // export default async function f () {}
        // export default async function () {}
        // export default async x => x
        val declaration = if (this.matchAsyncFunction()) this.parseFunctionDeclaration(true) else this.parseAssignmentExpression()
        exportDeclaration = this.finalize(node, new Node.ExportDefaultDeclaration(declaration))
      } else {
        if (this.matchContextualKeyword("from")) {
          this.throwError(Messages.UnexpectedToken, this.lookahead.value)
        }
        // export default {};
        // export default [];
        // export default (1 + 2);
        val declaration = if (this.`match`("{")) this.parseObjectInitializer() else if (this.`match`("[")) this.parseArrayInitializer() else this.parseAssignmentExpression()
        this.consumeSemicolon()
        exportDeclaration = this.finalize(node, new Node.ExportDefaultDeclaration(declaration))
      }
    } else if (this.`match`("*")) {
      // export * from 'foo';
      this.nextToken()
      if (!this.matchContextualKeyword("from")) {
        val message = if (this.lookahead.value) Messages.UnexpectedToken else Messages.MissingFromClause
        this.throwError(message, this.lookahead.value)
      }
      this.nextToken()
      val src = this.parseModuleSpecifier()
      this.consumeSemicolon()
      exportDeclaration = this.finalize(node, new Node.ExportAllDeclaration(src))
    } else if (this.lookahead.`type` == Keyword) {
      // export var f = 1;
      var declaration: Node.ExportableNamedDeclaration = null
      this.lookahead.value.get[String] match {
        case "let" | "const" =>
          declaration = this.parseLexicalDeclaration(new VariableOptions {
            inFor = false
          })
        case "var" | "class" | "function" =>
          declaration = this.parseStatementListItem()
        case _ =>
          this.throwUnexpectedToken(this.lookahead)
      }
      exportDeclaration = this.finalize(node, new Node.ExportNamedDeclaration(declaration, Seq(), null))
    } else if (this.matchAsyncFunction()) {
      val declaration = this.parseFunctionDeclaration()
      exportDeclaration = this.finalize(node, new Node.ExportNamedDeclaration(declaration, Seq(), null))
    } else {
      val specifiers = ArrayBuffer.empty[Node.ExportSpecifier]
      var source: Node.Literal = null
      var isExportFromIdentifier = false
      this.expect("{")
      while (!this.`match`("}")) {
        isExportFromIdentifier = isExportFromIdentifier || this.matchKeyword("default")
        specifiers.push(this.parseExportSpecifier())
        if (!this.`match`("}")) {
          this.expect(",")
        }
      }
      this.expect("}")
      if (this.matchContextualKeyword("from")) {
        // export {default} from 'foo';
        // export {foo} from 'foo';
        this.nextToken()
        source = this.parseModuleSpecifier()
        this.consumeSemicolon()
      } else if (isExportFromIdentifier) {
        // export {default}; // missing fromClause
        val message = if (this.lookahead.value) Messages.UnexpectedToken else Messages.MissingFromClause
        this.throwError(message, this.lookahead.value)
      } else {
        // export {foo};
        this.consumeSemicolon()
      }
      exportDeclaration = this.finalize(node, new Node.ExportNamedDeclaration(null, specifiers, source))
    }
    exportDeclaration
  }
  
}



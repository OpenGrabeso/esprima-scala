/*
ScalaFromJS: Dev
parser.ts
*/

package com.github.opengrabeso.esprima

/* import { assert } from './assert' */
/* import { ErrorHandler } from './error-handler' */
/* import { Messages } from './messages' */
/* import * as Node from './nodes' */
/* import { Comment, RawToken, Scanner, SourceLocation } from './scanner' */
/* import { Syntax } from './syntax' */
/* import { Token, TokenName } from './token' */
trait Config {
  var range: Boolean = _
  var loc: Boolean = _
  var source: String = _
  var tokens: Boolean = _
  var comment: Boolean = _
  var tolerant: Boolean = _
}

trait Context {
  var isModule: Boolean = _
  var allowIn: Boolean = _
  var allowStrictDirective: Boolean = _
  var allowYield: Boolean = _
  var await: Boolean = _
  var firstCoverInitializedNameError: RawToken = _
  var isAssignmentTarget: Boolean = _
  var isBindingElement: Boolean = _
  var inFunctionBody: Boolean = _
  var inIteration: Boolean = _
  var inSwitch: Boolean = _
  var labelSet = Map.empty[String, Boolean]
  var strict: Boolean = _
}

trait Marker {
  var index: Double = _
  var line: Double = _
  var column: Double = _
}

val ArrowParameterPlaceHolder = "ArrowParameterPlaceHolder"

trait ArrowParameterPlaceHolderNode {
  var `type`: String = _
  var params = Array.empty[Node.Expression]
  var async: Boolean = _
}

trait DeclarationOptions {
  var inFor: Boolean = _
}

trait TokenEntry {
  var `type`: String = _
  var value: String = _
  var regex = new {}
  var range = Array.empty[Double]
  var loc: SourceLocation = _
}

class Parser(code: String, options: Any = new {}, var delegate: (AnyRef, AnyRef) => Any) {
  var tokens = Array.empty[Any]
  var config: Config = new {
    var range = options.range.getClass == "boolean" && options.range
    var loc = options.loc.getClass == "boolean" && options.loc
    var source = null
    var tokens = options.tokens.getClass == "boolean" && options.tokens
    var comment = options.comment.getClass == "boolean" && options.comment
    var tolerant = options.tolerant.getClass == "boolean" && options.tolerant
  }
  if (config.loc && options.source && options.source != null) {
    config.source = String(options.source)
  }
  var errorHandler: ErrorHandler = new ErrorHandler()
  errorHandler.tolerant = config.tolerant
  var scanner: Scanner = new Scanner(code, errorHandler)
  scanner.trackComment = config.comment
  var operatorPrecedence = new {
    var `)` = 0
    var `;` = 0
    var `,` = 0
    var `=` = 0
    var `]` = 0
    var `||` = 1
    var `&&` = 2
    var `|` = 3
    var `^` = 4
    var `&` = 5
    var `==` = 6
    var `!=` = 6
    var `===` = 6
    var `!==` = 6
    var `<` = 7
    var `>` = 7
    var `<=` = 7
    var `>=` = 7
    var `<<` = 8
    var `>>` = 8
    var `>>>` = 8
    var `+` = 9
    var `-` = 9
    var `*` = 11
    var `/` = 11
    var `%` = 11
  }
  var lookahead: RawToken = new {
    var `type` = Token.EOF
    var value = ""
    var lineNumber = scanner.lineNumber
    var lineStart = 0
    var start = 0
    var end = 0
  }
  var hasLineTerminator: Boolean = false
  var context: Context = new {
    var isModule = false
    var await = false
    var allowIn = true
    var allowStrictDirective = true
    var allowYield = true
    var firstCoverInitializedNameError = null
    var isAssignmentTarget = false
    var isBindingElement = false
    var inFunctionBody = false
    var inIteration = false
    var inSwitch = false
    var labelSet = new {}
    var strict = false
  }
  tokens = Array()
  var startMarker: Marker = new {
    var index = 0
    var line = scanner.lineNumber
    var column = 0
  }
  var lastMarker: Marker = new {
    var index = 0
    var line = scanner.lineNumber
    var column = 0
  }
  this.nextToken()
  lastMarker = new {
    var index = scanner.index
    var line = scanner.lineNumber
    var column = scanner.index - scanner.lineStart
  }
  def throwError(messageFormat: String, values: String | Double): Unit = {
    val args = Array.prototype.slice.call(arguments, 1)
    val msg = messageFormat.replace("/%(\d)/g".r, (whole, idx) => {
      assert(idx < args.length, "Message reference must be in range")
      return args(idx)
    }
    )
    val index = this.lastMarker.index
    val line = this.lastMarker.line
    val column = this.lastMarker.column + 1
    throw this.errorHandler.createError(index, line, column, msg)
  }
  
  def tolerateError(messageFormat: String, values: String) = {
    val args = Array.prototype.slice.call(arguments, 1)
    val msg = messageFormat.replace("/%(\d)/g".r, (whole, idx) => {
      assert(idx < args.length, "Message reference must be in range")
      return args(idx)
    }
    )
    val index = this.lastMarker.index
    val line = this.scanner.lineNumber
    val column = this.lastMarker.column + 1
    this.errorHandler.tolerateError(index, line, column, msg)
  }
  
  // Throw an exception because of the token.
  def unexpectedTokenError(token: RawToken, message: String): Error = {
    var msg = message || Messages.UnexpectedToken
    var value: String | Double = _
    if (token) {
      if (!message) {
        msg = if (token.`type` == Token.EOF) Messages.UnexpectedEOS else if (token.`type` == Token.Identifier) Messages.UnexpectedIdentifier else if (token.`type` == Token.NumericLiteral) Messages.UnexpectedNumber else if (token.`type` == Token.StringLiteral) Messages.UnexpectedString else if (token.`type` == Token.Template) Messages.UnexpectedTemplate else Messages.UnexpectedToken
        if (token.`type` == Token.Keyword) {
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
    if (token && token.lineNumber.getClass == "number") {
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
  
  def throwUnexpectedToken(token: RawToken, message: String): Nothing = {
    throw this.unexpectedTokenError(token, message)
  }
  
  def tolerateUnexpectedToken(token: RawToken, message: String) = {
    this.errorHandler.tolerate(this.unexpectedTokenError(token, message))
  }
  
  def collectComments() = {
    if (!this.config.comment) {
      this.scanner.scanComments()
    } else {
      val comments = this.scanner.scanComments()
      if (comments.length > 0 && this.delegate) {
        for (e <- comments) {
          object node {
            var `type` = if (e.multiLine) "BlockComment" else "LineComment"
            var value = this.scanner.source.slice(e.slice(0), e.slice(1))
          }
          if (this.config.range) {
            node.range = e.range
          }
          if (this.config.loc) {
            node.loc = e.loc
          }
          object metadata {
            var start = new {
              var line = e.loc.start.line
              var column = e.loc.start.column
              var offset = e.range(0)
            }
            var end = new {
              var line = e.loc.end.line
              var column = e.loc.end.column
              var offset = e.range(1)
            }
          }
          this.delegate(node, metadata)
        }
      }
    }
  }
  
  // From internal representation to an external structure
  def getTokenRaw(token: RawToken): String = {
    this.scanner.source.slice(token.start, token.end)
  }
  
  def convertToken(token: RawToken): TokenEntry = {
    object t extends TokenEntry {
      var `type` = TokenName(token.`type`)
      var value = this.getTokenRaw(token)
    }
    if (this.config.range) {
      t.range = Array(token.start, token.end)
    }
    if (this.config.loc) {
      t.loc = new {
        var start = new {
          var line = this.startMarker.line
          var column = this.startMarker.column
        }
        var end = new {
          var line = this.scanner.lineNumber
          var column = this.scanner.index - this.scanner.lineStart
        }
      }
    }
    if (token.`type` == Token.RegularExpression) {
      val pattern = token.pattern.asInstanceOf[String]
      val flags = token.flags.asInstanceOf[String]
      t.regex = new {
        var pattern = pattern
        var flags = flags
      }
    }
    t
  }
  
  def nextToken(): RawToken = {
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
    if (next && this.context.strict && next.`type` == Token.Identifier) {
      if (this.scanner.isStrictModeReservedWord(next.value.asInstanceOf[String])) {
        next.`type` = Token.Keyword
      }
    }
    this.lookahead = next
    if (this.config.tokens && next.`type` != Token.EOF) {
      this.tokens.push(this.convertToken(next))
    }
    token
  }
  
  def nextRegexToken(): RawToken = {
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
    new {
      var index = this.startMarker.index
      var line = this.startMarker.line
      var column = this.startMarker.column
    }
  }
  
  def startNode(token: RawToken, lastLineStart: Double = 0): Marker = {
    var column = token.start - token.lineStart
    var line = token.lineNumber
    if (column < 0) {
      column += lastLineStart
      line -= 1
    }
    new {
      var index = token.start
      var line = line
      var column = column
    }
  }
  
  def finalize(marker: Marker, node: Any) = {
    if (this.config.range) {
      node.range = Array(marker.index, this.lastMarker.index)
    }
    if (this.config.loc) {
      node.loc = new {
        var start = new {
          var line = marker.line
          var column = marker.column
        }
        var end = new {
          var line = this.lastMarker.line
          var column = this.lastMarker.column
        }
      }
      if (this.config.source) {
        node.loc.source = this.config.source
      }
    }
    if (this.delegate) {
      object metadata {
        var start = new {
          var line = marker.line
          var column = marker.column
          var offset = marker.index
        }
        var end = new {
          var line = this.lastMarker.line
          var column = this.lastMarker.column
          var offset = this.lastMarker.index
        }
      }
      this.delegate(node, metadata)
    }
    node
  }
  
  // Expect the next token to match the specified punctuator.
  // If not, an exception will be thrown.
  def expect(value: String | Double) = {
    val token = this.nextToken()
    if (token.`type` != Token.Punctuator || token.value != value) {
      this.throwUnexpectedToken(token)
    }
  }
  
  // Quietly expect a comma when in tolerant mode, otherwise delegates to expect().
  def expectCommaSeparator() = {
    if (this.config.tolerant) {
      val token = this.lookahead
      if (token.`type` == Token.Punctuator && token.value == ",") {
        this.nextToken()
      } else if (token.`type` == Token.Punctuator && token.value == ";") {
        this.nextToken()
        this.tolerateUnexpectedToken(token)
      } else {
        this.tolerateUnexpectedToken(token, Messages.UnexpectedToken)
      }
    } else {
      this.expect(",")
    }
  }
  
  // Expect the next token to match the specified keyword.
  // If not, an exception will be thrown.
  def expectKeyword(keyword: String | Double) = {
    val token = this.nextToken()
    if (token.`type` != Token.Keyword || token.value != keyword) {
      this.throwUnexpectedToken(token)
    }
  }
  
  // Return true if the next token matches the specified punctuator.
  def `match`(value: String | Double) = {
    this.lookahead.`type` == Token.Punctuator && this.lookahead.value == value
  }
  
  // Return true if the next token matches the specified keyword
  def matchKeyword(keyword: String | Double) = {
    this.lookahead.`type` == Token.Keyword && this.lookahead.value == keyword
  }
  
  // Return true if the next token matches the specified contextual keyword
  // (where an identifier is sometimes a keyword depending on the context)
  def matchContextualKeyword(keyword: String | Double) = {
    this.lookahead.`type` == Token.Identifier && this.lookahead.value == keyword
  }
  
  // Return true if the next token is an assignment operator
  def matchAssign() = {
    if (this.lookahead.`type` != Token.Punctuator) {
      return false
    }
    val op = this.lookahead.value
    op == "=" || op == "*=" || op == "**=" || op == "/=" || op == "%=" || op == "+=" || op == "-=" || op == "<<=" || op == ">>=" || op == ">>>=" || op == "&=" || op == "^=" || op == "|="
  }
  
  // Cover grammar support.
  //
  // When an assignment expression position starts with an left parenthesis, the determination of the type
  // of the syntax is to be deferred arbitrarily long until the end of the parentheses pair (plus a lookahead)
  // or the first comma. This situation also defers the determination of all the expressions nested in the pair.
  //
  // There are three productions that can be parsed in a parentheses pair that needs to be determined
  // after the outermost pair is closed. They are:
  //
  //   1. AssignmentExpression
  //   2. BindingElements
  //   3. AssignmentTargets
  //
  // In order to avoid exponential backtracking, we use two flags to denote if the production can be
  // binding element or assignment target.
  //
  // The three productions have the relationship:
  //
  //   BindingElements ⊆ AssignmentTargets ⊆ AssignmentExpression
  //
  // with a single exception that CoverInitializedName when used directly in an Expression, generates
  // an early error. Therefore, we need the third state, firstCoverInitializedNameError, to track the
  // first usage of CoverInitializedName and report it when we reached the end of the parentheses pair.
  //
  // isolateCoverGrammar function runs the given parser function with a new cover grammar context, and it does not
  // effect the current flags. This means the production the parser parses is only used as an expression. Therefore
  // the CoverInitializedName check is conducted.
  //
  // inheritCoverGrammar function runs the given parse function with a new cover grammar context, and it propagates
  // the flags outside of the parser. This means the production the parser parses is used as a part of a potential
  // pattern. The CoverInitializedName check is deferred.
  def isolateCoverGrammar(parseFunction: Any) = {
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
  
  def inheritCoverGrammar(parseFunction: Any) = {
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
      if (this.lookahead.`type` != Token.EOF && !this.`match`("}")) {
        this.throwUnexpectedToken(this.lookahead)
      }
      this.lastMarker.index = this.startMarker.index
      this.lastMarker.line = this.startMarker.line
      this.lastMarker.column = this.startMarker.column
    }
  }
  
  // https://tc39.github.io/ecma262/#sec-primary-expression
  def parsePrimaryExpression(): Node.Expression = {
    val node = this.createNode()
    var expr: Node.Expression = _
    var token: RawToken = _
    var raw: String = _
    this.lookahead.`type` match {
      case Token.Identifier =>
        if ((this.context.isModule || this.context.await) && this.lookahead.value == "await") {
          this.tolerateUnexpectedToken(this.lookahead)
        }
        expr = if (this.matchAsyncFunction()) this.parseFunctionExpression() else this.finalize(node, new Node.Identifier(this.nextToken().value))
      case Token.NumericLiteral | Token.StringLiteral =>
        if (this.context.strict && this.lookahead.octal) {
          this.tolerateUnexpectedToken(this.lookahead, Messages.StrictOctalLiteral)
        }
        this.context.isAssignmentTarget = false
        this.context.isBindingElement = false
        token = this.nextToken()
        raw = this.getTokenRaw(token)
        expr = this.finalize(node, new Node.Literal(token.value, raw))
      case Token.BooleanLiteral =>
        this.context.isAssignmentTarget = false
        this.context.isBindingElement = false
        token = this.nextToken()
        raw = this.getTokenRaw(token)
        expr = this.finalize(node, new Node.Literal(token.value == "true", raw))
      case Token.NullLiteral =>
        this.context.isAssignmentTarget = false
        this.context.isBindingElement = false
        token = this.nextToken()
        raw = this.getTokenRaw(token)
        expr = this.finalize(node, new Node.Literal(null, raw))
      case Token.Template =>
        expr = this.parseTemplateLiteral()
      case Token.Punctuator =>
        this.lookahead.value match {
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
            expr = this.finalize(node, new Node.RegexLiteral(token.regex.asInstanceOf[(String) => Any], raw, token.pattern, token.flags))
          case _ =>
            expr = this.throwUnexpectedToken(this.nextToken())
        }
      case Token.Keyword =>
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
            expr = this.parseImportCall()
          } else {
            expr = this.throwUnexpectedToken(this.nextToken())
          }
        }
      case _ =>
        expr = this.throwUnexpectedToken(this.nextToken())
    }
    expr
  }
  
  // https://tc39.github.io/ecma262/#sec-array-initializer
  def parseSpreadElement(): Node.SpreadElement = {
    val node = this.createNode()
    this.expect("...")
    val arg = this.inheritCoverGrammar(this.parseAssignmentExpression)
    this.finalize(node, new Node.SpreadElement(arg))
  }
  
  def parseArrayInitializer(): Node.ArrayExpression = {
    val node = this.createNode()
    val elements = Array.empty[Node.ArrayExpressionElement]
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
  
  // https://tc39.github.io/ecma262/#sec-object-initializer
  def parsePropertyMethod(params: AnyRef): Node.BlockStatement = {
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
  
  def parsePropertyMethodFunction(): Node.FunctionExpression = {
    val isGenerator = false
    val node = this.createNode()
    val previousAllowYield = this.context.allowYield
    this.context.allowYield = true
    val params = this.parseFormalParameters()
    val method = this.parsePropertyMethod(params)
    this.context.allowYield = previousAllowYield
    this.finalize(node, new Node.FunctionExpression(null, params.params, method, isGenerator))
  }
  
  def parsePropertyMethodAsyncFunction(): Node.FunctionExpression = {
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
  
  def parseObjectPropertyKey(): Identifier | Literal = {
    val node = this.createNode()
    val token = this.nextToken()
    var key: Identifier | Literal = _
    token.`type` match {
      case Token.StringLiteral | Token.NumericLiteral =>
        if (this.context.strict && token.octal) {
          this.tolerateUnexpectedToken(token, Messages.StrictOctalLiteral)
        }
        val raw = this.getTokenRaw(token)
        key = this.finalize(node, new Node.Literal(token.value.asInstanceOf[String], raw))
      case Token.Identifier | Token.BooleanLiteral | Token.NullLiteral | Token.Keyword =>
        key = this.finalize(node, new Node.Identifier(token.value))
      case Token.Punctuator =>
        if (token.value == "[") {
          key = this.isolateCoverGrammar(this.parseAssignmentExpression)
          this.expect("]")
        } else {
          key = this.throwUnexpectedToken(token)
        }
      case _ =>
        key = this.throwUnexpectedToken(token)
    }
    key
  }
  
  def isPropertyKey(key: Identifier | Literal, value: String) = {
    key.`type` == Syntax.Identifier && key.name == value || key.`type` == Syntax.Literal && key.value == value
  }
  
  def parseObjectProperty(hasProto: AnyRef): Node.Property = {
    val node = this.createNode()
    val token = this.lookahead
    var kind: String = _
    var key: Identifier | Literal = null
    var value: Node.PropertyValue = null
    var computed = false
    var method = false
    var shorthand = false
    var isAsync = false
    if (token.`type` == Token.Identifier) {
      val id = token.value
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
    if (token.`type` == Token.Identifier && !isAsync && token.value == "get" && lookaheadPropertyKey) {
      kind = "get"
      computed = this.`match`("[")
      key = this.parseObjectPropertyKey()
      this.context.allowYield = false
      value = this.parseGetterMethod()
    } else if (token.`type` == Token.Identifier && !isAsync && token.value == "set" && lookaheadPropertyKey) {
      kind = "set"
      computed = this.`match`("[")
      key = this.parseObjectPropertyKey()
      value = this.parseSetterMethod()
    } else if (token.`type` == Token.Punctuator && token.value == "*" && lookaheadPropertyKey) {
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
      } else if (token.`type` == Token.Identifier) {
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
    this.finalize(node, new Node.Property(kind, key.asInstanceOf[Node.PropertyKey], computed, value, method, shorthand))
  }
  
  def parseObjectInitializer(): Node.ObjectExpression = {
    val node = this.createNode()
    this.expect("{")
    val properties = Array.empty[Node.ObjectExpressionProperty]
    object hasProto {
      var value = false
    }
    while (!this.`match`("}")) {
      properties.push(if (this.`match`("...")) this.parseSpreadElement() else this.parseObjectProperty(hasProto))
      if (!this.`match`("}")) {
        this.expectCommaSeparator()
      }
    }
    this.expect("}")
    this.finalize(node, new Node.ObjectExpression(properties))
  }
  
  // https://tc39.github.io/ecma262/#sec-template-literals
  def parseTemplateHead(): Node.TemplateElement = {
    assert(this.lookahead.head.asInstanceOf[Boolean], "Template literal must start with a template head")
    val node = this.createNode()
    val token = this.nextToken()
    val raw = token.value.asInstanceOf[String]
    val cooked = token.cooked.asInstanceOf[String]
    this.finalize(node, new Node.TemplateElement(new {
      var raw = raw
      var cooked = cooked
    }, token.tail.asInstanceOf[Boolean]))
  }
  
  def parseTemplateElement(): Node.TemplateElement = {
    if (this.lookahead.`type` != Token.Template) {
      this.throwUnexpectedToken()
    }
    val node = this.createNode()
    val token = this.nextToken()
    val raw = token.value.asInstanceOf[String]
    val cooked = token.cooked.asInstanceOf[String]
    this.finalize(node, new Node.TemplateElement(new {
      var raw = raw
      var cooked = cooked
    }, token.tail.asInstanceOf[Boolean]))
  }
  
  def parseTemplateLiteral(): Node.TemplateLiteral = {
    val node = this.createNode()
    val expressions = Array.empty[Node.Expression]
    val quasis = Array.empty[Node.TemplateElement]
    var quasi = this.parseTemplateHead()
    quasis.push(quasi)
    while (!quasi.tail) {
      expressions.push(this.parseExpression())
      quasi = this.parseTemplateElement()
      quasis.push(quasi)
    }
    this.finalize(node, new Node.TemplateLiteral(quasis, expressions))
  }
  
  // https://tc39.github.io/ecma262/#sec-grouping-operator
  def reinterpretExpressionAsPattern(expr: Any) = {
    expr.`type` match {
      case Syntax.Identifier | Syntax.MemberExpression | Syntax.RestElement | Syntax.AssignmentPattern =>
      case Syntax.SpreadElement =>
        expr.`type` = Syntax.RestElement
        this.reinterpretExpressionAsPattern(expr.argument)
      case Syntax.ArrayExpression =>
        expr.`type` = Syntax.ArrayPattern
        for (i <- expr.elements) {
          if (i != null) {
            this.reinterpretExpressionAsPattern(i)
          }
        }
      case Syntax.ObjectExpression =>
        expr.`type` = Syntax.ObjectPattern
        for (property <- expr.properties) {
          this.reinterpretExpressionAsPattern(if (property.`type` == Syntax.SpreadElement) property else property.value)
        }
      case Syntax.AssignmentExpression =>
        expr.`type` = Syntax.AssignmentPattern
        delete expr.operator
        this.reinterpretExpressionAsPattern(expr.left)
      case _ =>
    }
  }
  
  def parseGroupExpression(): ArrowParameterPlaceHolderNode | Node.Expression = {
    var expr: Node.RestElement = _
    this.expect("(")
    if (this.`match`(")")) {
      this.nextToken()
      if (!this.`match`("=>")) {
        this.expect("=>")
      }
      expr = new {
        var `type` = ArrowParameterPlaceHolder
        var params = Array()
        var async = false
      }
    } else {
      val startToken = this.lookahead
      val params = Array.empty[Unit]
      if (this.`match`("...")) {
        expr = this.parseRestElement(params)
        this.expect(")")
        if (!this.`match`("=>")) {
          this.expect("=>")
        }
        expr = new {
          var `type` = ArrowParameterPlaceHolder
          var params = Array(expr)
          var async = false
        }
      } else {
        var arrow = false
        this.context.isBindingElement = true
        expr = this.inheritCoverGrammar(this.parseAssignmentExpression)
        if (this.`match`(",")) {
          val expressions = Array.empty[Node.Expression]
          this.context.isAssignmentTarget = false
          expressions.push(expr)
          while (this.lookahead.`type` != Token.EOF) {
            if (!this.`match`(",")) {
              /* Unsupported: Break */ break;
            }
            this.nextToken()
            if (this.`match`(")")) {
              this.nextToken()
              for (i <- expressions) {
                this.reinterpretExpressionAsPattern(i)
              }
              arrow = true
              expr = new {
                var `type` = ArrowParameterPlaceHolder
                var params = expressions
                var async = false
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
              for (i <- expressions) {
                this.reinterpretExpressionAsPattern(i)
              }
              arrow = true
              expr = new {
                var `type` = ArrowParameterPlaceHolder
                var params = expressions
                var async = false
              }
            } else {
              expressions.push(this.inheritCoverGrammar(this.parseAssignmentExpression))
            }
            if (arrow) {
              /* Unsupported: Break */ break;
            }
          }
          if (!arrow) {
            expr = this.finalize(this.startNode(startToken), new Node.SequenceExpression(expressions))
          }
        }
        if (!arrow) {
          this.expect(")")
          if (this.`match`("=>")) {
            if (expr.`type` == Syntax.Identifier && expr.name == "yield") {
              arrow = true
              expr = new {
                var `type` = ArrowParameterPlaceHolder
                var params = Array(expr)
                var async = false
              }
            }
            if (!arrow) {
              if (!this.context.isBindingElement) {
                this.throwUnexpectedToken(this.lookahead)
              }
              if (expr.`type` == Syntax.SequenceExpression) {
                for (i <- expr.expressions) {
                  this.reinterpretExpressionAsPattern(i)
                }
              } else {
                this.reinterpretExpressionAsPattern(expr)
              }
              val parameters = if (expr.`type` == Syntax.SequenceExpression) expr.expressions else Array(expr)
              expr = new {
                var `type` = ArrowParameterPlaceHolder
                var params = parameters
                var async = false
              }
            }
          }
          this.context.isBindingElement = false
        }
      }
    }
    expr
  }
  
  // https://tc39.github.io/ecma262/#sec-left-hand-side-expressions
  def parseArguments(): Array[Expression | SpreadElement] = {
    this.expect("(")
    val args = Array.empty[Node.ArgumentListElement]
    if (!this.`match`(")")) {
      while (true) {
        val expr = if (this.`match`("...")) this.parseSpreadElement() else this.isolateCoverGrammar(this.parseAssignmentExpression)
        args.push(expr)
        if (this.`match`(")")) {
          /* Unsupported: Break */ break;
        }
        this.expectCommaSeparator()
        if (this.`match`(")")) {
          /* Unsupported: Break */ break;
        }
      }
    }
    this.expect(")")
    args
  }
  
  def isIdentifierName(token: RawToken): Boolean = {
    token.`type` == Token.Identifier || token.`type` == Token.Keyword || token.`type` == Token.BooleanLiteral || token.`type` == Token.NullLiteral
  }
  
  def parseIdentifierName(): Node.Identifier = {
    val node = this.createNode()
    val token = this.nextToken()
    if (!this.isIdentifierName(token)) {
      this.throwUnexpectedToken(token)
    }
    this.finalize(node, new Node.Identifier(token.value))
  }
  
  def parseNewExpression(): Node.MetaProperty | Node.NewExpression = {
    val node = this.createNode()
    val id = this.parseIdentifierName()
    assert(id.name == "new", "New expression must start with `new`")
    var expr: Any = _
    if (this.`match`(".")) {
      this.nextToken()
      if (this.lookahead.`type` == Token.Identifier && this.context.inFunctionBody && this.lookahead.value == "target") {
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
  
  def parseAsyncArgument() = {
    val arg = this.parseAssignmentExpression()
    this.context.firstCoverInitializedNameError = null
    arg
  }
  
  def parseAsyncArguments(): Array[Expression | SpreadElement] = {
    this.expect("(")
    val args = Array.empty[Node.ArgumentListElement]
    if (!this.`match`(")")) {
      while (true) {
        val expr = if (this.`match`("...")) this.parseSpreadElement() else this.isolateCoverGrammar(this.parseAsyncArgument)
        args.push(expr)
        if (this.`match`(")")) {
          /* Unsupported: Break */ break;
        }
        this.expectCommaSeparator()
        if (this.`match`(")")) {
          /* Unsupported: Break */ break;
        }
      }
    }
    this.expect(")")
    args
  }
  
  def matchImportCall(): Boolean = {
    var `match` = this.matchKeyword("import")
    if (`match`) {
      val state = this.scanner.saveState()
      this.scanner.scanComments()
      val next = this.scanner.lex()
      this.scanner.restoreState(state)
      `match` = next.`type` == Token.Punctuator && next.value == "("
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
    var expr: Marker = _
    if (this.matchKeyword("super") && this.context.inFunctionBody) {
      expr = this.createNode()
      this.nextToken()
      expr = this.finalize(expr, new Node.Super())
      if (!this.`match`("(") && !this.`match`(".") && !this.`match`("[")) {
        this.throwUnexpectedToken(this.lookahead)
      }
    } else {
      expr = this.inheritCoverGrammar(if (this.matchKeyword("new")) this.parseNewExpression else this.parsePrimaryExpression)
    }
    while (true) {
      if (this.`match`(".")) {
        this.context.isBindingElement = false
        this.context.isAssignmentTarget = true
        this.expect(".")
        val property = this.parseIdentifierName()
        expr = this.finalize(this.startNode(startToken), new Node.StaticMemberExpression(expr, property))
      } else if (this.`match`("(")) {
        val asyncArrow = maybeAsync && startToken.lineNumber == this.lookahead.lineNumber
        this.context.isBindingElement = false
        this.context.isAssignmentTarget = false
        val args = if (asyncArrow) this.parseAsyncArguments() else this.parseArguments()
        if (expr.`type` == Syntax.Import && args.length != 1) {
          this.tolerateError(Messages.BadImportCallArity)
        }
        expr = this.finalize(this.startNode(startToken), new Node.CallExpression(expr, args))
        if (asyncArrow && this.`match`("=>")) {
          for (i <- args) {
            this.reinterpretExpressionAsPattern(i)
          }
          expr = new {
            var `type` = ArrowParameterPlaceHolder
            var params = args
            var async = true
          }
        }
      } else if (this.`match`("[")) {
        this.context.isBindingElement = false
        this.context.isAssignmentTarget = true
        this.expect("[")
        val property = this.isolateCoverGrammar(this.parseExpression)
        this.expect("]")
        expr = this.finalize(this.startNode(startToken), new Node.ComputedMemberExpression(expr, property))
      } else if (this.lookahead.`type` == Token.Template && this.lookahead.head) {
        val quasi = this.parseTemplateLiteral()
        expr = this.finalize(this.startNode(startToken), new Node.TaggedTemplateExpression(expr, quasi))
      } else {
        /* Unsupported: Break */ break;
      }
    }
    this.context.allowIn = previousAllowIn
    expr
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
      } else if (this.lookahead.`type` == Token.Template && this.lookahead.head) {
        val quasi = this.parseTemplateLiteral()
        expr = this.finalize(node, new Node.TaggedTemplateExpression(expr, quasi))
      } else {
        /* Unsupported: Break */ break;
      }
    }
    expr
  }
  
  // https://tc39.github.io/ecma262/#sec-update-expressions
  def parseUpdateExpression(): Node.Expression = {
    var expr: Identifier = _
    val startToken = this.lookahead
    if (this.`match`("++") || this.`match`("--")) {
      val node = this.startNode(startToken)
      val token = this.nextToken()
      expr = this.inheritCoverGrammar(this.parseUnaryExpression)
      if (this.context.strict && expr.`type` == Syntax.Identifier && this.scanner.isRestrictedWord(expr.name)) {
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
      if (!this.hasLineTerminator && this.lookahead.`type` == Token.Punctuator) {
        if (this.`match`("++") || this.`match`("--")) {
          if (this.context.strict && expr.`type` == Syntax.Identifier && this.scanner.isRestrictedWord(expr.name)) {
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
  
  // https://tc39.github.io/ecma262/#sec-unary-operators
  def parseAwaitExpression(): Node.AwaitExpression = {
    val node = this.createNode()
    this.nextToken()
    val argument = this.parseUnaryExpression()
    this.finalize(node, new Node.AwaitExpression(argument))
  }
  
  def parseUnaryExpression(): Node.Expression = {
    var expr: Any = _
    if (this.`match`("+") || this.`match`("-") || this.`match`("~") || this.`match`("!") || this.matchKeyword("delete") || this.matchKeyword("void") || this.matchKeyword("typeof")) {
      val node = this.startNode(this.lookahead)
      val token = this.nextToken()
      expr = this.inheritCoverGrammar(this.parseUnaryExpression)
      expr = this.finalize(node, new Node.UnaryExpression(token.value, expr))
      if (this.context.strict && expr.operator == "delete" && expr.argument.`type` == Syntax.Identifier) {
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
  
  // https://tc39.github.io/ecma262/#sec-exp-operator
  // https://tc39.github.io/ecma262/#sec-multiplicative-operators
  // https://tc39.github.io/ecma262/#sec-additive-operators
  // https://tc39.github.io/ecma262/#sec-bitwise-shift-operators
  // https://tc39.github.io/ecma262/#sec-relational-operators
  // https://tc39.github.io/ecma262/#sec-equality-operators
  // https://tc39.github.io/ecma262/#sec-binary-bitwise-operators
  // https://tc39.github.io/ecma262/#sec-binary-logical-operators
  def binaryPrecedence(token: RawToken): Double = {
    val op = token.value
    var precedence: Double = _
    if (token.`type` == Token.Punctuator) {
      precedence = this.operatorPrecedence(op) || 0
    } else if (token.`type` == Token.Keyword) {
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
      val markers = Array(startToken, this.lookahead)
      var left = expr
      var right = this.isolateCoverGrammar(this.parseExponentiationExpression)
      val stack = Array(left, token.value, right)
      val precedences = Array(prec)
      while (true) {
        prec = this.binaryPrecedence(this.lookahead)
        if (prec <= 0) {
          /* Unsupported: Break */ break;
        }
        // Reduce: make a binary expression from the three topmost entries.
        while (stack.length > 2 && prec <= precedences(precedences.length - 1)) {
          right = stack.pop()
          val operator = stack.pop()
          precedences.pop()
          left = stack.pop()
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
      // Final reduce to clean-up the stack.
      var i = stack.length - 1
      expr = stack(i)
      var lastMarker = markers.pop()
      while (i > 1) {
        val marker = markers.pop()
        val lastLineStart = lastMarker && lastMarker.lineStart
        val node = this.startNode(marker, lastLineStart)
        val operator = stack(i - 1)
        expr = this.finalize(node, new Node.BinaryExpression(operator, stack(i - 2), expr))
        i -= 2
        lastMarker = marker
      }
    }
    expr
  }
  
  // https://tc39.github.io/ecma262/#sec-conditional-operator
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
  
  // https://tc39.github.io/ecma262/#sec-assignment-operators
  def checkPatternParam(options: AnyRef, param: String) = {
    param.`type` match {
      case Syntax.Identifier =>
        this.validateParam(options, param, param.name)
      case Syntax.RestElement =>
        this.checkPatternParam(options, param.argument)
      case Syntax.AssignmentPattern =>
        this.checkPatternParam(options, param.left)
      case Syntax.ArrayPattern =>
        for (i <- param.elements) {
          if (i != null) {
            this.checkPatternParam(options, i)
          }
        }
      case Syntax.ObjectPattern =>
        for (property <- param.properties) {
          this.checkPatternParam(options, if (property.`type` == Syntax.RestElement) property else property.value)
        }
      case _ =>
    }
    options.simple = options.simple && param.isInstanceOf[Node.Identifier]
  }
  
  def reinterpretAsCoverFormalsList(expr: String) = {
    var params = Array(expr)
    var asyncArrow = false
    expr.`type` match {
      case Syntax.Identifier =>
      case ArrowParameterPlaceHolder =>
        params = expr.params
        asyncArrow = expr.async
      case _ =>
        return null
    }
    object options {
      var simple = true
      var paramSet = new {}
    }
    for (i <- params.indices) {
      val param = params(i)
      if (param.`type` == Syntax.AssignmentPattern) {
        if (param.right.`type` == Syntax.YieldExpression) {
          if (param.right.argument) {
            this.throwUnexpectedToken(this.lookahead)
          }
          param.right.`type` = Syntax.Identifier
          param.right.name = "yield"
          delete param.right.argument
          delete param.right.delegate
        }
      } else if (asyncArrow && param.`type` == Syntax.Identifier && param.name == "await") {
        this.throwUnexpectedToken(this.lookahead)
      }
      this.checkPatternParam(options, param)
      params(i) = param
    }
    if (this.context.strict || !this.context.allowYield) {
      for (param <- params) {
        if (param.`type` == Syntax.YieldExpression) {
          this.throwUnexpectedToken(this.lookahead)
        }
      }
    }
    if (options.message == Messages.StrictParamDupe) {
      val token = if (this.context.strict) options.stricted else options.firstRestricted
      this.throwUnexpectedToken(token, options.message)
    }
    new {
      var simple = options.simple
      var params = params
      var stricted = options.stricted
      var firstRestricted = options.firstRestricted
      var message = options.message
    }
  }
  
  def parseAssignmentExpression(): Node.Expression = {
    var expr: Any = _
    if (!this.context.allowYield && this.matchKeyword("yield")) {
      expr = this.parseYieldExpression()
    } else {
      val startToken = this.lookahead
      var token = startToken
      expr = this.parseConditionalExpression()
      if (token.`type` == Token.Identifier && token.lineNumber == this.lookahead.lineNumber && token.value == "async") {
        if (this.lookahead.`type` == Token.Identifier || this.matchKeyword("yield")) {
          val arg = this.parsePrimaryExpression()
          this.reinterpretExpressionAsPattern(arg)
          expr = new {
            var `type` = ArrowParameterPlaceHolder
            var params = Array(arg)
            var async = true
          }
        }
      }
      if (expr.isInstanceOf[String] || this.`match`("=>")) {
        var expr_cast = expr.asInstanceOf[String]
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
          var body: Node.BlockStatement | Node.Expression = _
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
          expr_cast = if (isAsync) this.finalize(node, new Node.AsyncArrowFunctionExpression(list.params, body, expression)) else this.finalize(node, new Node.ArrowFunctionExpression(list.params, body, expression))
          this.context.strict = previousStrict
          this.context.allowStrictDirective = previousAllowStrictDirective
          this.context.allowYield = previousAllowYield
          this.context.await = previousAwait
        }
      } else {
        if (this.matchAssign()) {
          if (!this.context.isAssignmentTarget) {
            this.tolerateError(Messages.InvalidLHSInAssignment)
          }
          if (this.context.strict && expr.`type` == Syntax.Identifier) {
            val id = expr.asInstanceOf[Node.Identifier]
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
            this.reinterpretExpressionAsPattern(expr)
          }
          token = this.nextToken()
          val operator = token.value.asInstanceOf[String]
          val right = this.isolateCoverGrammar(this.parseAssignmentExpression)
          expr = this.finalize(this.startNode(startToken), new Node.AssignmentExpression(operator, expr, right))
          this.context.firstCoverInitializedNameError = null
        }
      }
    }
    expr
  }
  
  // https://tc39.github.io/ecma262/#sec-comma-operator
  def parseExpression(): Node.Expression | Node.SequenceExpression = {
    val startToken = this.lookahead
    var expr = this.isolateCoverGrammar(this.parseAssignmentExpression)
    if (this.`match`(",")) {
      val expressions = Array.empty[Node.Expression]
      expressions.push(expr)
      while (this.lookahead.`type` != Token.EOF) {
        if (!this.`match`(",")) {
          /* Unsupported: Break */ break;
        }
        this.nextToken()
        expressions.push(this.isolateCoverGrammar(this.parseAssignmentExpression))
      }
      expr = this.finalize(this.startNode(startToken), new Node.SequenceExpression(expressions))
    }
    expr
  }
  
  // https://tc39.github.io/ecma262/#sec-block
  def parseStatementListItem(): Declaration | Statement = {
    var statement: Declaration | Statement = _
    this.context.isAssignmentTarget = true
    this.context.isBindingElement = true
    if (this.lookahead.`type` == Token.Keyword) {
      this.lookahead.value match {
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
          statement = this.parseLexicalDeclaration(new {
            var inFor = false
          })
        case "function" =>
          statement = this.parseFunctionDeclaration()
        case "class" =>
          statement = this.parseClassDeclaration()
        case "let" =>
          statement = if (this.isLexicalDeclaration()) this.parseLexicalDeclaration(new {
            var inFor = false
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
    val block = Array.empty[Node.StatementListItem]
    while (true) {
      if (this.`match`("}")) {
        /* Unsupported: Break */ break;
      }
      block.push(this.parseStatementListItem())
    }
    this.expect("}")
    this.finalize(node, new Node.BlockStatement(block))
  }
  
  // https://tc39.github.io/ecma262/#sec-let-and-const-declarations
  def parseLexicalBinding(kind: String, options: AnyRef): Node.VariableDeclarator = {
    val node = this.createNode()
    val params = Array.empty[Unit]
    val id = this.parsePattern(params, kind)
    if (this.context.strict && id.`type` == Syntax.Identifier) {
      if (this.scanner.isRestrictedWord(id.asInstanceOf[Node.Identifier].name)) {
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
  
  def parseBindingList(kind: String, options: AnyRef): Array[Node.VariableDeclarator] = {
    val list = Array(this.parseLexicalBinding(kind, options))
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
    next.`type` == Token.Identifier || next.`type` == Token.Punctuator && next.value == "[" || next.`type` == Token.Punctuator && next.value == "{" || next.`type` == Token.Keyword && next.value == "let" || next.`type` == Token.Keyword && next.value == "yield"
  }
  
  def parseLexicalDeclaration(options: AnyRef): Node.VariableDeclaration = {
    val node = this.createNode()
    val kind = this.nextToken().value.asInstanceOf[String]
    assert(kind == "let" || kind == "const", "Lexical declaration must be either let or const")
    val declarations = this.parseBindingList(kind, options)
    this.consumeSemicolon()
    this.finalize(node, new Node.VariableDeclaration(declarations, kind))
  }
  
  // https://tc39.github.io/ecma262/#sec-destructuring-binding-patterns
  def parseBindingRestElement(params: Array[Any], kind: String): Node.RestElement = {
    val node = this.createNode()
    this.expect("...")
    val arg = this.parsePattern(params, kind)
    this.finalize(node, new Node.RestElement(arg))
  }
  
  def parseArrayPattern(params: Array[Any], kind: String): Node.ArrayPattern = {
    val node = this.createNode()
    this.expect("[")
    val elements = Array.empty[Node.ArrayPatternElement]
    while (!this.`match`("]")) {
      if (this.`match`(",")) {
        this.nextToken()
        elements.push(null)
      } else {
        if (this.`match`("...")) {
          elements.push(this.parseBindingRestElement(params, kind))
          /* Unsupported: Break */ break;
        } else {
          elements.push(this.parsePatternWithDefault(params, kind))
        }
        if (!this.`match`("]")) {
          this.expect(",")
        }
      }
    }
    this.expect("]")
    this.finalize(node, new Node.ArrayPattern(elements))
  }
  
  def parsePropertyPattern(params: Array[Any], kind: String): Node.Property = {
    val node = this.createNode()
    var computed = false
    var shorthand = false
    val method = false
    var key: Identifier | Literal = _
    var value: Node.PropertyValue = _
    if (this.lookahead.`type` == Token.Identifier) {
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
        value = this.parsePatternWithDefault(params, kind)
      }
    } else {
      computed = this.`match`("[")
      key = this.parseObjectPropertyKey()
      this.expect(":")
      value = this.parsePatternWithDefault(params, kind)
    }
    this.finalize(node, new Node.Property("init", key, computed, value, method, shorthand))
  }
  
  def parseRestProperty(params: Array[Any], kind: String): Node.RestElement = {
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
  
  def parseObjectPattern(params: Array[Any], kind: String): Node.ObjectPattern = {
    val node = this.createNode()
    val properties = Array.empty[Node.ObjectPatternProperty]
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
  
  def parsePattern(params: Array[Any], kind: String): Identifier | ArrayPattern | ObjectPattern = {
    var pattern: Any = _
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
  
  def parsePatternWithDefault(params: Array[Any], kind: String): Node.AssignmentPattern | Identifier | ArrayPattern | ObjectPattern = {
    val startToken = this.lookahead
    var pattern = this.parsePattern(params, kind)
    if (this.`match`("=")) {
      this.nextToken()
      val previousAllowYield = this.context.allowYield
      this.context.allowYield = true
      val right = this.isolateCoverGrammar(this.parseAssignmentExpression)
      this.context.allowYield = previousAllowYield
      pattern = this.finalize(this.startNode(startToken), new Node.AssignmentPattern(pattern, right))
    }
    pattern
  }
  
  // https://tc39.github.io/ecma262/#sec-variable-statement
  def parseVariableIdentifier(kind: String): Node.Identifier = {
    val node = this.createNode()
    val token = this.nextToken()
    if (token.`type` == Token.Keyword && token.value == "yield") {
      if (this.context.strict) {
        this.tolerateUnexpectedToken(token, Messages.StrictReservedWord)
      } else if (!this.context.allowYield) {
        this.throwUnexpectedToken(token)
      }
    } else if (token.`type` != Token.Identifier) {
      if (this.context.strict && token.`type` == Token.Keyword && this.scanner.isStrictModeReservedWord(token.value.asInstanceOf[String])) {
        this.tolerateUnexpectedToken(token, Messages.StrictReservedWord)
      } else {
        if (this.context.strict || token.value != "let" || kind != "var") {
          this.throwUnexpectedToken(token)
        }
      }
    } else if ((this.context.isModule || this.context.await) && token.`type` == Token.Identifier && token.value == "await") {
      this.tolerateUnexpectedToken(token)
    }
    this.finalize(node, new Node.Identifier(token.value))
  }
  
  def parseVariableDeclaration(options: DeclarationOptions): Node.VariableDeclarator = {
    val node = this.createNode()
    val params = Array.empty[RawToken]
    val id = this.parsePattern(params, "var")
    if (this.context.strict && id.`type` == Syntax.Identifier) {
      if (this.scanner.isRestrictedWord(id.asInstanceOf[Node.Identifier].name)) {
        this.tolerateError(Messages.StrictVarName)
      }
    }
    var init = null
    if (this.`match`("=")) {
      this.nextToken()
      init = this.isolateCoverGrammar(this.parseAssignmentExpression)
    } else if (id.`type` != Syntax.Identifier && !options.inFor) {
      this.expect("=")
    }
    this.finalize(node, new Node.VariableDeclarator(id, init))
  }
  
  def parseVariableDeclarationList(options: AnyRef): Array[Node.VariableDeclarator] = {
    object opt extends DeclarationOptions {
      var inFor = options.inFor
    }
    val list = Array.empty[Node.VariableDeclarator]
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
    val declarations = this.parseVariableDeclarationList(new {
      var inFor = false
    })
    this.consumeSemicolon()
    this.finalize(node, new Node.VariableDeclaration(declarations, "var"))
  }
  
  // https://tc39.github.io/ecma262/#sec-empty-statement
  def parseEmptyStatement(): Node.EmptyStatement = {
    val node = this.createNode()
    this.expect(";")
    this.finalize(node, new Node.EmptyStatement())
  }
  
  // https://tc39.github.io/ecma262/#sec-expression-statement
  def parseExpressionStatement(): Node.ExpressionStatement = {
    val node = this.createNode()
    val expr = this.parseExpression()
    this.consumeSemicolon()
    this.finalize(node, new Node.ExpressionStatement(expr))
  }
  
  // https://tc39.github.io/ecma262/#sec-if-statement
  def parseIfClause(): Node.Statement = {
    if (this.context.strict && this.matchKeyword("function")) {
      this.tolerateError(Messages.StrictFunction)
    }
    this.parseStatement()
  }
  
  def parseIfStatement(): Node.IfStatement = {
    val node = this.createNode()
    var consequent: Node.Statement = _
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
  
  // https://tc39.github.io/ecma262/#sec-do-while-statement
  def parseDoWhileStatement(): Node.DoWhileStatement = {
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
  
  // https://tc39.github.io/ecma262/#sec-while-statement
  def parseWhileStatement(): Node.WhileStatement = {
    val node = this.createNode()
    var body: Node.Statement = _
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
  
  // https://tc39.github.io/ecma262/#sec-for-statement
  // https://tc39.github.io/ecma262/#sec-for-in-and-for-of-statements
  def parseForStatement(): Node.ForStatement | Node.ForInStatement | Node.ForOfStatement = {
    var init = null
    var test: Node.Expression = null
    var update: Node.Expression = null
    var forIn = true
    var left: Marker = _
    var right: Node.Expression | Node.SequenceExpression = _
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
        val declarations = this.parseVariableDeclarationList(new {
          var inFor = true
        })
        this.context.allowIn = previousAllowIn
        if (declarations.length == 1 && this.matchKeyword("in")) {
          val decl = declarations(0)
          if (decl.init && (decl.id.`type` == Syntax.ArrayPattern || decl.id.`type` == Syntax.ObjectPattern || this.context.strict)) {
            this.tolerateError(Messages.ForInOfLoopInitializer, "for-in")
          }
          init = this.finalize(init, new Node.VariableDeclaration(declarations, "var"))
          this.nextToken()
          left = init
          right = this.parseExpression()
          init = null
        } else if (declarations.length == 1 && declarations(0).init == null && this.matchContextualKeyword("of")) {
          init = this.finalize(init, new Node.VariableDeclaration(declarations, "var"))
          this.nextToken()
          left = init
          right = this.parseAssignmentExpression()
          init = null
          forIn = false
        } else {
          init = this.finalize(init, new Node.VariableDeclaration(declarations, "var"))
          this.expect(";")
        }
      } else if (this.matchKeyword("const") || this.matchKeyword("let")) {
        init = this.createNode()
        val kind = this.nextToken().value.asInstanceOf[String]
        if (!this.context.strict && this.lookahead.value == "in") {
          init = this.finalize(init, new Node.Identifier(kind))
          this.nextToken()
          left = init
          right = this.parseExpression()
          init = null
        } else {
          val previousAllowIn = this.context.allowIn
          this.context.allowIn = false
          val declarations = this.parseBindingList(kind, new {
            var inFor = true
          })
          this.context.allowIn = previousAllowIn
          if (declarations.length == 1 && declarations(0).init == null && this.matchKeyword("in")) {
            init = this.finalize(init, new Node.VariableDeclaration(declarations, kind))
            this.nextToken()
            left = init
            right = this.parseExpression()
            init = null
          } else if (declarations.length == 1 && declarations(0).init == null && this.matchContextualKeyword("of")) {
            init = this.finalize(init, new Node.VariableDeclaration(declarations, kind))
            this.nextToken()
            left = init
            right = this.parseAssignmentExpression()
            init = null
            forIn = false
          } else {
            this.consumeSemicolon()
            init = this.finalize(init, new Node.VariableDeclaration(declarations, kind))
          }
        }
      } else {
        val initStartToken = this.lookahead
        val previousAllowIn = this.context.allowIn
        this.context.allowIn = false
        init = this.inheritCoverGrammar(this.parseAssignmentExpression)
        this.context.allowIn = previousAllowIn
        if (this.matchKeyword("in")) {
          if (!this.context.isAssignmentTarget || init.`type` == Syntax.AssignmentExpression) {
            this.tolerateError(Messages.InvalidLHSInForIn)
          }
          this.nextToken()
          this.reinterpretExpressionAsPattern(init)
          left = init
          right = this.parseExpression()
          init = null
        } else if (this.matchContextualKeyword("of")) {
          if (!this.context.isAssignmentTarget || init.`type` == Syntax.AssignmentExpression) {
            this.tolerateError(Messages.InvalidLHSInForLoop)
          }
          this.nextToken()
          this.reinterpretExpressionAsPattern(init)
          left = init
          right = this.parseAssignmentExpression()
          init = null
          forIn = false
        } else {
          if (this.`match`(",")) {
            val initSeq = Array(init)
            while (this.`match`(",")) {
              this.nextToken()
              initSeq.push(this.isolateCoverGrammar(this.parseAssignmentExpression))
            }
            init = this.finalize(this.startNode(initStartToken), new Node.SequenceExpression(initSeq))
          }
          this.expect(";")
        }
      }
    }
    if (left.getClass == "undefined") {
      if (!this.`match`(";")) {
        test = this.parseExpression()
      }
      this.expect(";")
      if (!this.`match`(")")) {
        update = this.parseExpression()
      }
    }
    var body = _
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
    if (left.getClass == "undefined") this.finalize(node, new Node.ForStatement(init, test, update, body)) else if (forIn) this.finalize(node, new Node.ForInStatement(left, right, body)) else this.finalize(node, new Node.ForOfStatement(left, right, body))
  }
  
  // https://tc39.github.io/ecma262/#sec-continue-statement
  def parseContinueStatement(): Node.ContinueStatement = {
    val node = this.createNode()
    this.expectKeyword("continue")
    var label: Node.Identifier = null
    if (this.lookahead.`type` == Token.Identifier && !this.hasLineTerminator) {
      val id = this.parseVariableIdentifier()
      label = id
      val key = "$" + id.name
      if (!Object.prototype.hasOwnProperty.call(this.context.labelSet, key)) {
        this.throwError(Messages.UnknownLabel, id.name)
      }
    }
    this.consumeSemicolon()
    if (label == null && !this.context.inIteration) {
      this.throwError(Messages.IllegalContinue)
    }
    this.finalize(node, new Node.ContinueStatement(label))
  }
  
  // https://tc39.github.io/ecma262/#sec-break-statement
  def parseBreakStatement(): Node.BreakStatement = {
    val node = this.createNode()
    this.expectKeyword("break")
    var label: Node.Identifier = null
    if (this.lookahead.`type` == Token.Identifier && !this.hasLineTerminator) {
      val id = this.parseVariableIdentifier()
      val key = "$" + id.name
      if (!Object.prototype.hasOwnProperty.call(this.context.labelSet, key)) {
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
  
  // https://tc39.github.io/ecma262/#sec-return-statement
  def parseReturnStatement(): Node.ReturnStatement = {
    if (!this.context.inFunctionBody) {
      this.tolerateError(Messages.IllegalReturn)
    }
    val node = this.createNode()
    this.expectKeyword("return")
    val hasArgument = !this.`match`(";") && !this.`match`("}") && !this.hasLineTerminator && this.lookahead.`type` != Token.EOF || this.lookahead.`type` == Token.Template
    val argument = if (hasArgument) this.parseExpression() else null
    this.consumeSemicolon()
    this.finalize(node, new Node.ReturnStatement(argument))
  }
  
  // https://tc39.github.io/ecma262/#sec-with-statement
  def parseWithStatement(): Node.WithStatement = {
    if (this.context.strict) {
      this.tolerateError(Messages.StrictModeWith)
    }
    val node = this.createNode()
    var body: Node.Statement = _
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
  
  // https://tc39.github.io/ecma262/#sec-switch-statement
  def parseSwitchCase(): Node.SwitchCase = {
    val node = this.createNode()
    var test: Node.Expression | Node.SequenceExpression = _
    if (this.matchKeyword("default")) {
      this.nextToken()
      test = null
    } else {
      this.expectKeyword("case")
      test = this.parseExpression()
    }
    this.expect(":")
    val consequent = Array.empty[Node.StatementListItem]
    while (true) {
      if (this.`match`("}") || this.matchKeyword("default") || this.matchKeyword("case")) {
        /* Unsupported: Break */ break;
      }
      consequent.push(this.parseStatementListItem())
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
    val cases = Array.empty[Node.SwitchCase]
    var defaultFound = false
    this.expect("{")
    while (true) {
      if (this.`match`("}")) {
        /* Unsupported: Break */ break;
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
    this.expect("}")
    this.context.inSwitch = previousInSwitch
    this.finalize(node, new Node.SwitchStatement(discriminant, cases))
  }
  
  // https://tc39.github.io/ecma262/#sec-labelled-statements
  def parseLabelledStatement(): Node.LabeledStatement | Node.ExpressionStatement = {
    val node = this.createNode()
    val expr = this.parseExpression()
    var statement: Node.ExpressionStatement | Node.LabeledStatement = _
    if (expr.`type` == Syntax.Identifier && this.`match`(":")) {
      this.nextToken()
      val id = expr.asInstanceOf[Node.Identifier]
      val key = "$" + id.name
      if (Object.prototype.hasOwnProperty.call(this.context.labelSet, key)) {
        this.throwError(Messages.Redeclaration, "Label", id.name)
      }
      this.context.labelSet(key) = true
      var body: Node.Statement = _
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
  
  // https://tc39.github.io/ecma262/#sec-throw-statement
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
  
  // https://tc39.github.io/ecma262/#sec-try-statement
  def parseCatchClause(): Node.CatchClause = {
    val node = this.createNode()
    this.expectKeyword("catch")
    this.expect("(")
    if (this.`match`(")")) {
      this.throwUnexpectedToken(this.lookahead)
    }
    val params = Array.empty[Any]
    val param = this.parsePattern(params)
    val paramMap = Map.empty[String, Boolean]
    for (i <- params) {
      val key = "$" + i.value
      if (Object.prototype.hasOwnProperty.call(paramMap, key)) {
        this.tolerateError(Messages.DuplicateBinding, i.value)
      }
      paramMap(key) = true
    }
    if (this.context.strict && param.`type` == Syntax.Identifier) {
      if (this.scanner.isRestrictedWord(param.asInstanceOf[Node.Identifier].name)) {
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
  
  // https://tc39.github.io/ecma262/#sec-debugger-statement
  def parseDebuggerStatement(): Node.DebuggerStatement = {
    val node = this.createNode()
    this.expectKeyword("debugger")
    this.consumeSemicolon()
    this.finalize(node, new Node.DebuggerStatement())
  }
  
  // https://tc39.github.io/ecma262/#sec-ecmascript-language-statements-and-declarations
  def parseStatement(): Node.Statement = {
    var statement: Node.Statement = _
    this.lookahead.`type` match {
      case Token.BooleanLiteral | Token.NullLiteral | Token.NumericLiteral | Token.StringLiteral | Token.Template | Token.RegularExpression =>
        statement = this.parseExpressionStatement()
      case Token.Punctuator =>
        val value = this.lookahead.value
        if (value == "{") {
          statement = this.parseBlock()
        } else if (value == "(") {
          statement = this.parseExpressionStatement()
        } else if (value == ";") {
          statement = this.parseEmptyStatement()
        } else {
          statement = this.parseExpressionStatement()
        }
      case Token.Identifier =>
        statement = if (this.matchAsyncFunction()) this.parseFunctionDeclaration() else this.parseLabelledStatement()
      case Token.Keyword =>
        this.lookahead.value match {
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
  
  // https://tc39.github.io/ecma262/#sec-function-definitions
  def parseFunctionSourceElements(): Node.BlockStatement = {
    val node = this.createNode()
    this.expect("{")
    val body = this.parseDirectivePrologues()
    val previousLabelSet = this.context.labelSet
    val previousInIteration = this.context.inIteration
    val previousInSwitch = this.context.inSwitch
    val previousInFunctionBody = this.context.inFunctionBody
    this.context.labelSet = new {}
    this.context.inIteration = false
    this.context.inSwitch = false
    this.context.inFunctionBody = true
    while (this.lookahead.`type` != Token.EOF) {
      if (this.`match`("}")) {
        /* Unsupported: Break */ break;
      }
      body.push(this.parseStatementListItem())
    }
    this.expect("}")
    this.context.labelSet = previousLabelSet
    this.context.inIteration = previousInIteration
    this.context.inSwitch = previousInSwitch
    this.context.inFunctionBody = previousInFunctionBody
    this.finalize(node, new Node.BlockStatement(body))
  }
  
  def validateParam(options: AnyRef, param: String, name: String) = {
    val key = "$" + name
    if (this.context.strict) {
      if (this.scanner.isRestrictedWord(name)) {
        options.stricted = param
        options.message = Messages.StrictParamName
      }
      if (Object.prototype.hasOwnProperty.call(options.paramSet, key)) {
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
      } else if (Object.prototype.hasOwnProperty.call(options.paramSet, key)) {
        options.stricted = param
        options.message = Messages.StrictParamDupe
      }
    }
    /*istanbul ignore next */
    if (Object.defineProperty.getClass == "function") {
      Object.defineProperty(options.paramSet, key, new {
        var value = true
        var enumerable = true
        var writable = true
        var configurable = true
      })
    } else {
      options.paramSet(key) = true
    }
  }
  
  def parseRestElement(params: Array[Any]): Node.RestElement = {
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
  
  def parseFormalParameter(options: AnyRef) = {
    val params = Array.empty[Any]
    val param = if (this.`match`("...")) this.parseRestElement(params) else this.parsePatternWithDefault(params)
    for (i <- params) {
      this.validateParam(options, i, i.value)
    }
    options.simple = options.simple && param.isInstanceOf[Node.Identifier]
    options.params.push(param)
  }
  
  def parseFormalParameters(firstRestricted: RawToken) = {
    object options {
      var simple = true
      var params = Array()
      var firstRestricted = firstRestricted
    }
    this.expect("(")
    if (!this.`match`(")")) {
      options.paramSet = new {}
      while (this.lookahead.`type` != Token.EOF) {
        this.parseFormalParameter(options)
        if (this.`match`(")")) {
          /* Unsupported: Break */ break;
        }
        this.expect(",")
        if (this.`match`(")")) {
          /* Unsupported: Break */ break;
        }
      }
    }
    this.expect(")")
    new {
      var simple = options.simple
      var params = options.params
      var stricted = options.stricted
      var firstRestricted = options.firstRestricted
      var message = options.message
    }
  }
  
  def matchAsyncFunction(): Boolean = {
    var `match` = this.matchContextualKeyword("async")
    if (`match`) {
      val state = this.scanner.saveState()
      this.scanner.scanComments()
      val next = this.scanner.lex()
      this.scanner.restoreState(state)
      `match` = state.lineNumber == next.lineNumber && next.`type` == Token.Keyword && next.value == "function"
    }
    `match`
  }
  
  def parseFunctionDeclaration(identifierIsOptional: Boolean): Node.AsyncFunctionDeclaration | Node.FunctionDeclaration = {
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
    var message: String = _
    var id: Node.Identifier = null
    var firstRestricted: RawToken = null
    if (!identifierIsOptional || !this.`match`("(")) {
      val token = this.lookahead
      id = this.parseVariableIdentifier()
      if (this.context.strict) {
        if (this.scanner.isRestrictedWord(token.value.asInstanceOf[String])) {
          this.tolerateUnexpectedToken(token, Messages.StrictFunctionName)
        }
      } else {
        if (this.scanner.isRestrictedWord(token.value.asInstanceOf[String])) {
          firstRestricted = token
          message = Messages.StrictFunctionName
        } else if (this.scanner.isStrictModeReservedWord(token.value.asInstanceOf[String])) {
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
  
  def parseFunctionExpression(): Node.AsyncFunctionExpression | Node.FunctionExpression = {
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
    var message: String = _
    var id: Node.Identifier = null
    var firstRestricted: RawToken = _
    val previousAllowAwait = this.context.await
    val previousAllowYield = this.context.allowYield
    this.context.await = isAsync
    this.context.allowYield = !isGenerator
    if (!this.`match`("(")) {
      val token = this.lookahead
      id = if (!this.context.strict && !isGenerator && this.matchKeyword("yield")) this.parseIdentifierName() else this.parseVariableIdentifier()
      if (this.context.strict) {
        if (this.scanner.isRestrictedWord(token.value.asInstanceOf[String])) {
          this.tolerateUnexpectedToken(token, Messages.StrictFunctionName)
        }
      } else {
        if (this.scanner.isRestrictedWord(token.value.asInstanceOf[String])) {
          firstRestricted = token
          message = Messages.StrictFunctionName
        } else if (this.scanner.isStrictModeReservedWord(token.value.asInstanceOf[String])) {
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
  
  // https://tc39.github.io/ecma262/#sec-directive-prologues-and-the-use-strict-directive
  def parseDirective(): Node.Directive | Node.ExpressionStatement = {
    val token = this.lookahead
    val node = this.createNode()
    val expr = this.parseExpression()
    val directive = if (expr.`type` == Syntax.Literal) this.getTokenRaw(token).slice(1, -1) else null
    this.consumeSemicolon()
    this.finalize(node, if (directive) new Node.Directive(expr, directive) else new Node.ExpressionStatement(expr))
  }
  
  def parseDirectivePrologues(): Array[Node.Statement] = {
    var firstRestricted: RawToken = null
    val body = Array.empty[Node.Statement]
    while (true) {
      val token = this.lookahead
      if (token.`type` != Token.StringLiteral) {
        /* Unsupported: Break */ break;
      }
      val statement = this.parseDirective()
      body.push(statement)
      val directive = statement.asInstanceOf[Node.Directive].directive
      if (directive.getClass != "string") {
        /* Unsupported: Break */ break;
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
    body
  }
  
  // https://tc39.github.io/ecma262/#sec-method-definitions
  def qualifiedPropertyName(token: RawToken): Boolean = {
    token.`type` match {
      case Token.Identifier | Token.StringLiteral | Token.BooleanLiteral | Token.NullLiteral | Token.NumericLiteral | Token.Keyword =>
        return true
      case Token.Punctuator =>
        return token.value == "["
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
  
  def parseSetterMethod(): Node.FunctionExpression = {
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
  
  def parseGeneratorMethod(): Node.FunctionExpression = {
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
  
  // https://tc39.github.io/ecma262/#sec-generator-function-definitions
  def isStartOfExpression(): Boolean = {
    var start = true
    val value = this.lookahead.value
    this.lookahead.`type` match {
      case Token.Punctuator =>
        start = value == "[" || value == "(" || value == "{" || value == "+" || value == "-" || value == "!" || value == "~" || value == "++" || value == "--" || value == "/" || value == "/=" // regular expression literal
      case Token.Keyword =>
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
  
  // https://tc39.github.io/ecma262/#sec-class-definitions
  def parseClassElement(hasConstructor: AnyRef): Node.Property = {
    var token = this.lookahead
    val node = this.createNode()
    var kind: String = ""
    var key: Identifier | Literal = null
    var value: Node.FunctionExpression = null
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
      if (token.`type` == Token.Identifier && !this.hasLineTerminator && token.value == "async") {
        val punctuator = this.lookahead.value
        if (punctuator != ":" && punctuator != "(" && punctuator != "*") {
          isAsync = true
          token = this.lookahead
          key = this.parseObjectPropertyKey()
          if (token.`type` == Token.Identifier) {
            if (token.value == "get" || token.value == "set") {
              this.tolerateUnexpectedToken(token)
            } else if (token.value == "constructor") {
              this.tolerateUnexpectedToken(token, Messages.ConstructorIsAsync)
            }
          }
        }
      }
    }
    val lookaheadPropertyKey = this.qualifiedPropertyName(this.lookahead)
    if (token.`type` == Token.Identifier) {
      if (token.value == "get" && lookaheadPropertyKey) {
        kind = "get"
        computed = this.`match`("[")
        key = this.parseObjectPropertyKey()
        this.context.allowYield = false
        value = this.parseGetterMethod()
      } else if (token.value == "set" && lookaheadPropertyKey) {
        kind = "set"
        computed = this.`match`("[")
        key = this.parseObjectPropertyKey()
        value = this.parseSetterMethod()
      }
    } else if (token.`type` == Token.Punctuator && token.value == "*" && lookaheadPropertyKey) {
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
        if (kind != "method" || !method || value && value.generator) {
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
  
  def parseClassElementList(): Array[Node.Property] = {
    val body = Array.empty[Node.Property]
    object hasConstructor {
      var value = false
    }
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
  
  def parseClassDeclaration(identifierIsOptional: Boolean): Node.ClassDeclaration = {
    val node = this.createNode()
    val previousStrict = this.context.strict
    this.context.strict = true
    this.expectKeyword("class")
    val id = if (identifierIsOptional && this.lookahead.`type` != Token.Identifier) null else this.parseVariableIdentifier()
    var superClass: Node.Identifier = null
    if (this.matchKeyword("extends")) {
      this.nextToken()
      superClass = this.isolateCoverGrammar(this.parseLeftHandSideExpressionAllowCall)
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
    val id = if (this.lookahead.`type` == Token.Identifier) this.parseVariableIdentifier() else null
    var superClass: Node.Identifier = null
    if (this.matchKeyword("extends")) {
      this.nextToken()
      superClass = this.isolateCoverGrammar(this.parseLeftHandSideExpressionAllowCall)
    }
    val classBody = this.parseClassBody()
    this.context.strict = previousStrict
    this.finalize(node, new Node.ClassExpression(id, superClass, classBody))
  }
  
  // https://tc39.github.io/ecma262/#sec-scripts
  // https://tc39.github.io/ecma262/#sec-modules
  def parseModule(): Node.Module = {
    this.context.strict = true
    this.context.isModule = true
    this.scanner.isModule = true
    val node = this.createNode()
    val body = this.parseDirectivePrologues()
    while (this.lookahead.`type` != Token.EOF) {
      body.push(this.parseStatementListItem())
    }
    this.finalize(node, new Node.Module(body))
  }
  
  def parseScript(): Node.Script = {
    val node = this.createNode()
    val body = this.parseDirectivePrologues()
    while (this.lookahead.`type` != Token.EOF) {
      body.push(this.parseStatementListItem())
    }
    this.finalize(node, new Node.Script(body))
  }
  
  // https://tc39.github.io/ecma262/#sec-imports
  def parseModuleSpecifier(): Node.Literal = {
    val node = this.createNode()
    if (this.lookahead.`type` != Token.StringLiteral) {
      this.throwError(Messages.InvalidModuleSpecifier)
    }
    val token = this.nextToken()
    val raw = this.getTokenRaw(token)
    this.finalize(node, new Node.Literal(token.value.asInstanceOf[String], raw))
  }
  
  // import {<foo as bar>} ...;
  def parseImportSpecifier(): Node.ImportSpecifier = {
    val node = this.createNode()
    var imported: Node.Identifier = _
    var local: Node.Identifier = _
    if (this.lookahead.`type` == Token.Identifier) {
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
  
  // {foo, bar as bas}
  def parseNamedImports(): Array[Node.ImportSpecifier] = {
    this.expect("{")
    val specifiers = Array.empty[Node.ImportSpecifier]
    while (!this.`match`("}")) {
      specifiers.push(this.parseImportSpecifier())
      if (!this.`match`("}")) {
        this.expect(",")
      }
    }
    this.expect("}")
    specifiers
  }
  
  // import <foo> ...;
  def parseImportDefaultSpecifier(): Node.ImportDefaultSpecifier = {
    val node = this.createNode()
    val local = this.parseIdentifierName()
    this.finalize(node, new Node.ImportDefaultSpecifier(local))
  }
  
  // import <* as foo> ...;
  def parseImportNamespaceSpecifier(): Node.ImportNamespaceSpecifier = {
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
    var src: Node.Literal = _
    var specifiers = Array.empty[Node.ImportDeclarationSpecifier]
    if (this.lookahead.`type` == Token.StringLiteral) {
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
  
  // https://tc39.github.io/ecma262/#sec-exports
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
  
  def parseExportDeclaration(): ExportAllDeclaration | ExportDefaultDeclaration | ExportNamedDeclaration = {
    if (this.context.inFunctionBody) {
      this.throwError(Messages.IllegalExportDeclaration)
    }
    val node = this.createNode()
    this.expectKeyword("export")
    var exportDeclaration = _
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
    } else if (this.lookahead.`type` == Token.Keyword) {
      // export var f = 1;
      var declaration: Any = _
      this.lookahead.value match {
        case "let" | "const" =>
          declaration = this.parseLexicalDeclaration(new {
            var inFor = false
          })
        case "var" | "class" | "function" =>
          declaration = this.parseStatementListItem()
        case _ =>
          this.throwUnexpectedToken(this.lookahead)
      }
      exportDeclaration = this.finalize(node, new Node.ExportNamedDeclaration(declaration, Array(), null))
    } else if (this.matchAsyncFunction()) {
      val declaration = this.parseFunctionDeclaration()
      exportDeclaration = this.finalize(node, new Node.ExportNamedDeclaration(declaration, Array(), null))
    } else {
      val specifiers = Array.empty[Node.ExportSpecifier]
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


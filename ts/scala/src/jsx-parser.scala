/*
ScalaFromJS: Dev 2020-02-24 19:42:52
jsx-parser.ts
*/

package esprima
/* import { Character } from './character' */
/* import * as JSXNode from './jsx-nodes' */
/* import { JSXSyntax } from './jsx-syntax' */
/* import * as Node from './nodes' */
/* import { Marker, Parser } from './parser' */
/* import { Token, TokenName } from './token' */
/* import { XHTMLEntities } from './xhtml-entities' */
trait MetaJSXElement() {
  var node: Marker = _
  var opening: JSXNode.JSXOpeningElement = _
  var closing: JSXNode.JSXClosingElement | Null = _
  var children = Array.empty[JSXNode.JSXChild]
}

object JSXToken extends Enumeration {
  val Identifier = Value(100)
  val Text = Value()
}

trait RawJSXToken() {
  var `type`: Token | JSXToken = _
  var value: String = _
  var lineNumber: Double = _
  var lineStart: Double = _
  var start: Double = _
  var end: Double = _
}

TokenName(JSXToken.Identifier) = "JSXIdentifier"
TokenName(JSXToken.Text) = "JSXText"
// Fully qualified element name, e.g. <svg:path> returns "svg:path"

def getQualifiedElementName(elementName: JSXNode.JSXElementName): String = {
  var qualifiedName: String = _
  elementName.`type` match {
    case JSXSyntax.JSXIdentifier =>
      val id = elementName.asInstanceOf[StaticMemberExpression(Identifier(JSXNode),Identifier(JSXIdentifier))]
      qualifiedName = id.name
    case JSXSyntax.JSXNamespacedName =>
      val ns = elementName.asInstanceOf[StaticMemberExpression(Identifier(JSXNode),Identifier(JSXNamespacedName))]
      qualifiedName = getQualifiedElementName(ns.namespace) + ":" + getQualifiedElementName(ns.name)
    case JSXSyntax.JSXMemberExpression =>
      val expr = elementName.asInstanceOf[StaticMemberExpression(Identifier(JSXNode),Identifier(JSXMemberExpression))]
      qualifiedName = getQualifiedElementName(expr.`object`) + "." + getQualifiedElementName(expr.property)
      /*istanbul ignore next */
    case _ =>
  }
  qualifiedName
}

class JSXParser(code: String, options: Any, delegate: (AnyRef, AnyRef) => Any) extends Parser(code, options, delegate) {
  override def parsePrimaryExpression(): Node.Expression | JSXNode.JSXElement = {
    if (this.`match`("<")) this.parseJSXRoot() else super.parsePrimaryExpression()
  }
  
  def startJSX() = {
    // Unwind the scanner before the lookahead token.
    this.scanner.index = this.startMarker.index
    this.scanner.lineNumber = this.startMarker.line
    this.scanner.lineStart = this.startMarker.index - this.startMarker.column
  }
  
  def finishJSX() = {
    // Prime the next lookahead.
    this.nextToken()
  }
  
  def reenterJSX() = {
    this.startJSX()
    this.expectJSX("}")
    // Pop the closing '}' added from the lookahead.
    if (this.config.tokens) {
      this.tokens.pop()
    }
  }
  
  def createJSXNode(): Marker = {
    this.collectComments()
    new {
      var index = this.scanner.index
      var line = this.scanner.lineNumber
      var column = this.scanner.index - this.scanner.lineStart
    }
  }
  
  def createJSXChildNode(): Marker = {
    new {
      var index = this.scanner.index
      var line = this.scanner.lineNumber
      var column = this.scanner.index - this.scanner.lineStart
    }
  }
  
  def scanXHTMLEntity(quote: String): String = {
    var result = "&"
    var valid = true
    var terminated = false
    var numeric = false
    var hex = false
    while (!this.scanner.eof() && valid && !terminated) {
      val ch = this.scanner.source(this.scanner.index)
      if (ch == quote) {
        /* Unsupported: Break */ break;
      }
      terminated = ch == ";"
      result += ch
      this.scanner.index += 1
      if (!terminated) {
        result.length match {
          case 2 =>
            // e.g. '&#123;'
            numeric = ch == "#"
          case 3 =>
            if (numeric) {
              // e.g. '&#x41;'
              hex = ch == "x"
              valid = hex || Character.isDecimalDigit(ch.charCodeAt(0))
              numeric = numeric && !hex
            }
          case _ =>
            valid = valid && !(numeric && !Character.isDecimalDigit(ch.charCodeAt(0)))
            valid = valid && !(hex && !Character.isHexDigit(ch.charCodeAt(0)))
        }
      }
    }
    if (valid && terminated && result.length > 2) {
      // e.g. '&#x41;' becomes just '#x41'
      val str = result.substr(1, result.length - 2)
      if (numeric && str.length > 1) {
        result = String.fromCharCode(parseInt(str.substr(1), 10))
      } else if (hex && str.length > 2) {
        result = String.fromCharCode(parseInt("0" + str.substr(1), 16))
      } else if (!numeric && !hex && XHTMLEntities(str)) {
        result = XHTMLEntities(str)
      }
    }
    result
  }
  
  // Scan the next JSX token. This replaces Scanner#lex when in JSX mode.
  def lexJSX(): RawJSXToken = {
    val cp = this.scanner.source.charCodeAt(this.scanner.index)
    // < > / : = { }
    if (cp == 60 || cp == 62 || cp == 47 || cp == 58 || cp == 61 || cp == 123 || cp == 125) {
      val value = this.scanner.source({
        val temp = this.scanner.index
        this.scanner.index += 1
        temp
      })
      return new {
        var `type` = Token.Punctuator
        var value = value
        var lineNumber = this.scanner.lineNumber
        var lineStart = this.scanner.lineStart
        var start = this.scanner.index - 1
        var end = this.scanner.index
      }
    }
    // " '
    if (cp == 34 || cp == 39) {
      val start = this.scanner.index
      val quote = this.scanner.source({
        val temp = this.scanner.index
        this.scanner.index += 1
        temp
      })
      var str = ""
      while (!this.scanner.eof()) {
        val ch = this.scanner.source({
          val temp = this.scanner.index
          this.scanner.index += 1
          temp
        })
        if (ch == quote) {
          /* Unsupported: Break */ break;
        } else if (ch == "&") {
          str += this.scanXHTMLEntity(quote)
        } else {
          str += ch
        }
      }
      return new {
        var `type` = Token.StringLiteral
        var value = str
        var lineNumber = this.scanner.lineNumber
        var lineStart = this.scanner.lineStart
        var start = start
        var end = this.scanner.index
      }
    }
    // ... or .
    if (cp == 46) {
      val n1 = this.scanner.source.charCodeAt(this.scanner.index + 1)
      val n2 = this.scanner.source.charCodeAt(this.scanner.index + 2)
      val value = if (n1 == 46 && n2 == 46) "..." else "."
      val start = this.scanner.index
      this.scanner.index += value.length
      return new {
        var `type` = Token.Punctuator
        var value = value
        var lineNumber = this.scanner.lineNumber
        var lineStart = this.scanner.lineStart
        var start = start
        var end = this.scanner.index
      }
    }
    // `
    if (cp == 96) {
      // Only placeholder, since it will be rescanned as a real assignment expression.
      return new {
        var `type` = Token.Template
        var value = ""
        var lineNumber = this.scanner.lineNumber
        var lineStart = this.scanner.lineStart
        var start = this.scanner.index
        var end = this.scanner.index
      }
    }
    // Identifer can not contain backslash (char code 92).
    if (Character.isIdentifierStart(cp) && cp != 92) {
      val start = this.scanner.index
      this.scanner.index += 1
      while (!this.scanner.eof()) {
        val ch = this.scanner.source.charCodeAt(this.scanner.index)
        if (Character.isIdentifierPart(ch) && ch != 92) {
          this.scanner.index += 1
        } else if (ch == 45) {
          // Hyphen (char code 45) can be part of an identifier.
          this.scanner.index += 1
        } else {
          /* Unsupported: Break */ break;
        }
      }
      val id = this.scanner.source.slice(start, this.scanner.index)
      return new {
        var `type` = JSXToken.Identifier
        var value = id
        var lineNumber = this.scanner.lineNumber
        var lineStart = this.scanner.lineStart
        var start = start
        var end = this.scanner.index
      }
    }
    this.scanner.lex().asInstanceOf[RawJSXToken]
  }
  
  def nextJSXToken(): RawJSXToken = {
    this.collectComments()
    this.startMarker.index = this.scanner.index
    this.startMarker.line = this.scanner.lineNumber
    this.startMarker.column = this.scanner.index - this.scanner.lineStart
    val token = this.lexJSX()
    this.lastMarker.index = this.scanner.index
    this.lastMarker.line = this.scanner.lineNumber
    this.lastMarker.column = this.scanner.index - this.scanner.lineStart
    if (this.config.tokens) {
      this.tokens.push(this.convertToken(token.asInstanceOf[Identifier(any)]))
    }
    token
  }
  
  def nextJSXText(): RawJSXToken = {
    this.startMarker.index = this.scanner.index
    this.startMarker.line = this.scanner.lineNumber
    this.startMarker.column = this.scanner.index - this.scanner.lineStart
    val start = this.scanner.index
    var text = ""
    while (!this.scanner.eof()) {
      val ch = this.scanner.source(this.scanner.index)
      if (ch == "{" || ch == "<") {
        /* Unsupported: Break */ break;
      }
      this.scanner.index += 1
      text += ch
      if (Character.isLineTerminator(ch.charCodeAt(0))) {
        this.scanner.lineNumber += 1
        if (ch == "\r" && this.scanner.source(this.scanner.index) == "\n") {
          this.scanner.index += 1
        }
        this.scanner.lineStart = this.scanner.index
      }
    }
    this.lastMarker.index = this.scanner.index
    this.lastMarker.line = this.scanner.lineNumber
    this.lastMarker.column = this.scanner.index - this.scanner.lineStart
    object token {
      var `type` = JSXToken.Text
      var value = text
      var lineNumber = this.scanner.lineNumber
      var lineStart = this.scanner.lineStart
      var start = start
      var end = this.scanner.index
    }
    if (text.length > 0 && this.config.tokens) {
      this.tokens.push(this.convertToken(token.asInstanceOf[Identifier(any)]))
    }
    token
  }
  
  def peekJSXToken(): RawJSXToken = {
    val state = this.scanner.saveState()
    this.scanner.scanComments()
    val next = this.lexJSX()
    this.scanner.restoreState(state)
    next
  }
  
  // Expect the next JSX token to match the specified punctuator.
  // If not, an exception will be thrown.
  def expectJSX(value: String) = {
    val token = this.nextJSXToken()
    if (token.`type` != Token.Punctuator || token.value != value) {
      this.throwUnexpectedToken(token)
    }
  }
  
  // Return true if the next JSX token matches the specified punctuator.
  def matchJSX(value: String) = {
    val next = this.peekJSXToken()
    next.`type` == Token.Punctuator && next.value == value
  }
  
  def parseJSXIdentifier(): JSXNode.JSXIdentifier = {
    val node = this.createJSXNode()
    val token = this.nextJSXToken()
    if (token.`type` != JSXToken.Identifier) {
      this.throwUnexpectedToken(token)
    }
    this.finalize(node, new JSXNode.JSXIdentifier(token.value))
  }
  
  def parseJSXElementName(): JSXNode.JSXElementName = {
    val node = this.createJSXNode()
    var elementName = this.parseJSXIdentifier()
    if (this.matchJSX(":")) {
      val namespace = elementName
      this.expectJSX(":")
      val name = this.parseJSXIdentifier()
      elementName = this.finalize(node, new JSXNode.JSXNamespacedName(namespace, name))
    } else if (this.matchJSX(".")) {
      while (this.matchJSX(".")) {
        val `object` = elementName
        this.expectJSX(".")
        val property = this.parseJSXIdentifier()
        elementName = this.finalize(node, new JSXNode.JSXMemberExpression(`object`, property))
      }
    }
    elementName
  }
  
  def parseJSXAttributeName(): JSXNode.JSXAttributeName = {
    val node = this.createJSXNode()
    var attributeName: JSXNode.JSXAttributeName = _
    val identifier = this.parseJSXIdentifier()
    if (this.matchJSX(":")) {
      val namespace = identifier
      this.expectJSX(":")
      val name = this.parseJSXIdentifier()
      attributeName = this.finalize(node, new JSXNode.JSXNamespacedName(namespace, name))
    } else {
      attributeName = identifier
    }
    attributeName
  }
  
  def parseJSXStringLiteralAttribute(): Node.Literal = {
    val node = this.createJSXNode()
    val token = this.nextJSXToken()
    if (token.`type` != Token.StringLiteral) {
      this.throwUnexpectedToken(token)
    }
    val raw = this.getTokenRaw(token)
    this.finalize(node, new Node.Literal(token.value, raw))
  }
  
  def parseJSXExpressionAttribute(): JSXNode.JSXExpressionContainer = {
    val node = this.createJSXNode()
    this.expectJSX("{")
    this.finishJSX()
    if (this.`match`("}")) {
      this.tolerateError("JSX attributes must only be assigned a non-empty expression")
    }
    val expression = this.parseAssignmentExpression()
    this.reenterJSX()
    this.finalize(node, new JSXNode.JSXExpressionContainer(expression))
  }
  
  def parseJSXAttributeValue(): JSXNode.JSXAttributeValue = {
    if (this.matchJSX("{")) this.parseJSXExpressionAttribute() else if (this.matchJSX("<")) this.parseJSXElement() else this.parseJSXStringLiteralAttribute()
  }
  
  def parseJSXNameValueAttribute(): JSXNode.JSXAttribute = {
    val node = this.createJSXNode()
    val name = this.parseJSXAttributeName()
    var value: JSXNode.JSXAttributeValue | Null = null
    if (this.matchJSX("=")) {
      this.expectJSX("=")
      value = this.parseJSXAttributeValue()
    }
    this.finalize(node, new JSXNode.JSXAttribute(name, value))
  }
  
  def parseJSXSpreadAttribute(): JSXNode.JSXSpreadAttribute = {
    val node = this.createJSXNode()
    this.expectJSX("{")
    this.expectJSX("...")
    this.finishJSX()
    val argument = this.parseAssignmentExpression()
    this.reenterJSX()
    this.finalize(node, new JSXNode.JSXSpreadAttribute(argument))
  }
  
  def parseJSXAttributes(): Array[JSXNode.JSXElementAttribute] = {
    val attributes = Array.empty[JSXNode.JSXElementAttribute]
    while (!this.matchJSX("/") && !this.matchJSX(">")) {
      val attribute = if (this.matchJSX("{")) this.parseJSXSpreadAttribute() else this.parseJSXNameValueAttribute()
      attributes.push(attribute)
    }
    attributes
  }
  
  def parseJSXOpeningElement(): JSXNode.JSXOpeningElement = {
    val node = this.createJSXNode()
    this.expectJSX("<")
    val name = this.parseJSXElementName()
    val attributes = this.parseJSXAttributes()
    val selfClosing = this.matchJSX("/")
    if (selfClosing) {
      this.expectJSX("/")
    }
    this.expectJSX(">")
    this.finalize(node, new JSXNode.JSXOpeningElement(name, selfClosing, attributes))
  }
  
  def parseJSXBoundaryElement(): JSXNode.JSXOpeningElement | JSXNode.JSXClosingElement = {
    val node = this.createJSXNode()
    this.expectJSX("<")
    if (this.matchJSX("/")) {
      this.expectJSX("/")
      val name = this.parseJSXElementName()
      this.expectJSX(">")
      return this.finalize(node, new JSXNode.JSXClosingElement(name))
    }
    val name = this.parseJSXElementName()
    val attributes = this.parseJSXAttributes()
    val selfClosing = this.matchJSX("/")
    if (selfClosing) {
      this.expectJSX("/")
    }
    this.expectJSX(">")
    this.finalize(node, new JSXNode.JSXOpeningElement(name, selfClosing, attributes))
  }
  
  def parseJSXEmptyExpression(): JSXNode.JSXEmptyExpression = {
    val node = this.createJSXChildNode()
    this.collectComments()
    this.lastMarker.index = this.scanner.index
    this.lastMarker.line = this.scanner.lineNumber
    this.lastMarker.column = this.scanner.index - this.scanner.lineStart
    this.finalize(node, new JSXNode.JSXEmptyExpression())
  }
  
  def parseJSXExpressionContainer(): JSXNode.JSXExpressionContainer = {
    val node = this.createJSXNode()
    this.expectJSX("{")
    var expression: Node.Expression | JSXNode.JSXEmptyExpression = _
    if (this.matchJSX("}")) {
      expression = this.parseJSXEmptyExpression()
      this.expectJSX("}")
    } else {
      this.finishJSX()
      expression = this.parseAssignmentExpression()
      this.reenterJSX()
    }
    this.finalize(node, new JSXNode.JSXExpressionContainer(expression))
  }
  
  def parseJSXChildren(): Array[JSXNode.JSXChild] = {
    val children = Array.empty[JSXNode.JSXChild]
    while (!this.scanner.eof()) {
      val node = this.createJSXChildNode()
      val token = this.nextJSXText()
      if (token.start < token.end) {
        val raw = this.getTokenRaw(token)
        val child = this.finalize(node, new JSXNode.JSXText(token.value, raw))
        children.push(child)
      }
      if (this.scanner.source(this.scanner.index) == "{") {
        val container = this.parseJSXExpressionContainer()
        children.push(container)
      } else {
        /* Unsupported: Break */ break;
      }
    }
    children
  }
  
  def parseComplexJSXElement(el_par: MetaJSXElement): MetaJSXElement = {
    var el: MetaJSXElement = el_par
    val stack = Array.empty[MetaJSXElement]
    while (!this.scanner.eof()) {
      el.children = el.children.concat(this.parseJSXChildren())
      val node = this.createJSXChildNode()
      val element = this.parseJSXBoundaryElement()
      element match {
        case element_cast: JSXOpeningElement =>
          val opening = element_cast.asInstanceOf[StaticMemberExpression(Identifier(JSXNode),Identifier(JSXOpeningElement))]
          if (opening.selfClosing) {
            val child = this.finalize(node, new JSXNode.JSXElement(opening, Array(), null))
            el.children.push(child)
          } else {
            stack.push(el)
            el = new {
              var node = node
              var opening = opening
              var closing = null
              var children = Array()
            }
          }
        case _ =>
      }
      element match {
        case element_cast: JSXClosingElement =>
          el.closing = element_cast.asInstanceOf[StaticMemberExpression(Identifier(JSXNode),Identifier(JSXClosingElement))]
          val open = getQualifiedElementName(el.opening.name)
          val close = getQualifiedElementName(el.closing.name)
          if (open != close) {
            this.tolerateError("Expected corresponding JSX closing tag for %0", open)
          }
          if (stack.length > 0) {
            val child = this.finalize(el.node, new JSXNode.JSXElement(el.opening, el.children, el.closing))
            el = stack(stack.length - 1)
            el.children.push(child)
            stack.pop()
          } else {
            /* Unsupported: Break */ break;
          }
        case _ =>
      }
    }
    el
  }
  
  def parseJSXElement(): JSXNode.JSXElement = {
    val node = this.createJSXNode()
    val opening = this.parseJSXOpeningElement()
    var children = Array.empty[JSXNode.JSXChild]
    var closing: JSXNode.JSXClosingElement | Null = null
    if (!opening.selfClosing) {
      val el = this.parseComplexJSXElement(new {
        var node = node
        var opening = opening
        var closing = closing
        var children = children
      })
      children = el.children
      closing = el.closing
    }
    this.finalize(node, new JSXNode.JSXElement(opening, children, closing))
  }
  
  def parseJSXRoot(): JSXNode.JSXElement = {
    // Pop the opening '<' added from the lookahead.
    if (this.config.tokens) {
      this.tokens.pop()
    }
    this.startJSX()
    val element = this.parseJSXElement()
    this.finishJSX()
    element
  }
  
  override def isStartOfExpression(): Boolean = {
    super.isStartOfExpression() || this.`match`("<")
  }
  
}


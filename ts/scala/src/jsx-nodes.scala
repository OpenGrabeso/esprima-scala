/*
ScalaFromJS: Dev 2020-02-22 18:14:57
jsx-nodes.ts
*/

package esprima

/* import { JSXSyntax } from './jsx-syntax' */
/* import * as Node from './nodes' */
type JSXAttributeName = JSXIdentifier | JSXNamespacedName
type JSXAttributeValue = Node.Literal | JSXElement | JSXSpreadAttribute | JSXExpressionContainer
type JSXChild = JSXElement | JSXExpressionContainer | JSXText
type JSXElementAttribute = JSXAttribute | JSXSpreadAttribute
type JSXElementName = JSXIdentifier | JSXNamespacedName | JSXMemberExpression

class JSXClosingElement(var name: JSXElementName) {
  var `type`: String = JSXSyntax.JSXClosingElement
}

class JSXElement(var openingElement: JSXOpeningElement, var children: Array[JSXChild], var closingElement: JSXClosingElement | Null) {
  var `type`: String = JSXSyntax.JSXElement
}

class JSXEmptyExpression() {
  var `type`: String = JSXSyntax.JSXEmptyExpression
}

class JSXExpressionContainer(var expression: Node.Expression | JSXEmptyExpression) {
  var `type`: String = JSXSyntax.JSXExpressionContainer
}

class JSXIdentifier(var name: String) {
  var `type`: String = JSXSyntax.JSXIdentifier
}

class JSXMemberExpression(var `object`: JSXMemberExpression | JSXIdentifier, var property: JSXIdentifier) {
  var `type`: String = JSXSyntax.JSXMemberExpression
}

class JSXAttribute(var name: JSXAttributeName, var value: JSXAttributeValue | Null) {
  var `type`: String = JSXSyntax.JSXAttribute
}

class JSXNamespacedName(var namespace: JSXIdentifier, var name: JSXIdentifier) {
  var `type`: String = JSXSyntax.JSXNamespacedName
}

class JSXOpeningElement(var name: JSXElementName, var selfClosing: Boolean, var attributes: Array[JSXElementAttribute]) {
  var `type`: String = JSXSyntax.JSXOpeningElement
}

class JSXSpreadAttribute(var argument: Node.Expression) {
  var `type`: String = JSXSyntax.JSXSpreadAttribute
}

class JSXText(var value: String, var raw: String) {
  var `type`: String = JSXSyntax.JSXText
}


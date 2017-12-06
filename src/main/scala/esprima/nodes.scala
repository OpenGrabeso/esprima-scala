/*
ScalaFromJS: 2017-12-06 21:28:23.723
nodes.js
*/

package esprima

import esprima.port.RegExp

import scala.collection.mutable.ArrayBuffer

object Node {

  trait Node {

    var `type`: String
    var range: (Int, Int) = _
    var loc: Scanner.SourceLocation = _

    var leadingComments: ArrayBuffer[CommentHandler.Comment] = _
    var innerComments: ArrayBuffer[CommentHandler.Comment] = _
    var trailingComments: ArrayBuffer[CommentHandler.Comment] = _
  }

  abstract class CommentNode extends Node {
    var value: String = _
  }

  class ArrayExpression(var elements: Array[Node]) extends Node {
    var `type` = Syntax.ArrayExpression
  }


  class ArrayPattern(var elements: Array[Node]) extends Node {
    var `type` = Syntax.ArrayPattern
  }


  class ArrowFunctionExpression(var params: Any, var body: Any, var expression: Any) extends Node {
    var `type` = Syntax.ArrowFunctionExpression
    var id = null
    var generator: Boolean = false
    var async: Boolean = false
  }


  class AssignmentExpression(var operator: String, var left: Node, var right: Node) extends Node {
    var `type` = Syntax.AssignmentExpression
  }


  class AssignmentPattern(var left: Node, var right: Node) extends Node {
    var `type` = Syntax.AssignmentPattern
  }


  class AsyncArrowFunctionExpression(var params: Any, var body: Any, var expression: Node) extends Node {
    var `type` = Syntax.ArrowFunctionExpression
    var id = null
    var generator: Boolean = false
    var async: Boolean = true
  }


  class AsyncFunctionDeclaration(var id: Any, var params: Any, var body: Any) extends Node {
    var `type` = Syntax.FunctionDeclaration
    var generator: Boolean = false
    var expression: Boolean = false
    var async: Boolean = true
  }


  class AsyncFunctionExpression(var id: Any, var params: Any, var body: Any) extends Node {
    var `type` = Syntax.FunctionExpression
    var generator: Boolean = false
    var expression: Boolean = false
    var async: Boolean = true
  }


  class AwaitExpression(var argument: Node) extends Node {
    var `type` = Syntax.AwaitExpression
  }


  class BinaryExpression(operator_par: String, left_par: Node, right_par: Node) extends Node {
    var `type`: String = _
    var operator: String = _
    var left: Any = _
    var right: Any = _
    this.constructor(operator_par, left_par, right_par)

    def constructor(operator: String, left: Any, right: Any) = {
      val logical = operator == "||" || operator == "&&"
      this.`type` = if (logical) Syntax.LogicalExpression else Syntax.BinaryExpression
      this.operator = operator
      this.left = left
      this.right = right
    }

  }


  class BlockStatement(var body: Seq[Node]) extends Node {
    var `type` = Syntax.BlockStatement
  }


  class BreakStatement(var label: Any) extends Node {
    var `type` = Syntax.BreakStatement
  }


  class CallExpression(var callee: Any, var arguments: Any) extends Node {
    var `type` = Syntax.CallExpression
  }


  class CatchClause(var param: Any, var body: Any) extends Node {
    var `type` = Syntax.CatchClause
  }


  class ClassBody(var body: Array[MethodDefinition]) extends Node {
    var `type` = Syntax.ClassBody
  }


  class ClassDeclaration(var id: Any, var superClass: Any, var body: Any) extends Node {
    var `type` = Syntax.ClassDeclaration
  }


  class ClassExpression(var id: Any, var superClass: Any, var body: Any) extends Node {
    var `type` = Syntax.ClassExpression
  }


  class ComputedMemberExpression(var `object`: Any, var property: Any) extends Node {
    var `type` = Syntax.MemberExpression
    var computed: Boolean = true
  }


  class ConditionalExpression(var test: Any, var consequent: Any, var alternate: Any) extends Node {
    var `type` = Syntax.ConditionalExpression
  }


  class ContinueStatement(var label: Any) extends Node {
    var `type` = Syntax.ContinueStatement
  }


  class DebuggerStatement() extends Node {
    var `type` = Syntax.DebuggerStatement
  }


  class Directive(var expression: Any, var directive: String) extends Node {
    var `type` = Syntax.ExpressionStatement
  }


  class DoWhileStatement(var body: Any, var test: Any) extends Node {
    var `type` = Syntax.DoWhileStatement
  }


  class EmptyStatement() extends Node {
    var `type` = Syntax.EmptyStatement
  }


  class ExportAllDeclaration(var source: Any) extends Node {
    var `type` = Syntax.ExportAllDeclaration
  }


  class ExportDefaultDeclaration(var declaration: Any) extends Node {
    var `type` = Syntax.ExportDefaultDeclaration
  }


  class ExportNamedDeclaration(var declaration: Any, var specifiers: Any, var source: Any) extends Node {
    var `type` = Syntax.ExportNamedDeclaration
  }


  class ExportSpecifier(var local: Any, var exported: Any) extends Node {
    var `type` = Syntax.ExportSpecifier
  }


  class ExpressionStatement(var expression: Any) extends Node {
    var `type` = Syntax.ExpressionStatement
  }


  class ForInStatement(var left: Any, var right: Any, var body: Any) extends Node {
    var `type` = Syntax.ForInStatement
    var each: Boolean = false
  }


  class ForOfStatement(var left: Any, var right: Any, var body: Any) extends Node {
    var `type` = Syntax.ForOfStatement
  }


  class ForStatement(var init: Any, var test: Any, var update: Any, var body: Any) extends Node {
    var `type` = Syntax.ForStatement
  }


  class FunctionDeclaration(var id: Any, var params: Any, var body: Any, var generator: Any) extends Node {
    var `type` = Syntax.FunctionDeclaration
    var expression: Boolean = false
    var async: Boolean = false
  }


  class FunctionExpression(var id: Any, var params: Any, var body: Any, var generator: Any) extends Node {
    var `type` = Syntax.FunctionExpression
    var expression: Boolean = false
    var async: Boolean = false
  }


  class Identifier(var name: String) extends Node {
    var `type` = Syntax.Identifier
  }


  class IfStatement(var test: Any, var consequent: Any, var alternate: Any) extends Node {
    var `type` = Syntax.IfStatement
  }


  class Import() extends Node {
    var `type` = Syntax.Import
  }


  class ImportDeclaration(var specifiers: Any, var source: Any) extends Node {
    var `type` = Syntax.ImportDeclaration
  }


  class ImportDefaultSpecifier(var local: Any) extends Node {
    var `type` = Syntax.ImportDefaultSpecifier
  }


  class ImportNamespaceSpecifier(var local: Any) extends Node {
    var `type` = Syntax.ImportNamespaceSpecifier
  }


  class ImportSpecifier(var local: Any, var imported: Any) extends Node {
    var `type` = Syntax.ImportSpecifier
  }


  class LabeledStatement(var label: Any, var body: Any) extends Node {
    var `type` = Syntax.LabeledStatement
  }


  class Literal(var value: OrType, var raw: Any) extends Node {
    var `type` = Syntax.Literal
  }


  class MetaProperty(var meta: Any, var property: Any) extends Node {
    var `type` = Syntax.MetaProperty
  }


  class MethodDefinition(var key: Any, var computed: Any, var value: Any, var kind: Any, var static: Any) extends Node {
    var `type` = Syntax.MethodDefinition
  }

  sealed abstract class Program(var body: Seq[Node]) extends Node {
    var `type` = Syntax.Program

    def sourceType: String

    var comments: ArrayBuffer[CommentHandler.Comment] = _
    var tokens: ArrayBuffer[Parser.TokenEntry] = _
    var errors: ArrayBuffer[ErrorHandler.Error] = _

  }

  class Module(body: Seq[Node]) extends Program(body) {
    def sourceType: String = "module"
  }

  class NewExpression(var callee: Any, var arguments: Any) extends Node {
    var `type` = Syntax.NewExpression
  }


  class ObjectExpression(var properties: Any) extends Node {
    var `type` = Syntax.ObjectExpression
  }


  class ObjectPattern(var properties: Array[Node]) extends Node {
    var `type` = Syntax.ObjectPattern
  }


  class Property(var kind: Any, var key: Any, var computed: Any, var value: Any, var method: Any, var shorthand: Any) extends Node {
    var `type` = Syntax.Property
  }


  class RegexLiteral(var value: Any, var raw: Any, pattern: String, flags: String) extends Node {
    var `type` = Syntax.Literal
    var regex = new RegExp(
      pattern = pattern,
      flags = flags
    )
  }


  class RestElement(var argument: Any) extends Node {
    var `type` = Syntax.RestElement
  }


  class ReturnStatement(var argument: Any) extends Node {
    var `type` = Syntax.ReturnStatement
  }


  class Script(body: Seq[Node]) extends Program(body) {
    def sourceType: String = "script"
  }

  class SequenceExpression(var expressions: Array[Node]) extends Node {
    var `type` = Syntax.SequenceExpression
  }


  class SpreadElement(var argument: Node) extends Node {
    var `type` = Syntax.SpreadElement
  }


  class StaticMemberExpression(var `object`: Any, var property: Any) extends Node {
    var `type` = Syntax.MemberExpression
    var computed: Boolean = false
  }


  class Super() extends Node {
    var `type` = Syntax.Super
  }


  class SwitchCase(var test: Node, var consequent: Array[Node]) extends Node {
    var `type` = Syntax.SwitchCase
  }


  class SwitchStatement(var discriminant: Any, var cases: Any) extends Node {
    var `type` = Syntax.SwitchStatement
  }


  class TaggedTemplateExpression(var tag: Any, var quasi: Any) extends Node {
    var `type` = Syntax.TaggedTemplateExpression
  }


  trait TemplateElementValue {
    def cooked: String

    def raw: String
  }

  class TemplateElement(var value: TemplateElementValue, var tail: Boolean) extends Node {
    var `type`: String = Syntax.TemplateElement
  }


  class TemplateLiteral(var quasis: Any, var expressions: Any) extends Node {
    var `type` = Syntax.TemplateLiteral
  }


  class ThisExpression() extends Node {
    var `type` = Syntax.ThisExpression
  }


  class ThrowStatement(var argument: Any) extends Node {
    var `type` = Syntax.ThrowStatement
  }


  class TryStatement(var block: Any, var handler: Any, var finalizer: Any) extends Node {
    var `type` = Syntax.TryStatement
  }


  class UnaryExpression(var operator: Any, var argument: Any) extends Node {
    var `type` = Syntax.UnaryExpression
    var prefix: Boolean = true
  }


  class UpdateExpression(var operator: Any, var argument: Any, var prefix: Any) extends Node {
    var `type` = Syntax.UpdateExpression
  }


  class VariableDeclaration(var declarations: Seq[VariableDeclarator], var kind: Any) extends Node {
    var `type` = Syntax.VariableDeclaration
  }


  class VariableDeclarator(var id: Any, var init: Node) extends Node {
    var `type` = Syntax.VariableDeclarator
  }


  class WhileStatement(var test: Any, var body: Any) extends Node {
    var `type` = Syntax.WhileStatement
  }


  class WithStatement(var `object`: Any, var body: Any) extends Node {
    var `type` = Syntax.WithStatement
  }


  class YieldExpression(var argument: Any, var delegate: Any) extends Node {
    var `type` = Syntax.YieldExpression
  }


}
/*
ScalaFromJS: 2017-12-06 21:28:23.723
nodes.js
*/

package esprima

import esprima.port.RegExp

import scala.collection.mutable.ArrayBuffer

object Node {

  trait Node {
    override def toString = `type`

    var `type`: String
    var range: (Int, Int) = _
    var loc: Scanner.SourceLocation = _

    var leadingComments: ArrayBuffer[CommentHandler.Comment] = _
    var innerComments: ArrayBuffer[CommentHandler.Comment] = _
    var trailingComments: ArrayBuffer[CommentHandler.Comment] = _
  }

  trait HasGenerator {
    def generator: Boolean
  }

  trait IsScope

  abstract class CommentNode extends Node {
    var value: String = _
  }

  class ArrayExpression(var elements: Array[Node]) extends Node {
    var `type` = Syntax.ArrayExpression
  }


  class ArrayPattern(var elements: Array[Node]) extends Node {
    var `type` = Syntax.ArrayPattern
  }


  class ArrowFunctionExpression(var params: Array[Node], var body: Node, var expression: Boolean) extends Node with HasGenerator {
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


  class AsyncArrowFunctionExpression(var params: Array[Node], var body: Node, var expression: Boolean) extends Node with HasGenerator {
    var `type` = Syntax.ArrowFunctionExpression
    var id = null
    var generator: Boolean = false
    var async: Boolean = true
  }

  trait AFunctionDeclaration extends Node with HasGenerator

  class AsyncFunctionDeclaration(var id: Node, var params: Array[Node], var body: Node) extends AFunctionDeclaration {
    var `type` = Syntax.FunctionDeclaration
    var generator: Boolean = false
    var expression: Boolean = false
    var async: Boolean = true
  }


  class AsyncFunctionExpression(var id: Node, var params: Array[Node], var body: Node) extends Node with HasGenerator {
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
    var left: Node = _
    var right: Node = _
    this.constructor(operator_par, left_par, right_par)

    def constructor(operator: String, left: Node, right: Node) = {
      val logical = operator == "||" || operator == "&&"
      this.`type` = if (logical) Syntax.LogicalExpression else Syntax.BinaryExpression
      this.operator = operator
      this.left = left
      this.right = right
    }

  }


  class BlockStatement(var body: Seq[Node]) extends Node with IsScope {
    var `type` = Syntax.BlockStatement
  }


  class BreakStatement(var label: Node) extends Node {
    var `type` = Syntax.BreakStatement
  }


  class CallExpression(var callee: Node, var arguments: Array[Node]) extends Node {
    var `type` = Syntax.CallExpression
  }


  class CatchClause(var param: Node, var body: Node) extends Node {
    var `type` = Syntax.CatchClause
  }


  class ClassBody(var body: Array[MethodDefinition]) extends Node  with IsScope {
    var `type` = Syntax.ClassBody
  }


  class ClassDeclaration(var id: Node, var superClass: Node, var body: Node) extends Node {
    var `type` = Syntax.ClassDeclaration
  }


  class ClassExpression(var id: Node, var superClass: Node, var body: Node) extends Node {
    var `type` = Syntax.ClassExpression
  }


  class ComputedMemberExpression(var `object`: Node, var property: Node) extends Node {
    var `type` = Syntax.MemberExpression
    var computed: Boolean = true
  }


  class ConditionalExpression(var test: Node, var consequent: Node, var alternate: Node) extends Node {
    var `type` = Syntax.ConditionalExpression
  }


  class ContinueStatement(var label: Node) extends Node {
    var `type` = Syntax.ContinueStatement
  }


  class DebuggerStatement() extends Node {
    var `type` = Syntax.DebuggerStatement
  }


  class Directive(var expression: Node, var directive: String) extends Node {
    var `type` = Syntax.ExpressionStatement
  }


  class DoWhileStatement(var body: Node, var test: Node) extends Node {
    var `type` = Syntax.DoWhileStatement
  }


  class EmptyStatement() extends Node {
    var `type` = Syntax.EmptyStatement
  }


  class ExportAllDeclaration(var source: Node) extends Node {
    var `type` = Syntax.ExportAllDeclaration
  }


  class ExportDefaultDeclaration(var declaration: Node) extends Node {
    var `type` = Syntax.ExportDefaultDeclaration
  }


  class ExportNamedDeclaration(var declaration: Node, var specifiers: Array[Node], var source: Node) extends Node {
    var `type` = Syntax.ExportNamedDeclaration
  }


  class ExportSpecifier(var local: Node, var exported: Node) extends Node {
    var `type` = Syntax.ExportSpecifier
  }


  class ExpressionStatement(var expression: Node) extends Node {
    var `type` = Syntax.ExpressionStatement
  }


  class ForInStatement(var left: Node, var right: Node, var body: Node) extends Node {
    var `type` = Syntax.ForInStatement
    var each: Boolean = false
  }


  class ForOfStatement(var left: Node, var right: Node, var body: Node) extends Node {
    var `type` = Syntax.ForOfStatement
  }


  class ForStatement(var init: Node, var test: Node, var update: Node, var body: Node) extends Node {
    var `type` = Syntax.ForStatement
  }


  class FunctionDeclaration(var id: Node, var params: Array[Node], var body: Node, var generator: Boolean) extends Node with AFunctionDeclaration {
    var `type` = Syntax.FunctionDeclaration
    var expression: Boolean = false
    var async: Boolean = false
  }


  class FunctionExpression(var id: Node, var params: Array[Node], var body: Node, var generator: Boolean) extends Node with HasGenerator {
    var `type` = Syntax.FunctionExpression
    var expression: Boolean = false
    var async: Boolean = false
  }


  class Identifier(var name: String) extends Node {
    var `type` = Syntax.Identifier
  }


  class IfStatement(var test: Node, var consequent: Node, var alternate: Node) extends Node {
    var `type` = Syntax.IfStatement
  }


  class Import() extends Node {
    var `type` = Syntax.Import
  }


  class ImportDeclaration(var specifiers: Array[Node], var source: Node) extends Node {
    var `type` = Syntax.ImportDeclaration
  }


  class ImportDefaultSpecifier(var local: Node) extends Node {
    var `type` = Syntax.ImportDefaultSpecifier
  }


  class ImportNamespaceSpecifier(var local: Node) extends Node {
    var `type` = Syntax.ImportNamespaceSpecifier
  }


  class ImportSpecifier(var local: Node, var imported: Node) extends Node {
    var `type` = Syntax.ImportSpecifier
  }


  class LabeledStatement(var label: Node, var body: Node) extends Node {
    var `type` = Syntax.LabeledStatement
  }


  class Literal(var value: OrType, var raw: String) extends Node {
    var `type` = Syntax.Literal
  }


  class MetaProperty(var meta: Node, var property: Node) extends Node {
    var `type` = Syntax.MetaProperty
  }


  class MethodDefinition(var key: Node, var computed: Boolean, var value: Node, var kind: Boolean, var static: Boolean) extends Node {
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

  class NewExpression(var callee: Node, var arguments: Array[Node]) extends Node {
    var `type` = Syntax.NewExpression
  }


  class ObjectExpression(var properties: Array[Node]) extends Node {
    var `type` = Syntax.ObjectExpression
  }


  class ObjectPattern(var properties: Array[Node]) extends Node {
    var `type` = Syntax.ObjectPattern
  }


  class Property(var kind: String, var key: Node, var computed: Boolean, var value: Node, var method: Boolean, var shorthand: Boolean) extends Node {
    var `type` = Syntax.Property
  }


  class RegexLiteral(var value: RegExp, var raw: String, pattern: String, flags: String) extends Node {
    var `type` = Syntax.Literal
    var regex = new RegExp(
      pattern = pattern,
      flags = flags
    )
  }


  class RestElement(var argument: Node) extends Node {
    var `type` = Syntax.RestElement
  }


  class ReturnStatement(var argument: Node) extends Node {
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


  class StaticMemberExpression(var `object`: Node, var property: Node) extends Node {
    var `type` = Syntax.MemberExpression
    var computed: Boolean = false
  }


  class Super() extends Node {
    var `type` = Syntax.Super
  }


  class SwitchCase(var test: Node, var consequent: Array[Node]) extends Node {
    var `type` = Syntax.SwitchCase
  }


  class SwitchStatement(var discriminant: Node, var cases: Array[SwitchCase]) extends Node {
    var `type` = Syntax.SwitchStatement
  }


  class TaggedTemplateExpression(var tag: Node, var quasi: Node) extends Node {
    var `type` = Syntax.TaggedTemplateExpression
  }


  trait TemplateElementValue {
    def cooked: String

    def raw: String
  }

  class TemplateElement(var value: TemplateElementValue, var tail: Boolean) extends Node {
    var `type`: String = Syntax.TemplateElement
  }


  class TemplateLiteral(var quasis: Array[Node], var expressions: Array[Node]) extends Node {
    var `type` = Syntax.TemplateLiteral
  }


  class ThisExpression() extends Node {
    var `type` = Syntax.ThisExpression
  }


  class ThrowStatement(var argument: Node) extends Node {
    var `type` = Syntax.ThrowStatement
  }


  class TryStatement(var block: Node, var handler: Node, var finalizer: Node) extends Node {
    var `type` = Syntax.TryStatement
  }


  class UnaryExpression(var operator: String, var argument: Node) extends Node {
    var `type` = Syntax.UnaryExpression
    var prefix: Boolean = true
  }


  class UpdateExpression(var operator: String, var argument: Node, var prefix: Boolean) extends Node {
    var `type` = Syntax.UpdateExpression
  }


  class VariableDeclaration(var declarations: Seq[VariableDeclarator], var kind: String) extends Node {
    var `type` = Syntax.VariableDeclaration
  }


  class VariableDeclarator(var id: Node, var init: Node) extends Node {
    var `type` = Syntax.VariableDeclarator
  }


  class WhileStatement(var test: Node, var body: Node) extends Node {
    var `type` = Syntax.WhileStatement
  }


  class WithStatement(var `object`: Node, var body: Node) extends Node {
    var `type` = Syntax.WithStatement
  }


  class YieldExpression(var argument: Node, var delegate: Boolean) extends Node {
    var `type` = Syntax.YieldExpression
  }


}
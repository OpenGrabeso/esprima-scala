/*
ScalaFromJS: 2017-12-05 14:51:46.266
nodes.js
*/

package esprima

object Node {

class ArrayExpression(var elements: Any) {
  var `type` = Syntax.ArrayExpression
}



class ArrayPattern(var elements: Any) {
  var `type` = Syntax.ArrayPattern
}



class ArrowFunctionExpression(var params: Any, var body: Any, var expression: Any) {
  var `type` = Syntax.ArrowFunctionExpression
  var id = null
  var generator: Boolean = false
  var async: Boolean = false
}



class AssignmentExpression(var operator: Any, var left: Any, var right: Any) {
  var `type` = Syntax.AssignmentExpression
}



class AssignmentPattern(var left: Any, var right: Any) {
  var `type` = Syntax.AssignmentPattern
}



class AsyncArrowFunctionExpression(var params: Any, var body: Any, var expression: Any) {
  var `type` = Syntax.ArrowFunctionExpression
  var id = null
  var generator: Boolean = false
  var async: Boolean = true
}



class AsyncFunctionDeclaration(var id: Any, var params: Any, var body: Any) {
  var `type` = Syntax.FunctionDeclaration
  var generator: Boolean = false
  var expression: Boolean = false
  var async: Boolean = true
}



class AsyncFunctionExpression(var id: Any, var params: Any, var body: Any) {
  var `type` = Syntax.FunctionExpression
  var generator: Boolean = false
  var expression: Boolean = false
  var async: Boolean = true
}



class AwaitExpression(var argument: Any) {
  var `type` = Syntax.AwaitExpression
}



class BinaryExpression(operator_par: String, left_par: Any, right_par: Any) {
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



class BlockStatement(var body: Any) {
  var `type` = Syntax.BlockStatement
}



class BreakStatement(var label: Any) {
  var `type` = Syntax.BreakStatement
}



class CallExpression(var callee: Any, var arguments: Any) {
  var `type` = Syntax.CallExpression
}



class CatchClause(var param: Any, var body: Any) {
  var `type` = Syntax.CatchClause
}



class ClassBody(var body: Any) {
  var `type` = Syntax.ClassBody
}



class ClassDeclaration(var id: Any, var superClass: Any, var body: Any) {
  var `type` = Syntax.ClassDeclaration
}



class ClassExpression(var id: Any, var superClass: Any, var body: Any) {
  var `type` = Syntax.ClassExpression
}



class ComputedMemberExpression(var `object`: Any, var property: Any) {
  var `type` = Syntax.MemberExpression
  var computed: Boolean = true
}



class ConditionalExpression(var test: Any, var consequent: Any, var alternate: Any) {
  var `type` = Syntax.ConditionalExpression
}



class ContinueStatement(var label: Any) {
  var `type` = Syntax.ContinueStatement
}



class DebuggerStatement() {
  var `type` = Syntax.DebuggerStatement
}



class Directive(var expression: Any, var directive: String) {
  var `type` = Syntax.ExpressionStatement
}



class DoWhileStatement(var body: Any, var test: Any) {
  var `type` = Syntax.DoWhileStatement
}



class EmptyStatement() {
  var `type` = Syntax.EmptyStatement
}



class ExportAllDeclaration(var source: Any) {
  var `type` = Syntax.ExportAllDeclaration
}



class ExportDefaultDeclaration(var declaration: Any) {
  var `type` = Syntax.ExportDefaultDeclaration
}



class ExportNamedDeclaration(var declaration: Any, var specifiers: Any, var source: Any) {
  var `type` = Syntax.ExportNamedDeclaration
}



class ExportSpecifier(var local: Any, var exported: Any) {
  var `type` = Syntax.ExportSpecifier
}



class ExpressionStatement(var expression: Any) {
  var `type` = Syntax.ExpressionStatement
}



class ForInStatement(var left: Any, var right: Any, var body: Any) {
  var `type` = Syntax.ForInStatement
  var each: Boolean = false
}



class ForOfStatement(var left: Any, var right: Any, var body: Any) {
  var `type` = Syntax.ForOfStatement
}



class ForStatement(var init: Any, var test: Any, var update: Any, var body: Any) {
  var `type` = Syntax.ForStatement
}



class FunctionDeclaration(var id: Any, var params: Any, var body: Any, var generator: Any) {
  var `type` = Syntax.FunctionDeclaration
  var expression: Boolean = false
  var async: Boolean = false
}



class FunctionExpression(var id: Any, var params: Any, var body: Any, var generator: Any) {
  var `type` = Syntax.FunctionExpression
  var expression: Boolean = false
  var async: Boolean = false
}



class Identifier(var name: String) {
  var `type` = Syntax.Identifier
}



class IfStatement(var test: Any, var consequent: Any, var alternate: Any) {
  var `type` = Syntax.IfStatement
}



class Import() {
  var `type` = Syntax.Import
}



class ImportDeclaration(var specifiers: Any, var source: Any) {
  var `type` = Syntax.ImportDeclaration
}



class ImportDefaultSpecifier(var local: Any) {
  var `type` = Syntax.ImportDefaultSpecifier
}



class ImportNamespaceSpecifier(var local: Any) {
  var `type` = Syntax.ImportNamespaceSpecifier
}



class ImportSpecifier(var local: Any, var imported: Any) {
  var `type` = Syntax.ImportSpecifier
}



class LabeledStatement(var label: Any, var body: Any) {
  var `type` = Syntax.LabeledStatement
}



class Literal(var value: Any, var raw: Any) {
  var `type` = Syntax.Literal
}



class MetaProperty(var meta: Any, var property: Any) {
  var `type` = Syntax.MetaProperty
}



class MethodDefinition(var key: Any, var computed: Any, var value: Any, var kind: Any, var static: Any) {
  var `type` = Syntax.MethodDefinition
}



class Module(var body: Any) {
  var `type` = Syntax.Program
  var sourceType: String = "module"
}



class NewExpression(var callee: Any, var arguments: Any) {
  var `type` = Syntax.NewExpression
}



class ObjectExpression(var properties: Any) {
  var `type` = Syntax.ObjectExpression
}



class ObjectPattern(var properties: Any) {
  var `type` = Syntax.ObjectPattern
}



class Property(var kind: Any, var key: Any, var computed: Any, var value: Any, var method: Any, var shorthand: Any) {
  var `type` = Syntax.Property
}



class RegexLiteral(var value: Any, var raw: Any, pattern: Any, flags: Any) {
  var `type` = Syntax.Literal
  var regex = new {
    var pattern = pattern
    var flags = flags
  }
}



class RestElement(var argument: Any) {
  var `type` = Syntax.RestElement
}



class ReturnStatement(var argument: Any) {
  var `type` = Syntax.ReturnStatement
}



class Script(var body: Any) {
  var `type` = Syntax.Program
  var sourceType: String = "script"
}



class SequenceExpression(var expressions: Any) {
  var `type` = Syntax.SequenceExpression
}



class SpreadElement(var argument: Any) {
  var `type` = Syntax.SpreadElement
}



class StaticMemberExpression(var `object`: Any, var property: Any) {
  var `type` = Syntax.MemberExpression
  var computed: Boolean = false
}



class Super() {
  var `type` = Syntax.Super
}



class SwitchCase(var test: Any, var consequent: Any) {
  var `type` = Syntax.SwitchCase
}



class SwitchStatement(var discriminant: Any, var cases: Any) {
  var `type` = Syntax.SwitchStatement
}



class TaggedTemplateExpression(var tag: Any, var quasi: Any) {
  var `type` = Syntax.TaggedTemplateExpression
}



class TemplateElement(var value: String, var tail: Any) {
  var `type`: Double = Syntax.TemplateElement
}



class TemplateLiteral(var quasis: Any, var expressions: Any) {
  var `type` = Syntax.TemplateLiteral
}



class ThisExpression() {
  var `type` = Syntax.ThisExpression
}



class ThrowStatement(var argument: Any) {
  var `type` = Syntax.ThrowStatement
}



class TryStatement(var block: Any, var handler: Any, var finalizer: Any) {
  var `type` = Syntax.TryStatement
}



class UnaryExpression(var operator: Any, var argument: Any) {
  var `type` = Syntax.UnaryExpression
  var prefix: Boolean = true
}



class UpdateExpression(var operator: Any, var argument: Any, var prefix: Any) {
  var `type` = Syntax.UpdateExpression
}



class VariableDeclaration(var declarations: Any, var kind: Any) {
  var `type` = Syntax.VariableDeclaration
}



class VariableDeclarator(var id: Any, var init: Any) {
  var `type` = Syntax.VariableDeclarator
}



class WhileStatement(var test: Any, var body: Any) {
  var `type` = Syntax.WhileStatement
}



class WithStatement(var `object`: Any, var body: Any) {
  var `type` = Syntax.WithStatement
}



class YieldExpression(var argument: Any, var delegate: Any) {
  var `type` = Syntax.YieldExpression
}


}
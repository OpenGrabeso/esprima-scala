/*
ScalaFromJS: 2017-12-06 21:28:23.723
nodes.js
*/

package esprima
"use strict"
Object.defineProperty(exports, "__esModule", new {
  var value = true
})
val syntax_1 = require("./syntax")
/*tslint:disable:max-classes-per-file */

class ArrayExpression(var elements: Any) {
  var `type` = syntax_1.Syntax.ArrayExpression
}

exports.ArrayExpression = ArrayExpression

class ArrayPattern(var elements: Any) {
  var `type` = syntax_1.Syntax.ArrayPattern
}

exports.ArrayPattern = ArrayPattern

class ArrowFunctionExpression(var params: Any, var body: Any, var expression: Any) {
  var `type` = syntax_1.Syntax.ArrowFunctionExpression
  var id = null
  var generator: Boolean = false
  var async: Boolean = false
}

exports.ArrowFunctionExpression = ArrowFunctionExpression

class AssignmentExpression(var operator: Any, var left: Any, var right: Any) {
  var `type` = syntax_1.Syntax.AssignmentExpression
}

exports.AssignmentExpression = AssignmentExpression

class AssignmentPattern(var left: Any, var right: Any) {
  var `type` = syntax_1.Syntax.AssignmentPattern
}

exports.AssignmentPattern = AssignmentPattern

class AsyncArrowFunctionExpression(var params: Any, var body: Any, var expression: Any) {
  var `type` = syntax_1.Syntax.ArrowFunctionExpression
  var id = null
  var generator: Boolean = false
  var async: Boolean = true
}

exports.AsyncArrowFunctionExpression = AsyncArrowFunctionExpression

class AsyncFunctionDeclaration(var id: Any, var params: Any, var body: Any) {
  var `type` = syntax_1.Syntax.FunctionDeclaration
  var generator: Boolean = false
  var expression: Boolean = false
  var async: Boolean = true
}

exports.AsyncFunctionDeclaration = AsyncFunctionDeclaration

class AsyncFunctionExpression(var id: Any, var params: Any, var body: Any) {
  var `type` = syntax_1.Syntax.FunctionExpression
  var generator: Boolean = false
  var expression: Boolean = false
  var async: Boolean = true
}

exports.AsyncFunctionExpression = AsyncFunctionExpression

class AwaitExpression(var argument: Any) {
  var `type` = syntax_1.Syntax.AwaitExpression
}

exports.AwaitExpression = AwaitExpression

class BinaryExpression(operator_par: String, left_par: Any, right_par: Any) {
  var `type` = _
  var operator: String = _
  var left = _
  var right = _
  this.constructor(operator_par, left_par, right_par)

  def constructor(operator: String, left: Any, right: Any) = {
    val logical = operator == "||" || operator == "&&"
    this.`type` = if (logical) syntax_1.Syntax.LogicalExpression else syntax_1.Syntax.BinaryExpression
    this.operator = operator
    this.left = left
    this.right = right
  }
  
}

exports.BinaryExpression = BinaryExpression

class BlockStatement(var body: Any) {
  var `type` = syntax_1.Syntax.BlockStatement
}

exports.BlockStatement = BlockStatement

class BreakStatement(var label: Any) {
  var `type` = syntax_1.Syntax.BreakStatement
}

exports.BreakStatement = BreakStatement

class CallExpression(var callee: Any, var arguments: Any) {
  var `type` = syntax_1.Syntax.CallExpression
}

exports.CallExpression = CallExpression

class CatchClause(var param: Any, var body: Any) {
  var `type` = syntax_1.Syntax.CatchClause
}

exports.CatchClause = CatchClause

class ClassBody(var body: Any) {
  var `type` = syntax_1.Syntax.ClassBody
}

exports.ClassBody = ClassBody

class ClassDeclaration(var id: Any, var superClass: Any, var body: Any) {
  var `type` = syntax_1.Syntax.ClassDeclaration
}

exports.ClassDeclaration = ClassDeclaration

class ClassExpression(var id: Any, var superClass: Any, var body: Any) {
  var `type` = syntax_1.Syntax.ClassExpression
}

exports.ClassExpression = ClassExpression

class ComputedMemberExpression(var `object`: Any, var property: Any) {
  var `type` = syntax_1.Syntax.MemberExpression
  var computed: Boolean = true
}

exports.ComputedMemberExpression = ComputedMemberExpression

class ConditionalExpression(var test: Any, var consequent: Any, var alternate: Any) {
  var `type` = syntax_1.Syntax.ConditionalExpression
}

exports.ConditionalExpression = ConditionalExpression

class ContinueStatement(var label: Any) {
  var `type` = syntax_1.Syntax.ContinueStatement
}

exports.ContinueStatement = ContinueStatement

class DebuggerStatement() {
  var `type` = syntax_1.Syntax.DebuggerStatement
}

exports.DebuggerStatement = DebuggerStatement

class Directive(var expression: Any, var directive: String) {
  var `type` = syntax_1.Syntax.ExpressionStatement
}

exports.Directive = Directive

class DoWhileStatement(var body: Any, var test: Any) {
  var `type` = syntax_1.Syntax.DoWhileStatement
}

exports.DoWhileStatement = DoWhileStatement

class EmptyStatement() {
  var `type` = syntax_1.Syntax.EmptyStatement
}

exports.EmptyStatement = EmptyStatement

class ExportAllDeclaration(var source: Any) {
  var `type` = syntax_1.Syntax.ExportAllDeclaration
}

exports.ExportAllDeclaration = ExportAllDeclaration

class ExportDefaultDeclaration(var declaration: Any) {
  var `type` = syntax_1.Syntax.ExportDefaultDeclaration
}

exports.ExportDefaultDeclaration = ExportDefaultDeclaration

class ExportNamedDeclaration(var declaration: Any, var specifiers: Any, var source: Any) {
  var `type` = syntax_1.Syntax.ExportNamedDeclaration
}

exports.ExportNamedDeclaration = ExportNamedDeclaration

class ExportSpecifier(var local: Any, var exported: Any) {
  var `type` = syntax_1.Syntax.ExportSpecifier
}

exports.ExportSpecifier = ExportSpecifier

class ExpressionStatement(var expression: Any) {
  var `type` = syntax_1.Syntax.ExpressionStatement
}

exports.ExpressionStatement = ExpressionStatement

class ForInStatement(var left: Any, var right: Any, var body: Any) {
  var `type` = syntax_1.Syntax.ForInStatement
  var each: Boolean = false
}

exports.ForInStatement = ForInStatement

class ForOfStatement(var left: Any, var right: Any, var body: Any) {
  var `type` = syntax_1.Syntax.ForOfStatement
}

exports.ForOfStatement = ForOfStatement

class ForStatement(var init: Any, var test: Any, var update: Any, var body: Any) {
  var `type` = syntax_1.Syntax.ForStatement
}

exports.ForStatement = ForStatement

class FunctionDeclaration(var id: Any, var params: Any, var body: Any, var generator: Any) {
  var `type` = syntax_1.Syntax.FunctionDeclaration
  var expression: Boolean = false
  var async: Boolean = false
}

exports.FunctionDeclaration = FunctionDeclaration

class FunctionExpression(var id: Any, var params: Any, var body: Any, var generator: Any) {
  var `type` = syntax_1.Syntax.FunctionExpression
  var expression: Boolean = false
  var async: Boolean = false
}

exports.FunctionExpression = FunctionExpression

class Identifier(var name: String) {
  var `type` = syntax_1.Syntax.Identifier
}

exports.Identifier = Identifier

class IfStatement(var test: Any, var consequent: Any, var alternate: Any) {
  var `type` = syntax_1.Syntax.IfStatement
}

exports.IfStatement = IfStatement

class Import() {
  var `type` = syntax_1.Syntax.Import
}

exports.Import = Import

class ImportDeclaration(var specifiers: Any, var source: Any) {
  var `type` = syntax_1.Syntax.ImportDeclaration
}

exports.ImportDeclaration = ImportDeclaration

class ImportDefaultSpecifier(var local: Any) {
  var `type` = syntax_1.Syntax.ImportDefaultSpecifier
}

exports.ImportDefaultSpecifier = ImportDefaultSpecifier

class ImportNamespaceSpecifier(var local: Any) {
  var `type` = syntax_1.Syntax.ImportNamespaceSpecifier
}

exports.ImportNamespaceSpecifier = ImportNamespaceSpecifier

class ImportSpecifier(var local: Any, var imported: Any) {
  var `type` = syntax_1.Syntax.ImportSpecifier
}

exports.ImportSpecifier = ImportSpecifier

class LabeledStatement(var label: Any, var body: Any) {
  var `type` = syntax_1.Syntax.LabeledStatement
}

exports.LabeledStatement = LabeledStatement

class Literal(var value: String, var raw: Any) {
  var `type` = syntax_1.Syntax.Literal
}

exports.Literal = Literal

class MetaProperty(var meta: Any, var property: Any) {
  var `type` = syntax_1.Syntax.MetaProperty
}

exports.MetaProperty = MetaProperty

class MethodDefinition(var key: Any, var computed: Any, var value: Any, var kind: Any, var static: Any) {
  var `type` = syntax_1.Syntax.MethodDefinition
}

exports.MethodDefinition = MethodDefinition

class Module(var body: Any) {
  var `type` = syntax_1.Syntax.Program
  var sourceType: String = "module"
}

exports.Module = Module

class NewExpression(var callee: Any, var arguments: Any) {
  var `type` = syntax_1.Syntax.NewExpression
}

exports.NewExpression = NewExpression

class ObjectExpression(var properties: Any) {
  var `type` = syntax_1.Syntax.ObjectExpression
}

exports.ObjectExpression = ObjectExpression

class ObjectPattern(var properties: Any) {
  var `type` = syntax_1.Syntax.ObjectPattern
}

exports.ObjectPattern = ObjectPattern

class Property(var kind: Any, var key: Any, var computed: Any, var value: Any, var method: Any, var shorthand: Any) {
  var `type` = syntax_1.Syntax.Property
}

exports.Property = Property

class RegexLiteral(var value: Any, var raw: Any, pattern: Any, flags: Any) {
  var `type` = syntax_1.Syntax.Literal
  var regex = new {
    var pattern = pattern
    var flags = flags
  }
}

exports.RegexLiteral = RegexLiteral

class RestElement(var argument: Any) {
  var `type` = syntax_1.Syntax.RestElement
}

exports.RestElement = RestElement

class ReturnStatement(var argument: Any) {
  var `type` = syntax_1.Syntax.ReturnStatement
}

exports.ReturnStatement = ReturnStatement

class Script(var body: Any) {
  var `type` = syntax_1.Syntax.Program
  var sourceType: String = "script"
}

exports.Script = Script

class SequenceExpression(var expressions: Any) {
  var `type` = syntax_1.Syntax.SequenceExpression
}

exports.SequenceExpression = SequenceExpression

class SpreadElement(var argument: Any) {
  var `type` = syntax_1.Syntax.SpreadElement
}

exports.SpreadElement = SpreadElement

class StaticMemberExpression(var `object`: Any, var property: Any) {
  var `type` = syntax_1.Syntax.MemberExpression
  var computed: Boolean = false
}

exports.StaticMemberExpression = StaticMemberExpression

class Super() {
  var `type` = syntax_1.Syntax.Super
}

exports.Super = Super

class SwitchCase(var test: Any, var consequent: Any) {
  var `type` = syntax_1.Syntax.SwitchCase
}

exports.SwitchCase = SwitchCase

class SwitchStatement(var discriminant: Any, var cases: Any) {
  var `type` = syntax_1.Syntax.SwitchStatement
}

exports.SwitchStatement = SwitchStatement

class TaggedTemplateExpression(var tag: Any, var quasi: Any) {
  var `type` = syntax_1.Syntax.TaggedTemplateExpression
}

exports.TaggedTemplateExpression = TaggedTemplateExpression

class TemplateElement(var value: Any, var tail: Any) {
  var `type` = syntax_1.Syntax.TemplateElement
}

exports.TemplateElement = TemplateElement

class TemplateLiteral(var quasis: Any, var expressions: Any) {
  var `type` = syntax_1.Syntax.TemplateLiteral
}

exports.TemplateLiteral = TemplateLiteral

class ThisExpression() {
  var `type` = syntax_1.Syntax.ThisExpression
}

exports.ThisExpression = ThisExpression

class ThrowStatement(var argument: Any) {
  var `type` = syntax_1.Syntax.ThrowStatement
}

exports.ThrowStatement = ThrowStatement

class TryStatement(var block: Any, var handler: Any, var finalizer: Any) {
  var `type` = syntax_1.Syntax.TryStatement
}

exports.TryStatement = TryStatement

class UnaryExpression(var operator: Any, var argument: Any) {
  var `type` = syntax_1.Syntax.UnaryExpression
  var prefix: Boolean = true
}

exports.UnaryExpression = UnaryExpression

class UpdateExpression(var operator: Any, var argument: Any, var prefix: Any) {
  var `type` = syntax_1.Syntax.UpdateExpression
}

exports.UpdateExpression = UpdateExpression

class VariableDeclaration(var declarations: Any, var kind: Any) {
  var `type` = syntax_1.Syntax.VariableDeclaration
}

exports.VariableDeclaration = VariableDeclaration

class VariableDeclarator(var id: Any, var init: Any) {
  var `type` = syntax_1.Syntax.VariableDeclarator
}

exports.VariableDeclarator = VariableDeclarator

class WhileStatement(var test: Any, var body: Any) {
  var `type` = syntax_1.Syntax.WhileStatement
}

exports.WhileStatement = WhileStatement

class WithStatement(var `object`: Any, var body: Any) {
  var `type` = syntax_1.Syntax.WithStatement
}

exports.WithStatement = WithStatement

class YieldExpression(var argument: Any, var delegate: Any) {
  var `type` = syntax_1.Syntax.YieldExpression
}

exports.YieldExpression = YieldExpression

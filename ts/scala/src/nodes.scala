/*
ScalaFromJS: Dev
nodes.ts
*/

package com.github.opengrabeso.esprima
/* import { Syntax } from './syntax' */
type ArgumentListElement = Expression | SpreadElement
type ArrayExpressionElement = Expression | SpreadElement
type ArrayPatternElement = AssignmentPattern | Identifier | ArrayPattern | ObjectPattern | RestElement
type BindingPattern = ArrayPattern | ObjectPattern
type BindingIdentifier = Identifier
type ChainElement = CallExpression | ComputedMemberExpression | StaticMemberExpression
type Declaration = Any
type ExportableDefaultDeclaration = Any
type ExportableNamedDeclaration = AsyncFunctionDeclaration | ClassDeclaration | FunctionDeclaration | VariableDeclaration
type ExportDeclaration = ExportAllDeclaration | ExportDefaultDeclaration | ExportNamedDeclaration
type Expression = Any
type FunctionParameter = AssignmentPattern | Identifier | ArrayPattern | ObjectPattern
type ImportDeclarationSpecifier = ImportDefaultSpecifier | ImportNamespaceSpecifier | ImportSpecifier
type ObjectExpressionProperty = Property | SpreadElement
type ObjectPatternProperty = Property | RestElement
type Statement = Any
type PropertyKey = Identifier | Literal
type PropertyValue = Any
type StatementListItem = Declaration | Statement
class ArrayExpression(var elements: Array[Expression | SpreadElement]) {
  var `type`: String = Syntax.ArrayExpression
}

class ArrayPattern(var elements: Array[AssignmentPattern | BindingIdentifier | BindingPattern | RestElement]) {
  var `type`: String = Syntax.ArrayPattern
}

class ArrowFunctionExpression(var params: Array[AssignmentPattern | Identifier | ArrayPattern | ObjectPattern], var body: BlockStatement | Expression, var expression: Boolean) {
  var `type`: String = Syntax.ArrowFunctionExpression
  var id: Identifier = null
  var generator: Boolean = false
  var async: Boolean = false
}

class AssignmentExpression(var operator: String, var left: Expression, var right: Expression) {
  var `type`: String = Syntax.AssignmentExpression
}

class AssignmentPattern(var left: Identifier | ArrayPattern | ObjectPattern, var right: Expression) {
  var `type`: String = Syntax.AssignmentPattern
}

class AsyncArrowFunctionExpression(var params: Array[AssignmentPattern | Identifier | ArrayPattern | ObjectPattern], var body: BlockStatement | Expression, var expression: Boolean) {
  var `type`: String = Syntax.ArrowFunctionExpression
  var id: Identifier = null
  var generator: Boolean = false
  var async: Boolean = true
}

class AsyncFunctionDeclaration(var id: Identifier, var params: Array[AssignmentPattern | Identifier | ArrayPattern | ObjectPattern], var body: BlockStatement, var generator: Boolean) {
  var `type`: String = Syntax.FunctionDeclaration
  var expression: Boolean = false
  var async: Boolean = true
}

class AsyncFunctionExpression(var id: Identifier, var params: Array[AssignmentPattern | Identifier | ArrayPattern | ObjectPattern], var body: BlockStatement, var generator: Boolean) {
  var `type`: String = Syntax.FunctionExpression
  var expression: Boolean = false
  var async: Boolean = true
}

class AwaitExpression(var argument: Expression) {
  var `type`: String = Syntax.AwaitExpression
}

class BinaryExpression(operator_par: String, left_par: Expression, right_par: Expression) {
  var `type`: String = _
  var operator: String = _
  var left: Expression = _
  var right: Expression = _
  this.constructor(operator_par, left_par, right_par)

  def constructor(operator: String, left: Expression, right: Expression) = {
    val logical = operator == "||" || operator == "&&" || operator == "??"
    this.`type` = if (logical) Syntax.LogicalExpression else Syntax.BinaryExpression
    this.operator = operator
    this.left = left
    this.right = right
  }
  
}

class BlockStatement(var body: Array[Statement]) {
  var `type`: String = Syntax.BlockStatement
}

class BreakStatement(var label: Identifier) {
  var `type`: String = Syntax.BreakStatement
}

class CallExpression(var callee: Expression | Import, var arguments: Array[Expression | SpreadElement], var optional: Boolean) {
  var `type`: String = Syntax.CallExpression
}

class CatchClause(var param: Identifier | ArrayPattern | ObjectPattern, var body: BlockStatement) {
  var `type`: String = Syntax.CatchClause
}

class ChainExpression(var expression: CallExpression | ComputedMemberExpression | StaticMemberExpression) {
  var `type`: String = Syntax.ChainExpression
}

class ClassBody(var body: Array[Property]) {
  var `type`: String = Syntax.ClassBody
}

class ClassDeclaration(var id: Identifier, var superClass: Identifier, var body: ClassBody) {
  var `type`: String = Syntax.ClassDeclaration
}

class ClassExpression(var id: Identifier, var superClass: Identifier, var body: ClassBody) {
  var `type`: String = Syntax.ClassExpression
}

class ComputedMemberExpression(var `object`: Expression, var property: Expression, var optional: Boolean) {
  var `type`: String = Syntax.MemberExpression
  var computed: Boolean = true
}

class ConditionalExpression(var test: Expression, var consequent: Expression, var alternate: Expression) {
  var `type`: String = Syntax.ConditionalExpression
}

class ContinueStatement(var label: Identifier) {
  var `type`: String = Syntax.ContinueStatement
}

class DebuggerStatement {
  var `type`: String = Syntax.DebuggerStatement
}

class Directive(var expression: Expression, var directive: String) {
  var `type`: String = Syntax.ExpressionStatement
}

class DoWhileStatement(var body: Statement, var test: Expression) {
  var `type`: String = Syntax.DoWhileStatement
}

class EmptyStatement {
  var `type`: String = Syntax.EmptyStatement
}

class ExportAllDeclaration(var source: Literal) {
  var `type`: String = Syntax.ExportAllDeclaration
}

class ExportDefaultDeclaration(var declaration: ExportableDefaultDeclaration) {
  var `type`: String = Syntax.ExportDefaultDeclaration
}

class ExportNamedDeclaration(var declaration: AsyncFunctionDeclaration | ClassDeclaration | FunctionDeclaration | VariableDeclaration, var specifiers: Array[ExportSpecifier], var source: Literal) {
  var `type`: String = Syntax.ExportNamedDeclaration
}

class ExportSpecifier(var local: Identifier, var exported: Identifier) {
  var `type`: String = Syntax.ExportSpecifier
}

class ExpressionStatement(var expression: Expression) {
  var `type`: String = Syntax.ExpressionStatement
}

class ForInStatement(var left: Expression, var right: Expression, var body: Statement) {
  var `type`: String = Syntax.ForInStatement
  var each: Boolean = false
}

class ForOfStatement(var left: Expression, var right: Expression, var body: Statement, var await: Boolean) {
  var `type`: String = Syntax.ForOfStatement
}

class ForStatement(var init: Expression, var test: Expression, var update: Expression, var body: Statement) {
  var `type`: String = Syntax.ForStatement
}

class FunctionDeclaration(var id: Identifier, var params: Array[AssignmentPattern | Identifier | ArrayPattern | ObjectPattern], var body: BlockStatement, var generator: Boolean) {
  var `type`: String = Syntax.FunctionDeclaration
  var expression: Boolean = false
  var async: Boolean = false
}

class FunctionExpression(var id: Identifier, var params: Array[AssignmentPattern | Identifier | ArrayPattern | ObjectPattern], var body: BlockStatement, var generator: Boolean) {
  var `type`: String = Syntax.FunctionExpression
  var expression: Boolean = false
  var async: Boolean = false
}

class Identifier(var name: String) {
  var `type`: String = Syntax.Identifier
}

class IfStatement(var test: Expression, var consequent: Statement, var alternate: Statement) {
  var `type`: String = Syntax.IfStatement
}

class Import {
  var `type`: String = Syntax.Import
}

class ImportDeclaration(var specifiers: Array[ImportDefaultSpecifier | ImportNamespaceSpecifier | ImportSpecifier], var source: Literal) {
  var `type`: String = Syntax.ImportDeclaration
}

class ImportDefaultSpecifier(var local: Identifier) {
  var `type`: String = Syntax.ImportDefaultSpecifier
}

class ImportNamespaceSpecifier(var local: Identifier) {
  var `type`: String = Syntax.ImportNamespaceSpecifier
}

class ImportSpecifier(var local: Identifier, var imported: Identifier) {
  var `type`: String = Syntax.ImportSpecifier
}

class LabeledStatement(var label: Identifier, var body: Statement) {
  var `type`: String = Syntax.LabeledStatement
}

class Literal(var value: Boolean | Double | String, var raw: String) {
  var `type`: String = Syntax.Literal
}

class MetaProperty(var meta: Identifier, var property: Identifier) {
  var `type`: String = Syntax.MetaProperty
}

class MethodDefinition(var key: Expression, var computed: Boolean, var value: AsyncFunctionExpression | FunctionExpression, var kind: String, var static: Boolean) {
  var `type`: String = Syntax.MethodDefinition
}

class Module(var body: Array[Declaration | Statement]) {
  var `type`: String = Syntax.Program
  var sourceType: String = "module"
}

class NewExpression(var callee: Expression, var arguments: Array[Expression | SpreadElement]) {
  var `type`: String = Syntax.NewExpression
}

class ObjectExpression(var properties: Array[Property | SpreadElement]) {
  var `type`: String = Syntax.ObjectExpression
}

class ObjectPattern(var properties: Array[Property | RestElement]) {
  var `type`: String = Syntax.ObjectPattern
}

class Property(var kind: String, var key: Identifier | Literal, var computed: Boolean, var value: PropertyValue, var method: Boolean, var shorthand: Boolean) {
  var `type`: String = Syntax.Property
}

class RegexLiteral(var value: (String) => Any, var raw: String, pattern: String, flags: String) {
  var `type`: String = Syntax.Literal
  var regex = new /*RegexLiteral/regex*/ {
    var pattern = pattern
    var flags = flags
  }
}

class RestElement(var argument: Identifier | ArrayPattern | ObjectPattern) {
  var `type`: String = Syntax.RestElement
}

class ReturnStatement(var argument: Expression) {
  var `type`: String = Syntax.ReturnStatement
}

class Script(var body: Array[Declaration | Statement]) {
  var `type`: String = Syntax.Program
  var sourceType: String = "script"
}

class SequenceExpression(var expressions: Array[Expression]) {
  var `type`: String = Syntax.SequenceExpression
}

class SpreadElement(var argument: Expression) {
  var `type`: String = Syntax.SpreadElement
}

class StaticMemberExpression(var `object`: Expression, var property: Expression, var optional: Boolean) {
  var `type`: String = Syntax.MemberExpression
  var computed: Boolean = false
}

class Super {
  var `type`: String = Syntax.Super
}

class SwitchCase(var test: Expression, var consequent: Array[Statement]) {
  var `type`: String = Syntax.SwitchCase
}

class SwitchStatement(var discriminant: Expression, var cases: Array[SwitchCase]) {
  var `type`: String = Syntax.SwitchStatement
}

class TaggedTemplateExpression(var tag: Expression, var quasi: TemplateLiteral) {
  var `type`: String = Syntax.TaggedTemplateExpression
}

trait TemplateElementValue {
  var cooked: String = _
  var raw: String = _
}

class TemplateElement(var value: TemplateElementValue, var tail: Boolean) {
  var `type`: String = Syntax.TemplateElement
}

class TemplateLiteral(var quasis: Array[TemplateElement], var expressions: Array[Expression]) {
  var `type`: String = Syntax.TemplateLiteral
}

class ThisExpression {
  var `type`: String = Syntax.ThisExpression
}

class ThrowStatement(var argument: Expression) {
  var `type`: String = Syntax.ThrowStatement
}

class TryStatement(var block: BlockStatement, var handler: CatchClause, var finalizer: BlockStatement) {
  var `type`: String = Syntax.TryStatement
}

class UnaryExpression(var operator: String, var argument: Expression) {
  var `type`: String = Syntax.UnaryExpression
  var prefix: Boolean = true
}

class UpdateExpression(var operator: String, var argument: Expression, var prefix: Boolean) {
  var `type`: String = Syntax.UpdateExpression
}

class VariableDeclaration(var declarations: Array[VariableDeclarator], var kind: String) {
  var `type`: String = Syntax.VariableDeclaration
}

class VariableDeclarator(var id: Identifier | ArrayPattern | ObjectPattern, var init: Expression) {
  var `type`: String = Syntax.VariableDeclarator
}

class WhileStatement(var test: Expression, var body: Statement) {
  var `type`: String = Syntax.WhileStatement
}

class WithStatement(var `object`: Expression, var body: Statement) {
  var `type`: String = Syntax.WithStatement
}

class YieldExpression(var argument: Expression, var delegate: Boolean) {
  var `type`: String = Syntax.YieldExpression
}


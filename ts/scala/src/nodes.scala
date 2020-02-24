/*
ScalaFromJS: Dev 2020-02-24 19:42:52
nodes.ts
*/

package esprima
/* import { Syntax } from './syntax' */
type ArgumentListElement = Expression | SpreadElement
type ArrayExpressionElement = Expression | SpreadElement | Null
type ArrayPatternElement = AssignmentPattern | BindingIdentifier | BindingPattern | RestElement | Null
type BindingPattern = ArrayPattern | ObjectPattern
type BindingIdentifier = Identifier
type Declaration = AsyncFunctionDeclaration | ClassDeclaration | ExportDeclaration | FunctionDeclaration | ImportDeclaration | VariableDeclaration
type ExportableDefaultDeclaration = BindingIdentifier | BindingPattern | ClassDeclaration | Expression | FunctionDeclaration
type ExportableNamedDeclaration = AsyncFunctionDeclaration | ClassDeclaration | FunctionDeclaration | VariableDeclaration
type ExportDeclaration = ExportAllDeclaration | ExportDefaultDeclaration | ExportNamedDeclaration
type Expression = ArrayExpression | ArrowFunctionExpression | AssignmentExpression | AsyncArrowFunctionExpression | AsyncFunctionExpression | AwaitExpression | BinaryExpression | CallExpression | ClassExpression | ComputedMemberExpression | ConditionalExpression | Identifier | FunctionExpression | Literal | NewExpression | ObjectExpression | RegexLiteral | SequenceExpression | StaticMemberExpression | TaggedTemplateExpression | ThisExpression | UnaryExpression | UpdateExpression | YieldExpression
type FunctionParameter = AssignmentPattern | BindingIdentifier | BindingPattern
type ImportDeclarationSpecifier = ImportDefaultSpecifier | ImportNamespaceSpecifier | ImportSpecifier
type ObjectExpressionProperty = Property | SpreadElement
type ObjectPatternProperty = Property | RestElement
type Statement = AsyncFunctionDeclaration | BreakStatement | ContinueStatement | DebuggerStatement | DoWhileStatement | EmptyStatement | ExpressionStatement | Directive | ForStatement | ForInStatement | ForOfStatement | FunctionDeclaration | IfStatement | ReturnStatement | SwitchStatement | ThrowStatement | TryStatement | VariableDeclaration | WhileStatement | WithStatement
type PropertyKey = Identifier | Literal
type PropertyValue = AssignmentPattern | AsyncFunctionExpression | BindingIdentifier | BindingPattern | FunctionExpression
type StatementListItem = Declaration | Statement

class ArrayExpression(var elements: Array[ArrayExpressionElement]) {
  var `type`: String = Syntax.ArrayExpression
}

class ArrayPattern(var elements: Array[ArrayPatternElement]) {
  var `type`: String = Syntax.ArrayPattern
}

class ArrowFunctionExpression(var params: Array[FunctionParameter], var body: BlockStatement | Expression, var expression: Boolean) {
  var `type`: String = Syntax.ArrowFunctionExpression
  var id: Identifier | Null = null
  var generator: Boolean = false
  var async: Boolean = false
}

class AssignmentExpression(var operator: String, var left: Expression, var right: Expression) {
  var `type`: String = Syntax.AssignmentExpression
}

class AssignmentPattern(var left: BindingIdentifier | BindingPattern, var right: Expression) {
  var `type`: String = Syntax.AssignmentPattern
}

class AsyncArrowFunctionExpression(var params: Array[FunctionParameter], var body: BlockStatement | Expression, var expression: Boolean) {
  var `type`: String = Syntax.ArrowFunctionExpression
  var id: Identifier | Null = null
  var generator: Boolean = false
  var async: Boolean = true
}

class AsyncFunctionDeclaration(var id: Identifier | Null, var params: Array[FunctionParameter], var body: BlockStatement) {
  var `type`: String = Syntax.FunctionDeclaration
  var generator: Boolean = false
  var expression: Boolean = false
  var async: Boolean = true
}

class AsyncFunctionExpression(var id: Identifier | Null, var params: Array[FunctionParameter], var body: BlockStatement) {
  var `type`: String = Syntax.FunctionExpression
  var generator: Boolean = false
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
    val logical = operator == "||" || operator == "&&"
    this.`type` = if (logical) Syntax.LogicalExpression else Syntax.BinaryExpression
    this.operator = operator
    this.left = left
    this.right = right
  }
  
}

class BlockStatement(var body: Array[Statement]) {
  var `type`: String = Syntax.BlockStatement
}

class BreakStatement(var label: Identifier | Null) {
  var `type`: String = Syntax.BreakStatement
}

class CallExpression(var callee: Expression | Import, var arguments: Array[ArgumentListElement]) {
  var `type`: String = Syntax.CallExpression
}

class CatchClause(var param: BindingIdentifier | BindingPattern, var body: BlockStatement) {
  var `type`: String = Syntax.CatchClause
}

class ClassBody(var body: Array[Property]) {
  var `type`: String = Syntax.ClassBody
}

class ClassDeclaration(var id: Identifier | Null, var superClass: Identifier | Null, var body: ClassBody) {
  var `type`: String = Syntax.ClassDeclaration
}

class ClassExpression(var id: Identifier | Null, var superClass: Identifier | Null, var body: ClassBody) {
  var `type`: String = Syntax.ClassExpression
}

class ComputedMemberExpression(var `object`: Expression, var property: Expression) {
  var `type`: String = Syntax.MemberExpression
  var computed: Boolean = true
}

class ConditionalExpression(var test: Expression, var consequent: Expression, var alternate: Expression) {
  var `type`: String = Syntax.ConditionalExpression
}

class ContinueStatement(var label: Identifier | Null) {
  var `type`: String = Syntax.ContinueStatement
}

class DebuggerStatement() {
  var `type`: String = Syntax.DebuggerStatement
}

class Directive(var expression: Expression, var directive: String) {
  var `type`: String = Syntax.ExpressionStatement
}

class DoWhileStatement(var body: Statement, var test: Expression) {
  var `type`: String = Syntax.DoWhileStatement
}

class EmptyStatement() {
  var `type`: String = Syntax.EmptyStatement
}

class ExportAllDeclaration(var source: Literal) {
  var `type`: String = Syntax.ExportAllDeclaration
}

class ExportDefaultDeclaration(var declaration: ExportableDefaultDeclaration) {
  var `type`: String = Syntax.ExportDefaultDeclaration
}

class ExportNamedDeclaration(var declaration: ExportableNamedDeclaration | Null, var specifiers: Array[ExportSpecifier], var source: Literal | Null) {
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

class ForOfStatement(var left: Expression, var right: Expression, var body: Statement) {
  var `type`: String = Syntax.ForOfStatement
}

class ForStatement(var init: Expression | Null, var test: Expression | Null, var update: Expression | Null, var body: Statement) {
  var `type`: String = Syntax.ForStatement
}

class FunctionDeclaration(var id: Identifier | Null, var params: Array[FunctionParameter], var body: BlockStatement, var generator: Boolean) {
  var `type`: String = Syntax.FunctionDeclaration
  var expression: Boolean = false
  var async: Boolean = false
}

class FunctionExpression(var id: Identifier | Null, var params: Array[FunctionParameter], var body: BlockStatement, var generator: Boolean) {
  var `type`: String = Syntax.FunctionExpression
  var expression: Boolean = false
  var async: Boolean = false
}

class Identifier(var name: String) {
  var `type`: String = Syntax.Identifier
}

class IfStatement(var test: Expression, var consequent: Statement, var alternate: Statement | Null) {
  var `type`: String = Syntax.IfStatement
}

class Import() {
  var `type`: String = Syntax.Import
}

class ImportDeclaration(var specifiers: Array[ImportDeclarationSpecifier], var source: Literal) {
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

class Literal(var value: Boolean | Double | String | Null, var raw: String) {
  var `type`: String = Syntax.Literal
}

class MetaProperty(var meta: Identifier, var property: Identifier) {
  var `type`: String = Syntax.MetaProperty
}

class MethodDefinition(var key: Expression | Null, var computed: Boolean, var value: AsyncFunctionExpression | FunctionExpression | Null, var kind: String, var static: Boolean) {
  var `type`: String = Syntax.MethodDefinition
}

class Module(var body: Array[StatementListItem]) {
  var `type`: String = Syntax.Program
  var sourceType: String = "module"
}

class NewExpression(var callee: Expression, var arguments: Array[ArgumentListElement]) {
  var `type`: String = Syntax.NewExpression
}

class ObjectExpression(var properties: Array[ObjectExpressionProperty]) {
  var `type`: String = Syntax.ObjectExpression
}

class ObjectPattern(var properties: Array[ObjectPatternProperty]) {
  var `type`: String = Syntax.ObjectPattern
}

class Property(var kind: String, var key: PropertyKey, var computed: Boolean, var value: PropertyValue | Null, var method: Boolean, var shorthand: Boolean) {
  var `type`: String = Syntax.Property
}

class RegexLiteral(var value: RegExp, var raw: String, pattern: String, flags: String) {
  var `type`: String = Syntax.Literal
  var regex = new {
    var pattern = pattern
    var flags = flags
  }
}

class RestElement(var argument: BindingIdentifier | BindingPattern) {
  var `type`: String = Syntax.RestElement
}

class ReturnStatement(var argument: Expression | Null) {
  var `type`: String = Syntax.ReturnStatement
}

class Script(var body: Array[StatementListItem]) {
  var `type`: String = Syntax.Program
  var sourceType: String = "script"
}

class SequenceExpression(var expressions: Array[Expression]) {
  var `type`: String = Syntax.SequenceExpression
}

class SpreadElement(var argument: Expression) {
  var `type`: String = Syntax.SpreadElement
}

class StaticMemberExpression(var `object`: Expression, var property: Expression) {
  var `type`: String = Syntax.MemberExpression
  var computed: Boolean = false
}

class Super() {
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

trait TemplateElementValue() {
  var cooked: String = _
  var raw: String = _
}

class TemplateElement(var value: TemplateElementValue, var tail: Boolean) {
  var `type`: String = Syntax.TemplateElement
}

class TemplateLiteral(var quasis: Array[TemplateElement], var expressions: Array[Expression]) {
  var `type`: String = Syntax.TemplateLiteral
}

class ThisExpression() {
  var `type`: String = Syntax.ThisExpression
}

class ThrowStatement(var argument: Expression) {
  var `type`: String = Syntax.ThrowStatement
}

class TryStatement(var block: BlockStatement, var handler: CatchClause | Null, var finalizer: BlockStatement | Null) {
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

class VariableDeclarator(var id: BindingIdentifier | BindingPattern, var init: Expression | Null) {
  var `type`: String = Syntax.VariableDeclarator
}

class WhileStatement(var test: Expression, var body: Statement) {
  var `type`: String = Syntax.WhileStatement
}

class WithStatement(var `object`: Expression, var body: Statement) {
  var `type`: String = Syntax.WithStatement
}

class YieldExpression(var argument: Expression | Null, var delegate: Boolean) {
  var `type`: String = Syntax.YieldExpression
}


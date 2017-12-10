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

  // export type ArgumentListElement = Expression | SpreadElement;

  trait ArgumentListElement extends Node
  trait ArrayExpressionElement extends Node
  trait ArrayPatternElement extends Node
  trait BindingPattern extends Node
  trait BindingIdentifier extends Node
  trait Declaration extends Node
  trait ExportableDefaultDeclaration extends Node
  trait ExportableNamedDeclaration extends Node
  trait ExportDeclaration extends Node
  trait Expression extends Node
  trait FunctionParameter extends Node
  trait ImportDeclarationSpecifier extends Node
  trait ObjectExpressionProperty extends Node
  trait ObjectPatternProperty extends Node
  trait Statement extends Node
  trait PropertyKey extends Node
  trait PropertyValue extends Node
  trait StatementListItem extends Node


  /*
  export type ArgumentListElement = Expression | SpreadElement;
  export type ArrayExpressionElement = Expression | SpreadElement | null;
  export type ArrayPatternElement = AssignmentPattern | BindingIdentifier | BindingPattern | RestElement | null;
  export type BindingPattern = ArrayPattern | ObjectPattern;
  export type BindingIdentifier = Identifier;
  export type Declaration = AsyncFunctionDeclaration | ClassDeclaration | ExportDeclaration | FunctionDeclaration | ImportDeclaration | VariableDeclaration;
  export type ExportableDefaultDeclaration = BindingIdentifier | BindingPattern | ClassDeclaration | Expression | FunctionDeclaration;
  export type ExportableNamedDeclaration = AsyncFunctionDeclaration | ClassDeclaration | FunctionDeclaration | VariableDeclaration;
  export type ExportDeclaration = ExportAllDeclaration | ExportDefaultDeclaration | ExportNamedDeclaration;
  export type Expression = ArrayExpression | ArrowFunctionExpression | AssignmentExpression | AsyncArrowFunctionExpression | AsyncFunctionExpression |
      AwaitExpression | BinaryExpression | CallExpression | ClassExpression | ComputedMemberExpression |
      ConditionalExpression | Identifier | FunctionExpression | Literal | NewExpression | ObjectExpression |
      RegexLiteral | SequenceExpression | StaticMemberExpression | TaggedTemplateExpression |
      ThisExpression | UnaryExpression | UpdateExpression | YieldExpression;
  export type FunctionParameter = AssignmentPattern | BindingIdentifier | BindingPattern;
  export type ImportDeclarationSpecifier = ImportDefaultSpecifier | ImportNamespaceSpecifier | ImportSpecifier;
  export type ObjectExpressionProperty = Property | SpreadElement;
  export type ObjectPatternProperty = Property | RestElement;
  export type Statement = AsyncFunctionDeclaration | BreakStatement | ContinueStatement | DebuggerStatement | DoWhileStatement |
      EmptyStatement | ExpressionStatement | Directive | ForStatement | ForInStatement | ForOfStatement |
      FunctionDeclaration | IfStatement | ReturnStatement | SwitchStatement | ThrowStatement |
      TryStatement | VariableDeclaration | WhileStatement | WithStatement;
  export type PropertyKey = Identifier | Literal;
  export type PropertyValue = AssignmentPattern | AsyncFunctionExpression | BindingIdentifier | BindingPattern | FunctionExpression;
  export type StatementListItem = Declaration | Statement;

   */


  trait HasGenerator {
    def generator: Boolean
  }

  trait SymbolDeclaration {
    def id: Node

    /** default implementation is to read id node
      * Implementaion can override this even when id has a different meaning
    */
    def symbolIds: Seq[Node] = Seq(id)
  }

  object SymbolDeclaration {
    def unapplySeq(decl: SymbolDeclaration): Option[Seq[String]] = {
      val symbolNames = decl.symbolIds.flatMap {
        case Identifier(name) =>
          Some(name)
        case _ =>
          None
      }
      Some(symbolNames)
    }
  }

  trait IsScope

  abstract class CommentNode extends Node {
    var value: String = _
  }

  class ArrayExpression(var elements: Seq[ArrayExpressionElement]) extends Node {
    var `type` = Syntax.ArrayExpression
  }


  class ArrayPattern(var elements: Array[ArrayPatternElement]) extends Node {
    var `type` = Syntax.ArrayPattern
  }

  trait BlockStatementOrExpression extends Node // BlockStatement | Expression

  class ArrowFunctionExpression(var params: Seq[FunctionParameter], var body: BlockStatementOrExpression, var expression: Boolean) extends Node with HasGenerator {
    var `type` = Syntax.ArrowFunctionExpression
    var id = null
    var generator: Boolean = false
    var async: Boolean = false
  }


  class AssignmentExpression(var operator: String, var left: Expression, var right: Expression) extends Node {
    var `type` = Syntax.AssignmentExpression
  }

  trait AssignmentPatternArg extends Node // BindingIdentifier | BindingPattern

  class AssignmentPattern(var left: AssignmentPatternArg, var right: AssignmentPatternArg) extends Node {
    var `type` = Syntax.AssignmentPattern
  }


  class AsyncArrowFunctionExpression(var params: Seq[FunctionParameter], var body: BlockStatementOrExpression, var expression: Boolean) extends Node with HasGenerator {
    var `type` = Syntax.ArrowFunctionExpression
    var id = null
    var generator: Boolean = false
    var async: Boolean = true
  }

  trait AFunctionDeclaration extends Node with HasGenerator

  class AsyncFunctionDeclaration(var id: Identifier, var params: Seq[FunctionParameter], var body: BlockStatement) extends AFunctionDeclaration {
    var `type` = Syntax.FunctionDeclaration
    var generator: Boolean = false
    var expression: Boolean = false
    var async: Boolean = true
  }


  class AsyncFunctionExpression(var id: Identifier, var params: Seq[FunctionParameter], var body: BlockStatement) extends Node with HasGenerator {
    var `type` = Syntax.FunctionExpression
    var generator: Boolean = false
    var expression: Boolean = false
    var async: Boolean = true
  }


  class AwaitExpression(var argument: Expression) extends Node {
    var `type` = Syntax.AwaitExpression
  }


  class BinaryExpression(operator_par: String, left_par: Expression, right_par: Expression) extends Node {
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


  class BlockStatement(var body: Seq[Statement]) extends Node with IsScope {
    var `type` = Syntax.BlockStatement
  }


  class BreakStatement(var label: Identifier) extends Node {
    var `type` = Syntax.BreakStatement
  }


  trait ExpressionOrImport extends Node // Expression | Import

  class CallExpression(var callee: ExpressionOrImport, var arguments: Seq[ArgumentListElement]) extends Node {
    var `type` = Syntax.CallExpression
  }

  trait BindingIdentifierOrPattern extends Node // BindingIdentifier | BindingPattern


  class CatchClause(var param: BindingIdentifierOrPattern, var body: BlockStatement) extends Node {
    var `type` = Syntax.CatchClause
  }

  // actually MethodDefinition seems to work
  class ClassBody(var body: Seq[Property]) extends Node with IsScope {
    var `type` = Syntax.ClassBody
  }


  class ClassDeclaration(var id: Identifier, var superClass: Identifier, var body: ClassBody) extends Node with SymbolDeclaration {
    var `type` = Syntax.ClassDeclaration
  }


  class ClassExpression(var id: Identifier, var superClass: Identifier, var body: ClassBody) extends Node {
    var `type` = Syntax.ClassExpression
  }


  class ComputedMemberExpression(var `object`: Expression, var property: Expression) extends Node {
    var `type` = Syntax.MemberExpression
    var computed: Boolean = true
  }


  class ConditionalExpression(var test: Expression, var consequent: Expression, var alternate: Expression) extends Node {
    var `type` = Syntax.ConditionalExpression
  }


  class ContinueStatement(var label: Identifier) extends Node {
    var `type` = Syntax.ContinueStatement
  }


  class DebuggerStatement() extends Node {
    var `type` = Syntax.DebuggerStatement
  }


  class Directive(var expression: Expression, var directive: String) extends Node {
    var `type` = Syntax.ExpressionStatement
  }


  class DoWhileStatement(var body: Statement, var test: Expression) extends Node {
    var `type` = Syntax.DoWhileStatement
  }


  class EmptyStatement() extends Node {
    var `type` = Syntax.EmptyStatement
  }


  class ExportAllDeclaration(var source: Literal) extends Node {
    var `type` = Syntax.ExportAllDeclaration
  }


  class ExportDefaultDeclaration(var declaration: ExportableDefaultDeclaration) extends Node {
    var `type` = Syntax.ExportDefaultDeclaration
  }


  class ExportNamedDeclaration(var declaration: ExportableNamedDeclaration, var specifiers: Seq[ExportSpecifier], var source: Literal) extends Node {
    var `type` = Syntax.ExportNamedDeclaration
  }


  class ExportSpecifier(var local: Identifier, var exported: Identifier) extends Node {
    var `type` = Syntax.ExportSpecifier
  }


  class ExpressionStatement(var Expression: Node) extends Node {
    var `type` = Syntax.ExpressionStatement
  }


  class ForInStatement(var left: Expression, var right: Expression, var body: Statement) extends Node {
    var `type` = Syntax.ForInStatement
    var each: Boolean = false
  }


  class ForOfStatement(var left: Expression, var right: Expression, var body: Statement) extends Node {
    var `type` = Syntax.ForOfStatement
  }


  class ForStatement(var init: Expression, var test: Expression, var update: Expression, var body: Statement) extends Node {
    var `type` = Syntax.ForStatement
  }


  case class FunctionDeclaration(var id: Identifier, var params: Seq[FunctionParameter], var body: BlockStatement, var generator: Boolean) extends Node with AFunctionDeclaration with SymbolDeclaration {
    var `type` = Syntax.FunctionDeclaration
    var expression: Boolean = false
    var async: Boolean = false

    override def symbolIds = id +: params
  }


  class FunctionExpression(var id: Identifier, var params: Seq[FunctionParameter], var body: BlockStatement, var generator: Boolean) extends Node with HasGenerator with SymbolDeclaration {
    var `type` = Syntax.FunctionExpression
    var expression: Boolean = false
    var async: Boolean = false

    override def symbolIds = params
  }


  case class Identifier(var name: String) extends Node {
    var `type` = Syntax.Identifier
  }


  class IfStatement(var test: Expression, var consequent: Statement, var alternate: Statement) extends Node {
    var `type` = Syntax.IfStatement
  }


  class Import() extends Node {
    var `type` = Syntax.Import
  }


  class ImportDeclaration(var specifiers: Seq[ImportDeclarationSpecifier], var source: Literal) extends Node {
    var `type` = Syntax.ImportDeclaration
  }


  class ImportDefaultSpecifier(var local: Identifier) extends Node {
    var `type` = Syntax.ImportDefaultSpecifier
  }


  class ImportNamespaceSpecifier(var local: Identifier) extends Node {
    var `type` = Syntax.ImportNamespaceSpecifier
  }


  class ImportSpecifier(var local: Identifier, var imported: Identifier) extends Node {
    var `type` = Syntax.ImportSpecifier
  }


  class LabeledStatement(var label: Identifier, var body: Statement) extends Node {
    var `type` = Syntax.LabeledStatement
  }


  class Literal(var value: OrType, var raw: String) extends Node {
    var `type` = Syntax.Literal
  }


  class MetaProperty(var meta: Identifier, var property: Identifier) extends Node {
    var `type` = Syntax.MetaProperty
  }

  trait AFunctionExpression extends Node // AsyncFunctionExpression | FunctionExpression

  class MethodDefinition(var key: Expression, var computed: Boolean, var value: AFunctionExpression, var kind: Boolean, var static: Boolean) extends Node {
    var `type` = Syntax.MethodDefinition
  }

  sealed abstract class Program(var body: Seq[StatementListItem]) extends Node with IsScope {
    var `type` = Syntax.Program

    def sourceType: String

    var comments: ArrayBuffer[CommentHandler.Comment] = _
    var tokens: ArrayBuffer[Parser.TokenEntry] = _
    var errors: ArrayBuffer[ErrorHandler.Error] = _

  }

  class Module(body: Seq[StatementListItem]) extends Program(body) {
    def sourceType: String = "module"
  }

  class NewExpression(var callee: Expression, var arguments: Seq[ArgumentListElement]) extends Node {
    var `type` = Syntax.NewExpression
  }


  class ObjectExpression(var properties: Seq[ObjectExpressionProperty]) extends Node {
    var `type` = Syntax.ObjectExpression
  }


  class ObjectPattern(var properties: Seq[ObjectPatternProperty]) extends Node {
    var `type` = Syntax.ObjectPattern
  }


  class Property(var kind: String, var key: PropertyKey, var computed: Boolean, var value: PropertyValue, var method: Boolean, var shorthand: Boolean) extends Node {
    var `type` = Syntax.Property
  }


  class RegexLiteral(var value: RegExp, var raw: String, pattern: String, flags: String) extends Node {
    var `type` = Syntax.Literal
    var regex = new RegExp(
      pattern = pattern,
      flags = flags
    )
  }


  class RestElement(var argument: BindingIdentifierOrPattern) extends Node {
    var `type` = Syntax.RestElement
  }


  class ReturnStatement(var argument: Expression) extends Node {
    var `type` = Syntax.ReturnStatement
  }


  class Script(body: Seq[StatementListItem]) extends Program(body) {
    def sourceType: String = "script"
  }

  class SequenceExpression(var expressions: Seq[Expression]) extends Node {
    var `type` = Syntax.SequenceExpression
  }


  class SpreadElement(var argument: Expression) extends Node with ArgumentListElement {
    var `type` = Syntax.SpreadElement
  }


  class StaticMemberExpression(var `object`: Expression, var property: Expression) extends Node {
    var `type` = Syntax.MemberExpression
    var computed: Boolean = false
  }


  class Super() extends Node {
    var `type` = Syntax.Super
  }


  class SwitchCase(var test: Expression, var consequent: Seq[Statement]) extends Node {
    var `type` = Syntax.SwitchCase
  }


  class SwitchStatement(var discriminant: Expression, var cases: Seq[SwitchCase]) extends Node {
    var `type` = Syntax.SwitchStatement
  }


  class TaggedTemplateExpression(var tag: Expression, var quasi: TemplateLiteral) extends Node {
    var `type` = Syntax.TaggedTemplateExpression
  }


  trait TemplateElementValue {
    def cooked: String
    def raw: String
  }

  class TemplateElement(var value: TemplateElementValue, var tail: Boolean) extends Node {
    var `type`: String = Syntax.TemplateElement
  }


  class TemplateLiteral(var quasis: Seq[TemplateElement], var expressions: Seq[Node]) extends Node {
    var `type` = Syntax.TemplateLiteral
  }


  class ThisExpression() extends Node {
    var `type` = Syntax.ThisExpression
  }


  class ThrowStatement(var argument: Expression) extends Node {
    var `type` = Syntax.ThrowStatement
  }


  class TryStatement(var block: BlockStatement, var handler: CatchClause, var finalizer: BlockStatement) extends Node {
    var `type` = Syntax.TryStatement
  }


  class UnaryExpression(var operator: String, var argument: Expression) extends Node {
    var `type` = Syntax.UnaryExpression
    var prefix: Boolean = true
  }


  class UpdateExpression(var operator: String, var argument: Expression, var prefix: Boolean) extends Node {
    var `type` = Syntax.UpdateExpression
  }


  class VariableDeclaration(var declarations: Seq[VariableDeclarator], var kind: String) extends Node {
    var `type` = Syntax.VariableDeclaration
  }


  case class VariableDeclarator(var id: BindingIdentifierOrPattern, var init: Expression) extends Node with SymbolDeclaration {
    var `type` = Syntax.VariableDeclarator
  }


  class WhileStatement(var test: Expression, var body: Statement) extends Node {
    var `type` = Syntax.WhileStatement
  }


  class WithStatement(var `object`: Expression, var body: Statement) extends Node {
    var `type` = Syntax.WithStatement
  }


  class YieldExpression(var argument: Expression, var delegate: Boolean) extends Node {
    var `type` = Syntax.YieldExpression
  }


}
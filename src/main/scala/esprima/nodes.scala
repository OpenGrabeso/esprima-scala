/*
ScalaFromJS: 2017-12-06 21:28:23.723
nodes.js
*/

package esprima

import esprima.port.RegExp

import scala.collection.mutable.ArrayBuffer

object Node {

  sealed trait Node {
    override def toString = `type`

    def `type`: String
    var range: (Int, Int) = _
    var loc: Scanner.SourceLocation = _

    var leadingComments: ArrayBuffer[CommentHandler.Comment] = _
    var innerComments: ArrayBuffer[CommentHandler.Comment] = _
    var trailingComments: ArrayBuffer[CommentHandler.Comment] = _

    def copyNode(that: Node): Node = {
      this.range = that.range
      this.loc = that.loc
      this.leadingComments = that.leadingComments
      this.innerComments = that.innerComments
      this.trailingComments = that.trailingComments
      this
    }
    override def clone: Node = ???
  }

  sealed trait ExpressionOrStatement extends Node
  sealed trait Expression extends Node with ArgumentListElement with ArrayExpressionElement with ExpressionOrStatement
    with ExportableDefaultDeclaration with ExpressionOrImport with BlockStatementOrExpression with PropertyValue
  /* ArrayExpression | ArrowFunctionExpression | AssignmentExpression | AsyncArrowFunctionExpression | AsyncFunctionExpression |
    AwaitExpression | BinaryExpression | CallExpression | ClassExpression | ComputedMemberExpression |
    ConditionalExpression | Identifier | FunctionExpression | Literal | NewExpression | ObjectExpression |
    RegexLiteral | SequenceExpression | StaticMemberExpression | TaggedTemplateExpression |
    ThisExpression | UnaryExpression | UpdateExpression | YieldExpression; */
  sealed trait ArgumentListElement extends Node // Expression | SpreadElement;
  sealed trait ArrayExpressionElement extends Node // Expression | SpreadElement | null;
  sealed trait BindingPattern extends Node with ArrayPatternElement with ExportableDefaultDeclaration with FunctionParameter with PropertyValue with BindingIdentifierOrPattern // ArrayPattern | ObjectPattern
  sealed trait BindingIdentifier extends Node with ArrayPatternElement with ExportableDefaultDeclaration with FunctionParameter with PropertyValue with BindingIdentifierOrPattern // Identifier;
  sealed trait ArrayPatternElement extends Node // AssignmentPattern | BindingIdentifier | BindingPattern | RestElement | null;
  sealed trait ExportDeclaration extends Node with Declaration // ExportAllDeclaration | ExportDefaultDeclaration | ExportNamedDeclaration;
  sealed trait Declaration extends Node with StatementListItem // = AsyncFunctionDeclaration | ClassDeclaration | ExportDeclaration | FunctionDeclaration | ImportDeclaration | VariableDeclaration;
  sealed trait ExportableDefaultDeclaration extends Node // BindingIdentifier | BindingPattern | ClassDeclaration | Expression | FunctionDeclaration;
  sealed trait ExportableNamedDeclaration extends Node // AsyncFunctionDeclaration | ClassDeclaration | FunctionDeclaration | VariableDeclaration;
  sealed trait FunctionParameter extends Node // = AssignmentPattern | BindingIdentifier | BindingPattern;
  sealed trait ImportDeclarationSpecifier extends Node //= ImportDefaultSpecifier | ImportNamespaceSpecifier | ImportSpecifier;
  sealed trait ObjectExpressionProperty extends Node //= Property | SpreadElement;
  sealed trait ObjectPatternProperty extends Node with BindingIdentifierOrPattern //= Property | RestElement;
  sealed trait Statement extends Node with StatementListItem with ExpressionOrStatement
  /*= AsyncFunctionDeclaration | BreakStatement | ContinueStatement | DebuggerStatement | DoWhileStatement |
    EmptyStatement | ExpressionStatement | Directive | ForStatement | ForInStatement | ForOfStatement |
    FunctionDeclaration | IfStatement | ReturnStatement | SwitchStatement | ThrowStatement |
    TryStatement | VariableDeclaration | WhileStatement | WithStatement;*/
  sealed trait PropertyKey extends Node //= Identifier | Literal;
  sealed trait PropertyValue extends Node //= AssignmentPattern | AsyncFunctionExpression | BindingIdentifier | BindingPattern | FunctionExpression;
  sealed trait StatementListItem extends Node with ExportableNamedDeclaration //= Declaration | Statement;

  sealed trait BindingIdentifierOrPattern extends Node // BindingIdentifier | BindingPattern

  sealed trait ExpressionOrImport extends Node // Expression | Import

  sealed trait BlockStatementOrExpression extends Node // BlockStatement | Expression

  sealed trait ClassBodyElement extends Node

  val ArrowParameterPlaceHolder = "ArrowParameterPlaceHolder"

  class ArrowParameterPlaceHolder extends Node.Node with Node.Expression with Node.FunctionParameter {
    def `type` = ArrowParameterPlaceHolder

    var params: Seq[Node.ArgumentListElement] = _
    var async: Boolean = _
  }


  sealed trait HasGenerator {
    def generator: Boolean
  }

  sealed trait SymbolDeclaration {
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

  case class ArrayExpression(var elements: Seq[ArrayExpressionElement]) extends Node with Expression {
    override def clone = copy().copyNode(this)
    def `type` = Syntax.ArrayExpression
  }


  case class ArrayPattern(var elements: Seq[ArrayPatternElement]) extends Node with BindingPattern {
    override def clone = copy().copyNode(this)
    def `type` = Syntax.ArrayPattern
  }


  case class ArrowFunctionExpression(var params: Seq[FunctionParameter], var body: BlockStatementOrExpression, var expression: Boolean) extends Node with HasGenerator with Expression {
    override def clone = copy().copyNode(this)
    def `type` = Syntax.ArrowFunctionExpression
    var id = null
    var generator: Boolean = false
    var async: Boolean = false
  }


  case class AssignmentExpression(var operator: String, var left: Expression, var right: Expression) extends Node with Expression {
    override def clone = copy().copyNode(this)
    def `type` = Syntax.AssignmentExpression
  }

  /* ported: added ObjectPatternProperty because of reinterpretExpressionAsObjectPattern */
  case class AssignmentPattern(var left: BindingIdentifierOrPattern, var right: Expression) extends Node
    with ArrayPatternElement with FunctionParameter with PropertyValue with ObjectPatternProperty {
    override def clone = copy().copyNode(this)
    def `type` = Syntax.AssignmentPattern
  }


  case class AsyncArrowFunctionExpression(var params: Seq[FunctionParameter], var body: BlockStatementOrExpression, var expression: Boolean) extends Node
    with HasGenerator with Expression {
    def `type` = Syntax.ArrowFunctionExpression
    var id = null
    var generator: Boolean = false
    var async: Boolean = true
  }

  /* ported: added StatementListItem because of parseFunctionDeclaration, Statement because of parseLabelledStatement */
  trait AFunctionDeclaration extends Node with HasGenerator with StatementListItem with Statement with ExportableNamedDeclaration with ExportableDefaultDeclaration

  case class AsyncFunctionDeclaration(var id: Identifier, var params: Seq[FunctionParameter], var body: BlockStatement) extends Node
    with AFunctionDeclaration with Declaration with ExportableNamedDeclaration with Statement {
    override def clone = copy().copyNode(this)
    def `type` = Syntax.FunctionDeclaration
    var generator: Boolean = false
    var expression: Boolean = false
    var async: Boolean = true
  }


  case class AsyncFunctionExpression(var id: Identifier, var params: Seq[FunctionParameter], var body: BlockStatement) extends Node
    with HasGenerator with Expression with PropertyValue {
    def `type` = Syntax.FunctionExpression
    override def clone = copy().copyNode(this)
    var generator: Boolean = false
    var expression: Boolean = false
    var async: Boolean = true
  }


  case class AwaitExpression(var argument: Expression) extends Node with Expression {
    def `type` = Syntax.AwaitExpression
    override def clone = copy().copyNode(this)
  }


  case class BinaryExpression(var operator: String, var left: Expression, var right: Expression) extends Node with Expression {
    private val logical = operator == "||" || operator == "&&"
    def `type` = if (logical) Syntax.LogicalExpression else Syntax.BinaryExpression
    override def clone = copy().copyNode(this)
  }


  /* ported: type adjusted */
  case class BlockStatement(var body: Seq[StatementListItem]) extends Node with IsScope with Statement with BlockStatementOrExpression {
    def `type` = Syntax.BlockStatement
    override def clone = copy().copyNode(this)
  }


  case class BreakStatement(var label: Identifier) extends Node with Statement {
    def `type` = Syntax.BreakStatement
    override def clone = copy().copyNode(this).copyNode(this)
  }


  case class CallExpression(var callee: ExpressionOrImport, var arguments: Seq[ArgumentListElement]) extends Node with Expression {
    def `type` = Syntax.CallExpression
    override def clone = copy().copyNode(this)
  }


  case class CatchClause(var param: BindingIdentifierOrPattern, var body: BlockStatement) extends Node {
    def `type` = Syntax.CatchClause
    override def clone = copy().copyNode(this)
  }

  // actually MethodDefinition seems to work
  case class ClassBody(var body: Seq[ClassBodyElement]) extends Node with IsScope {
    def `type` = Syntax.ClassBody
    override def clone = copy().copyNode(this)
  }


  // ported: added Statement because of parseLabelledStatement
  case class ClassDeclaration(var id: Identifier, var superClass: Identifier, var body: ClassBody) extends Node
    with SymbolDeclaration with Declaration with ExportableDefaultDeclaration with ExportableNamedDeclaration with Statement {
    def `type` = Syntax.ClassDeclaration
    override def clone = copy().copyNode(this)
  }


  case class ClassExpression(var id: Identifier, var superClass: Identifier, var body: ClassBody) extends Node with Expression {
    def `type` = Syntax.ClassExpression
    override def clone = copy().copyNode(this)
  }


  /* ported: added ArrayPatternElement because of reinterpretExpressionAsArrayPattern */
  case class ComputedMemberExpression(var `object`: Expression, var property: Expression) extends Node with Expression with ArrayPatternElement {
    def `type` = Syntax.MemberExpression
    var computed: Boolean = true
    override def clone = copy().copyNode(this)
  }


  case class ConditionalExpression(var test: Expression, var consequent: Expression, var alternate: Expression) extends Node with Expression {
    def `type` = Syntax.ConditionalExpression
    override def clone = copy().copyNode(this)
  }


  case class ContinueStatement(var label: Identifier) extends Node with Statement {
    def `type` = Syntax.ContinueStatement
    override def clone = copy().copyNode(this)
  }


  case class DebuggerStatement() extends Node with Statement {
    def `type` = Syntax.DebuggerStatement
    override def clone = DebuggerStatement().copyNode(this)
  }


  case class Directive(var expression: Expression, var directive: String) extends Node with Statement {
    def `type` = Syntax.ExpressionStatement
    override def clone = copy().copyNode(this)
  }


  case class DoWhileStatement(var body: Statement, var test: Expression) extends Node with Statement {
    def `type` = Syntax.DoWhileStatement
    override def clone = copy().copyNode(this)
  }


  case class EmptyStatement() extends Node with Statement {
    def `type` = Syntax.EmptyStatement
    override def clone = EmptyStatement().copyNode(this)
  }


  case class ExportAllDeclaration(var source: Literal) extends Node with ExportDeclaration {
    def `type` = Syntax.ExportAllDeclaration
    override def clone = copy().copyNode(this)
  }


  case class ExportDefaultDeclaration(var declaration: ExportableDefaultDeclaration) extends Node with ExportDeclaration {
    def `type` = Syntax.ExportDefaultDeclaration
    override def clone = copy().copyNode(this)
  }


  case class ExportNamedDeclaration(var declaration: ExportableNamedDeclaration, var specifiers: Seq[ExportSpecifier], var source: Literal) extends Node with ExportDeclaration {
    def `type` = Syntax.ExportNamedDeclaration
    override def clone = copy().copyNode(this)
  }


  case class ExportSpecifier(var local: Identifier, var exported: Identifier) extends Node {
    def `type` = Syntax.ExportSpecifier
    override def clone = copy().copyNode(this)
  }


  case class ExpressionStatement(var expression: Expression) extends Node with Statement {
    def `type` = Syntax.ExpressionStatement
    override def clone = copy().copyNode(this)
  }


  case class ForInStatement(var left: ExpressionOrStatement, var right: Expression, var body: Statement) extends Node with Statement {
    def `type` = Syntax.ForInStatement
    var each: Boolean = false
    override def clone = copy().copyNode(this)
  }


  case class ForOfStatement(var left: ExpressionOrStatement, var right: Expression, var body: Statement) extends Node with Statement {
    def `type` = Syntax.ForOfStatement
    override def clone = copy().copyNode(this)
  }


  case class ForStatement(var init: ExpressionOrStatement, var test: Expression, var update: Expression, var body: Statement) extends Node with Statement {
    def `type` = Syntax.ForStatement
    override def clone = copy().copyNode(this)
  }


  case class FunctionDeclaration(var id: Identifier, var params: Seq[FunctionParameter], var body: BlockStatement, var generator: Boolean) extends Node
    with AFunctionDeclaration with SymbolDeclaration with Declaration with ExportableDefaultDeclaration with ExportableNamedDeclaration with Statement {
    def `type` = Syntax.FunctionDeclaration
    override def clone = copy().copyNode(this)
    var expression: Boolean = false
    var async: Boolean = false

    override def symbolIds = id +: params
  }


  case class FunctionExpression(var id: Identifier, var params: Seq[FunctionParameter], var body: BlockStatement, var generator: Boolean) extends Node
    with HasGenerator with SymbolDeclaration with Expression with PropertyValue {
    def `type` = Syntax.FunctionExpression
    override def clone = copy().copyNode(this)
    var expression: Boolean = false
    var async: Boolean = false

    override def symbolIds = params
  }


  case class Identifier(var name: String) extends Node with Expression with BindingIdentifier with PropertyKey {
    def `type` = Syntax.Identifier
    override def clone = copy().copyNode(this)
  }


  case class IfStatement(var test: Expression, var consequent: Statement, var alternate: Statement) extends Node with Statement {
    def `type` = Syntax.IfStatement
    override def clone = copy().copyNode(this)
  }


  case class Import() extends Node with ExpressionOrImport {
    def `type` = Syntax.Import
    override def clone = Import().copyNode(this)
  }


  case class ImportDeclaration(var specifiers: Seq[ImportDeclarationSpecifier], var source: Literal) extends Node with Declaration {
    def `type` = Syntax.ImportDeclaration
    override def clone = copy().copyNode(this)
  }


  case class ImportDefaultSpecifier(var local: Identifier) extends Node with ImportDeclarationSpecifier {
    def `type` = Syntax.ImportDefaultSpecifier
    override def clone = copy().copyNode(this)
  }


  case class ImportNamespaceSpecifier(var local: Identifier) extends Node with ImportDeclarationSpecifier {
    def `type` = Syntax.ImportNamespaceSpecifier
    override def clone = copy().copyNode(this)
  }


  case class ImportSpecifier(var local: Identifier, var imported: Identifier) extends Node with ImportDeclarationSpecifier {
    def `type` = Syntax.ImportSpecifier
    override def clone = copy().copyNode(this)
  }


  case class LabeledStatement(var label: Identifier, var body: Statement) extends Node with Statement {
    def `type` = Syntax.LabeledStatement
    override def clone = copy().copyNode(this)
  }


  case class Literal(var value: OrType, var raw: String) extends Node with Expression with PropertyKey {
    def `type` = Syntax.Literal
    override def clone = copy().copyNode(this)
  }


  /* ported: added Expression because of parseNewExpression */
  case class MetaProperty(var meta: Identifier, var property: Identifier) extends Node with Expression {
    def `type` = Syntax.MetaProperty
    override def clone = copy().copyNode(this)
  }

  trait AFunctionExpression extends Node // AsyncFunctionExpression | FunctionExpression

  case class MethodDefinition(var key: PropertyKey, var computed: Boolean, var value: PropertyValue, var kind: Boolean, var static: Boolean) extends Node with ClassBodyElement {
    def `type` = Syntax.MethodDefinition
    override def clone = copy().copyNode(this)
  }

  sealed abstract class Program(var body: Seq[StatementListItem]) extends Node with IsScope {
    def `type` = Syntax.Program

    def sourceType: String

    var comments: ArrayBuffer[CommentHandler.Comment] = _
    var tokens: ArrayBuffer[Parser.TokenEntry] = _
    var errors: ArrayBuffer[ErrorHandler.Error] = _

    def copyNode(that: Program): Program = {
      super.copyNode(that)
      this.comments = that.comments
      this.tokens = that.tokens
      this.errors = that.errors
      this
    }
  }

  object Program {
    def unapply(arg: Program): Option[Seq[StatementListItem]] = Some(arg.body)
  }

  class Module(body: Seq[StatementListItem]) extends Program(body) {
    def sourceType: String = "module"
    override def clone = new Module(body).copyNode(this)
  }

  case class NewExpression(var callee: Expression, var arguments: Seq[ArgumentListElement]) extends Node with Expression {
    def `type` = Syntax.NewExpression
    override def clone = copy().copyNode(this)
  }


  case class ObjectExpression(var properties: Seq[ObjectExpressionProperty]) extends Node with Expression {
    def `type` = Syntax.ObjectExpression
    override def clone = copy().copyNode(this)
  }


  /* ported: added ObjectPatternProperty because of reinterpretExpressionAsObjectPattern */
  case class ObjectPattern(var properties: Seq[ObjectPatternProperty]) extends Node with BindingPattern with ObjectPatternProperty {
    def `type` = Syntax.ObjectPattern
    override def clone = copy().copyNode(this)
  }


  case class Property(var kind: String, var key: PropertyKey, var computed: Boolean, var value: PropertyValue, var method: Boolean, var shorthand: Boolean) extends Node
    with ObjectExpressionProperty with ObjectPatternProperty {
    def `type` = Syntax.Property
    override def clone = copy().copyNode(this)
  }


  case class RegexLiteral(var value: RegExp, var raw: String, pattern: String, flags: String) extends Node with Expression {
    def `type` = Syntax.Literal
    override def clone = copy().copyNode(this)
  }


  /* ported: added ArgumentListElement because of parseGroupExpression */
  case class RestElement(var argument: BindingIdentifierOrPattern) extends Node with ArrayPatternElement with ObjectPatternProperty with ArgumentListElement {
    def `type` = Syntax.RestElement
    override def clone = copy().copyNode(this)
  }


  case class ReturnStatement(var argument: Expression) extends Node with Statement {
    def `type` = Syntax.ReturnStatement
    override def clone = copy().copyNode(this)
  }


  class Script(body: Seq[StatementListItem]) extends Program(body) {
    def sourceType: String = "script"
    override def clone = new Script(body).copyNode(this)
  }

  case class SequenceExpression(var expressions: Seq[Expression]) extends Node with Expression {
    def `type` = Syntax.SequenceExpression
    override def clone = copy().copyNode(this)
  }


  case class SpreadElement(var argument: Expression) extends Node with ArgumentListElement with ArrayExpressionElement with ObjectExpressionProperty {
    def `type` = Syntax.SpreadElement
    override def clone = copy().copyNode(this)
  }


  case class StaticMemberExpression(var `object`: Expression, var property: Expression) extends Node with Expression with ArrayPatternElement {
    def `type` = Syntax.MemberExpression
    var computed: Boolean = false
    override def clone = copy().copyNode(this)
  }


  case class Super() extends Node with Expression {
    def `type` = Syntax.Super
    override def clone = Super().copyNode(this)
  }


  case class SwitchCase(var test: Expression, var consequent: Seq[Statement]) extends Node {
    def `type` = Syntax.SwitchCase
    override def clone = copy().copyNode(this)
  }


  case class SwitchStatement(var discriminant: Expression, var cases: Seq[SwitchCase]) extends Node with Statement {
    def `type` = Syntax.SwitchStatement
    override def clone = copy().copyNode(this)
  }


  case class TaggedTemplateExpression(var tag: Expression, var quasi: TemplateLiteral) extends Node with Expression {
    def `type` = Syntax.TaggedTemplateExpression
    override def clone = copy().copyNode(this)
  }


  trait TemplateElementValue {
    def cooked: String
    def raw: String
  }

  case class TemplateElement(var value: TemplateElementValue, var tail: Boolean) extends Node {
    def `type`: String = Syntax.TemplateElement
    override def clone = copy().copyNode(this)
  }


  /* ported: added Expression because of parsePrimaryExpression */
  case class TemplateLiteral(var quasis: Seq[TemplateElement], var expressions: Seq[Expression]) extends Node with Expression {
    def `type` = Syntax.TemplateLiteral
    override def clone = copy().copyNode(this)
  }


  case class ThisExpression() extends Node with Expression {
    def `type` = Syntax.ThisExpression
    override def clone = ThisExpression().copyNode(this)
  }


  case class ThrowStatement(var argument: Expression) extends Node with Statement {
    def `type` = Syntax.ThrowStatement
    override def clone = copy().copyNode(this)
  }


  case class TryStatement(var block: BlockStatement, var handler: CatchClause, var finalizer: BlockStatement) extends Node with Statement {
    def `type` = Syntax.TryStatement
    override def clone = copy().copyNode(this)
  }


  case class UnaryExpression(var operator: String, var argument: Expression) extends Node with Expression {
    def `type` = Syntax.UnaryExpression
    var prefix: Boolean = true
    override def clone = copy().copyNode(this)
  }


  case class UpdateExpression(var operator: String, var argument: Expression, var prefix: Boolean) extends Node with Expression {
    def `type` = Syntax.UpdateExpression
    override def clone = copy().copyNode(this)
  }


  case class VariableDeclaration(var declarations: Seq[VariableDeclarator], var kind: String) extends Node
    with Declaration with ExportableNamedDeclaration with Statement with ExportableDefaultDeclaration {
    def `type` = Syntax.VariableDeclaration
    override def clone = copy().copyNode(this)
  }


  case class VariableDeclarator(var id: BindingIdentifierOrPattern, var init: Expression) extends Node with SymbolDeclaration {
    def `type` = Syntax.VariableDeclarator
    override def clone = copy().copyNode(this)
  }


  case class WhileStatement(var test: Expression, var body: Statement) extends Node with Statement {
    def `type` = Syntax.WhileStatement
    override def clone = copy().copyNode(this)
  }


  case class WithStatement(var `object`: Expression, var body: Statement) extends Node with Statement {
    def `type` = Syntax.WithStatement
    override def clone = copy().copyNode(this)
  }


  case class YieldExpression(var argument: Expression, var delegate: Boolean) extends Node with Expression {
    def `type` = Syntax.YieldExpression
    override def clone = copy().copyNode(this)
  }


}
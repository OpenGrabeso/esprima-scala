/*
ScalaFromJS: 2017-12-06 21:28:23.723
nodes.js
*/

package com.github.opengrabeso.esprima

import port.RegExp

import scala.collection.mutable.ArrayBuffer

object Node {

  def outputSeq(name: String, body: collection.Seq[Node.Node]): String = {
    name +  "{" + body.mkString("\n", "\n", "\n") + "}\n"
  }

  trait Node {
    def simpleName: String = {
      // getSimpleName is nice, but sometimes throws InternalError("Malformed class name")
      val pos = Option(range).fold("")(_.toString)
      getClass.getSimpleName + "-" + System.identityHashCode(this).toString
    }

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

  trait ExpressionOrStatement extends Node
  trait Expression extends Node with ArgumentListElement with ArrayExpressionElement with ChainExpressionValue with ExpressionOrStatement with PropertyKey
    with ExportableDefaultDeclaration with ExpressionOrImport with BlockStatementOrExpression with PropertyValue
  /* ArrayExpression | ArrowFunctionExpression | AssignmentExpression | AsyncArrowFunctionExpression | AsyncFunctionExpression |
    AwaitExpression | BinaryExpression | CallExpression | ClassExpression | ComputedMemberExpression |
    ConditionalExpression | Identifier | FunctionExpression | Literal | NewExpression | ObjectExpression |
    RegexLiteral | SequenceExpression | StaticMemberExpression | TaggedTemplateExpression |
    ThisExpression | UnaryExpression | UpdateExpression | YieldExpression; */
  trait ChainExpressionValue extends Node
  trait ArgumentListElement extends Node // Expression | SpreadElement;
  trait ArrayExpressionElement extends Node // Expression | SpreadElement | null;
  trait BindingPattern extends Node with ArrayPatternElement with ExportableDefaultDeclaration with FunctionParameter with PropertyValue with BindingIdentifierOrPattern // ArrayPattern | ObjectPattern
  trait BindingIdentifier extends Node with ArrayPatternElement with ExportableDefaultDeclaration with FunctionParameter with PropertyValue with BindingIdentifierOrPattern // Identifier;
  trait ArrayPatternElement extends Node with BindingIdentifierOrPattern // AssignmentPattern | BindingIdentifier | BindingPattern | RestElement | null;
  trait ExportDeclaration extends Node with Declaration // ExportAllDeclaration | ExportDefaultDeclaration | ExportNamedDeclaration;
  trait Declaration extends Node with StatementListItem // = AsyncFunctionDeclaration | ClassDeclaration | ExportDeclaration | FunctionDeclaration | ImportDeclaration | VariableDeclaration;
  trait ExportableDefaultDeclaration extends Node // BindingIdentifier | BindingPattern | ClassDeclaration | Expression | FunctionDeclaration;
  trait ExportableNamedDeclaration extends Node // AsyncFunctionDeclaration | ClassDeclaration | FunctionDeclaration | VariableDeclaration;
  trait FunctionParameter extends Node // = AssignmentPattern | BindingIdentifier | BindingPattern;
  trait ImportDeclarationSpecifier extends Node //= ImportDefaultSpecifier | ImportNamespaceSpecifier | ImportSpecifier;
  trait ObjectExpressionProperty extends Node //= Property | SpreadElement;
  // ported: made alias, so that reinterpretExpressionAsPattern works with the type
  type ObjectPatternProperty = ArrayPatternElement // extends Node with BindingIdentifierOrPattern //= Property | RestElement;
  trait Statement extends Node with StatementListItem with ExpressionOrStatement
  /*= AsyncFunctionDeclaration | BreakStatement | ContinueStatement | DebuggerStatement | DoWhileStatement |
    EmptyStatement | ExpressionStatement | Directive | ForStatement | ForInStatement | ForOfStatement |
    FunctionDeclaration | IfStatement | ReturnStatement | SwitchStatement | ThrowStatement |
    TryStatement | VariableDeclaration | WhileStatement | WithStatement;*/
  trait PropertyKey extends Node //= Identifier | Literal;
  trait PropertyValue extends Node //= AssignmentPattern | AsyncFunctionExpression | BindingIdentifier | BindingPattern | FunctionExpression;
  trait StatementListItem extends Node with ExportableNamedDeclaration //= Declaration | Statement;

  trait BindingIdentifierOrPattern extends FunctionParameter // BindingIdentifier | BindingPattern
  trait TypeAnnotation extends Node

  trait ExpressionOrImport extends Node // Expression | Import

  trait BlockStatementOrExpression extends Node // BlockStatement | Expression

  trait ClassBodyElement extends Node

  val ArrowParameterPlaceHolder = "ArrowParameterPlaceHolder"

  class ArrowParameterPlaceHolder extends Node with Expression with FunctionParameter {
    override def toString = simpleName

    var params: collection.Seq[ArgumentListElement] = _
    var async: Boolean = _
  }


  trait HasGenerator {
    def generator: Boolean
  }

  abstract class CommentNode extends Node {
    var value: String = _
    def multiline: Boolean
  }

  case class ArrayExpression(var elements: collection.Seq[ArrayExpressionElement]) extends Node with Expression {
    override def clone = copy().copyNode(this)

  }

  // TypeScript grammar: https://github.com/rbuckton/grammarkdown/blob/master/spec/typescript.grammar
  case class TypeName(parent: collection.Seq[Identifier]) extends TypeAnnotation {
    override def clone = copy().copyNode(this)
  }

  case class LiteralType(t: Literal) extends TypeAnnotation {
    override def clone = copy().copyNode(this)
  }

  case class ArrayType(t: TypeAnnotation) extends TypeAnnotation {
    override def clone = copy().copyNode(this)
  }

  case class IndexedAccessType(t: TypeAnnotation, index: String) extends TypeAnnotation {
    override def clone = copy().copyNode(this)
  }

  case class UnionType(left: TypeAnnotation, right: TypeAnnotation) extends TypeAnnotation {
    override def clone = copy().copyNode(this)
  }

  case class ConditionalType(tpe: TypeAnnotation, ext: TypeAnnotation, left: TypeAnnotation, right: TypeAnnotation) extends TypeAnnotation {
    override def clone = copy().copyNode(this)
  }

  case class IntersectionType(left: TypeAnnotation, right: TypeAnnotation) extends TypeAnnotation {
    override def clone = copy().copyNode(this)
  }

  case class TypeMember(name: Identifier, optional: Boolean, readOnly: Boolean, `type`: TypeAnnotation) extends TypeAnnotation {
    override def clone = copy().copyNode(this)
  }

  case class ObjectType(body: collection.Seq[TypeMember]) extends TypeAnnotation {
    override def clone = copy().copyNode(this)
  }

  case class FunctionType(var params: collection.Seq[FunctionParameter], var returnType: TypeAnnotation) extends TypeAnnotation {
    override def clone = copy().copyNode(this)
  }

  case class TypeReference(t: TypeName, arg: collection.Seq[TypeAnnotation]) extends TypeAnnotation {
    override def clone = copy().copyNode(this)
  }

  case class TupleType(t: collection.Seq[TypeAnnotation]) extends TypeAnnotation {
    override def clone = copy().copyNode(this)
  }

  case class ArrayPattern(var elements: collection.Seq[ArrayPatternElement]) extends Node with BindingPattern with ArgumentListElement with Expression {
    override def clone = copy().copyNode(this)
  }


  case class ArrowFunctionExpression(var params: collection.Seq[FunctionParameter], var body: BlockStatementOrExpression, var expression: Boolean) extends Node with HasGenerator with Expression {
    override def clone = copy().copyNode(this)

    var id: Identifier = null
    var generator: Boolean = false
    var async: Boolean = false
  }


  case class AssignmentExpression(var operator: String, var left: Expression, var right: Expression) extends Node with Expression {
    override def clone = copy().copyNode(this)

  }

  /* ported: added ObjectPatternProperty because of reinterpretExpressionAsObjectPattern */
  case class AssignmentPattern(var left: BindingIdentifierOrPattern, var right: Expression) extends Node
    with ArrayPatternElement with ArgumentListElement with FunctionParameter with PropertyValue /*with ObjectPatternProperty*/ {
    override def clone = copy().copyNode(this)

  }


  case class AsyncArrowFunctionExpression(var params: collection.Seq[FunctionParameter], var body: BlockStatementOrExpression, var expression: Boolean) extends Node
    with HasGenerator with Expression {
    override def clone = copy().copyNode(this)

    var id: Identifier = null
    var generator: Boolean = false
    var async: Boolean = true
  }

  /* ported: added StatementListItem because of parseFunctionDeclaration, Statement because of parseLabelledStatement */
  trait AFunctionDeclaration extends Node with HasGenerator with StatementListItem with Statement with ExportableNamedDeclaration with ExportableDefaultDeclaration

  case class AsyncFunctionDeclaration(var id: Identifier, var params: collection.Seq[FunctionParameter], var body: BlockStatement, var isGenerator: Boolean) extends Node
    with AFunctionDeclaration with Declaration with ExportableNamedDeclaration with Statement {
    override def clone = copy().copyNode(this)

    var generator: Boolean = false
    var expression: Boolean = false
    var async: Boolean = true
  }


  case class AsyncFunctionExpression(var id: Identifier, var params: collection.Seq[FunctionParameter], var body: BlockStatement, var isGenerator: Boolean) extends Node
    with HasGenerator with Expression with PropertyValue {

    override def clone = copy().copyNode(this)
    var generator: Boolean = isGenerator
    var expression: Boolean = false
    var async: Boolean = true
  }


  case class AwaitExpression(var argument: Expression) extends Node with Expression {

    override def clone = copy().copyNode(this)
  }


  case class BinaryExpression(var operator: String, var left: Expression, var right: Expression) extends Node with Expression {
    //private val logical = operator == "||" || operator == "&&" || operator == "??"
    //def `type` = if (logical) Syntax.LogicalExpression else Syntax.BinaryExpression
    override def clone = copy().copyNode(this)
  }


  /* ported: type adjusted */
  case class BlockStatement(var body: collection.Seq[StatementListItem]) extends Node with Statement with BlockStatementOrExpression {

    override def clone = copy().copyNode(this)
  }


  case class BreakStatement(var label: Identifier) extends Node with Statement {

    override def clone = copy().copyNode(this)
  }


  case class CallExpression(var callee: ExpressionOrImport, var arguments: collection.Seq[ArgumentListElement]) extends Node with Expression with ChainExpressionValue {

    override def clone = copy().copyNode(this)
  }

  case class EnumBodyElement(var name: Identifier, var value: Expression) extends Node {
    override def clone = copy().copyNode(this)
  }
  case class EnumBody(var body: collection.Seq[EnumBodyElement]) extends Node {
    override def clone = copy().copyNode(this)
  }
  case class EnumDeclaration(var name: Identifier, var body: EnumBody) extends Node with Declaration {
    override def clone = copy().copyNode(this)
  }


  case class CatchClause(var param: BindingIdentifierOrPattern, var body: BlockStatement) extends Node {

    override def clone = copy().copyNode(this)
  }

  // actually MethodDefinition seems to work
  case class ClassBody(var body: collection.Seq[ClassBodyElement]) extends Node {
    override def toString = Node.outputSeq("ClassBody", body)

    override def clone = copy().copyNode(this)
  }

  case class ChainExpression(var expression: ChainExpressionValue) extends Node with Expression {
    override def clone = copy().copyNode(this)
  }

  case class TypeParameterListItem(var name: Node.Identifier, var constraint: Node.TypeAnnotation, var defaultValue: Node.TypeAnnotation) extends Node {
    override def clone = copy().copyNode(this)
  }

  // each pair specifies T extends X, X may be null
  case class TypeParameterList(var types: collection.Seq[TypeParameterListItem]) extends Node {
    override def clone = copy().copyNode(this)
  }

  // ported: added Statement because of parseLabelledStatement
  case class ClassDeclarationEx(var id: Identifier, var typeParameters: TypeParameterList, var superClass: Expression, var implements: collection.Seq[Identifier], var body: ClassBody, kind: String) extends Node
    with Declaration with ExportableDefaultDeclaration with ExportableNamedDeclaration with Statement {

    override def clone = copy().copyNode(this)
  }
  type ClassDeclaration = ClassDeclarationEx
  object ClassDeclaration {
    def apply(id: Identifier, superClass: Expression, implements: Seq[Identifier], body: ClassBody, kind: String): ClassDeclarationEx = {
      new ClassDeclarationEx(id, null, superClass, implements, body, kind)
    }
    def unapply(arg: ClassDeclarationEx) = {
      ClassDeclarationEx.unapply(arg).map { r =>
        (r._1, r._3, r._4, r._5, r._6)
      }
    }
  }

  case class NamespaceBody(var body: collection.Seq[Declaration]) extends Node
    with Declaration with ExportableDefaultDeclaration with ExportableNamedDeclaration with Statement {

    override def clone = copy().copyNode(this)
  }

  case class NamespaceDeclaration(var id: Identifier, var body: NamespaceBody) extends Node
    with Declaration with ExportableDefaultDeclaration with ExportableNamedDeclaration with Statement {

    override def clone = copy().copyNode(this)
  }

  case class TypeAliasDeclaration(id: Identifier, tpe: TypeAnnotation) extends Node with Declaration with Statement {
    override def clone = copy().copyNode(this)
  }

  case class ClassExpression(var id: Identifier, var superClass: Identifier, var body: ClassBody) extends Node with Expression {

    override def clone = copy().copyNode(this)
  }


  /* ported: added ArrayPatternElement because of reinterpretExpressionAsArrayPattern */
  case class ComputedMemberExpression(var `object`: Expression, var property: Expression, var optional: Boolean) extends Node with Expression with ArrayPatternElement with ChainExpressionValue {

    var computed: Boolean = true
    override def clone = copy().copyNode(this)
  }


  case class ConditionalExpression(var test: Expression, var consequent: Expression, var alternate: Expression) extends Node with Expression {

    override def clone = copy().copyNode(this)
  }


  case class ContinueStatement(var label: Identifier) extends Node with Statement {

    override def clone = copy().copyNode(this)
  }


  case class DebuggerStatement() extends Node with Statement {

    override def clone = DebuggerStatement().copyNode(this)
  }


  case class Directive(var expression: Expression, var directive: String) extends Node with Statement {

    override def clone = copy().copyNode(this)
  }


  case class DoWhileStatement(var body: Statement, var test: Expression) extends Node with Statement {

    override def clone = copy().copyNode(this)
  }


  case class EmptyStatement() extends Node with Statement {

    override def clone = EmptyStatement().copyNode(this)
  }


  case class ExportAllDeclaration(var source: Literal) extends Node with ExportDeclaration {

    override def clone = copy().copyNode(this)
  }


  case class ExportDefaultDeclaration(var declaration: ExportableDefaultDeclaration) extends Node with ExportDeclaration {

    override def clone = copy().copyNode(this)
  }


  case class ExportNamedDeclaration(var declaration: ExportableNamedDeclaration, var specifiers: collection.Seq[ExportSpecifier], var source: Literal) extends Node with ExportDeclaration {

    override def clone = copy().copyNode(this)
  }


  case class ExportSpecifier(var local: Identifier, var exported: Identifier) extends Node {

    override def clone = copy().copyNode(this)
  }


  case class ExpressionStatement(var expression: Expression) extends Node with Statement {

    override def clone = copy().copyNode(this)
  }


  case class ForInStatement(var left: ExpressionOrStatement, var right: Expression, var body: Statement) extends Node with Statement {

    var each: Boolean = false
    override def clone = copy().copyNode(this)
  }


  case class ForOfStatement(var left: ExpressionOrStatement, var right: Expression, var body: Statement, var await: Boolean) extends Node with Statement {

    override def clone = copy().copyNode(this)
  }


  case class ForStatement(var init: ExpressionOrStatement, var test: Expression, var update: Expression, var body: Statement) extends Node with Statement {

    override def clone = copy().copyNode(this)
  }


  case class FunctionDeclaration(
    var id: Identifier, var params: collection.Seq[FunctionParameter], var body: BlockStatement, var generator: Boolean,
    var ret: TypeAnnotation
  ) extends Node
    with AFunctionDeclaration with Declaration with ExportableDefaultDeclaration with ExportableNamedDeclaration with Statement {

    override def clone = copy().copyNode(this)
    var expression: Boolean = false
    var async: Boolean = false
  }


  case class FunctionExpression(
    var id: Identifier, var params: collection.Seq[FunctionParameter], var body: BlockStatement, var generator: Boolean,
    var ret: TypeAnnotation
  ) extends Node
    with HasGenerator with Expression with PropertyValue {

    override def clone = copy().copyNode(this)
    var expression: Boolean = false
    var async: Boolean = false
  }


  case class Identifier(var name: String) extends Node with Expression with BindingIdentifier with PropertyKey {

    override def clone = copy().copyNode(this)
  }


  case class IfStatement(var test: Expression, var consequent: Statement, var alternate: Statement) extends Node with Statement {

    override def clone = copy().copyNode(this)
  }


  case class Import() extends Node with ExpressionOrImport {

    override def clone = Import().copyNode(this)
  }


  case class ImportDeclaration(var specifiers: collection.Seq[ImportDeclarationSpecifier], var source: Literal) extends Node with Declaration {

    override def clone = copy().copyNode(this)
  }


  case class ImportDefaultSpecifier(var local: Identifier) extends Node with ImportDeclarationSpecifier {

    override def clone = copy().copyNode(this)
  }


  case class ImportNamespaceSpecifier(var local: Identifier) extends Node with ImportDeclarationSpecifier {

    override def clone = copy().copyNode(this)
  }


  case class ImportSpecifier(var local: Identifier, var imported: Identifier) extends Node with ImportDeclarationSpecifier {

    override def clone = copy().copyNode(this)
  }


  case class LabeledStatement(var label: Identifier, var body: Statement) extends Node with Statement {

    override def clone = copy().copyNode(this)
  }


  case class Literal(var value: OrType, var raw: String) extends Node with Expression with PropertyKey {

    override def clone = copy().copyNode(this)
  }


  /* ported: added Expression because of parseNewExpression */
  case class MetaProperty(var meta: Identifier, var property: Identifier) extends Node with Expression {

    override def clone = copy().copyNode(this)
  }

  trait AFunctionExpression extends Node // AsyncFunctionExpression | FunctionExpression

  case class MethodDefinitionEx(
    var key: PropertyKey, var typePars: TypeParameterList, var `type`: TypeAnnotation, var computed: Boolean, var value: PropertyValue,
    var kind: String, var static: Boolean, var optional: Boolean, var readOnly: Boolean
  ) extends Node with ClassBodyElement {
    override def clone = copy().copyNode(this)
  }

  type MethodDefinition = MethodDefinitionEx
  object MethodDefinition {
    def apply(key: PropertyKey, `type`: TypeAnnotation, computed: Boolean, value: PropertyValue, kind: String, static: Boolean): MethodDefinitionEx = {
      new MethodDefinitionEx(key, null, `type`, computed, value, kind, static, false, false)
    }
    def unapply(arg: MethodDefinitionEx) = {
      MethodDefinitionEx.unapply(arg).map(r => (r._1, r._3, r._4, r._5, r._6, r._7))
    }
  }

  abstract class Program(var body: collection.Seq[StatementListItem]) extends Node {

    override def toString = Node.outputSeq(sourceType, body)

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
    def unapply(arg: Program): Option[collection.Seq[StatementListItem]] = Some(arg.body)
  }

  class Module(bodyInit: collection.Seq[StatementListItem]) extends Program(bodyInit) {
    def sourceType: String = "module"
    override def clone = new Module(body).copyNode(this)
  }

  case class NewExpressionEx(var callee: Expression, var typeArgs: collection.Seq[Node.TypeAnnotation], var arguments: collection.Seq[ArgumentListElement]) extends Node with Expression {
    override def clone = copy().copyNode(this)
  }

  type NewExpression = NewExpressionEx
  object NewExpression {
    def apply(callee: Expression, arguments: collection.Seq[ArgumentListElement]) = new NewExpressionEx(callee, null, arguments)
    def unapply(arg: NewExpressionEx) = NewExpressionEx.unapply(arg).map(x => (x._1, x._3))
  }

  case class ObjectExpression(var properties: collection.Seq[ObjectExpressionProperty]) extends Node with Expression {

    override def clone = copy().copyNode(this)
  }


  /* ported: added ObjectPatternProperty because of reinterpretExpressionAsObjectPattern */
  case class ObjectPattern(var properties: collection.Seq[ObjectPatternProperty]) extends Node with BindingPattern with ObjectPatternProperty with ArgumentListElement {

    override def clone = copy().copyNode(this)
  }

  case class FunctionParameterWithType(name: BindingIdentifierOrPattern, `type`: TypeAnnotation, defValue: Expression, optional: Boolean) extends Node
    with FunctionParameter {

    override def clone = copy().copyNode(this)
  }

  case class PropertyEx(
    var kind: String, var key: PropertyKey, var computed: Boolean, var value: PropertyValue, var method: Boolean,
    var shorthand: Boolean, var readonly: Boolean
  ) extends Node
    with ObjectExpressionProperty with ObjectPatternProperty {

    override def clone = copy().copyNode(this)
  }

  type Property = PropertyEx
  object Property {
    def apply(kind: String, key: PropertyKey, computed: Boolean, value: PropertyValue, method: Boolean, shorthand: Boolean) = {
      new PropertyEx(kind, key, computed, value, method, shorthand, false)
    }
    def unapply(arg: PropertyEx) = PropertyEx.unapply(arg).map(x => (x._1, x._2, x._3, x._4, x._5, x._6))
  }

  case class RegexLiteral(var value: RegExp, var raw: String, pattern: String, flags: String) extends Node with Expression {

    override def clone = copy().copyNode(this)
  }


  /* ported: added ArgumentListElement because of parseGroupExpression */
  /* ported: added FunctionParameter because of parseFormalParameter */
  case class RestElement(var argument: BindingIdentifierOrPattern, var `type`: TypeAnnotation) extends Node with ArrayPatternElement /*with ObjectPatternProperty*/ with ArgumentListElement with FunctionParameter {

    override def clone = copy().copyNode(this)
  }


  case class ReturnStatement(var argument: Expression) extends Node with Statement {

    override def clone = copy().copyNode(this)
  }


  class Script(bodyInit: collection.Seq[StatementListItem]) extends Program(bodyInit) {
    def sourceType: String = "script"
    override def clone = new Script(body).copyNode(this)
  }

  case class SequenceExpression(var expressions: collection.Seq[Expression]) extends Node with Expression {

    override def clone = copy().copyNode(this)
  }


  case class SpreadElement(var argument: Expression) extends Node with ArgumentListElement with ArrayExpressionElement with ObjectExpressionProperty {

    override def clone = copy().copyNode(this)
  }


  case class StaticMemberExpression(var `object`: Expression, var property: Expression, var optional: Boolean)
    extends Node with Expression with ArrayPatternElement with ChainExpressionValue {

    var computed: Boolean = false
    override def clone = copy().copyNode(this)
  }


  case class Super() extends Node with Expression {

    override def clone = Super().copyNode(this)
  }


  case class SwitchCase(var test: Expression, var consequent: collection.Seq[Statement]) extends Node {

    override def clone = copy().copyNode(this)
  }


  case class SwitchStatement(var discriminant: Expression, var cases: collection.Seq[SwitchCase]) extends Node with Statement {

    override def clone = copy().copyNode(this)
  }


  case class TaggedTemplateExpression(var tag: Expression, var quasi: TemplateLiteral) extends Node with Expression {

    override def clone = copy().copyNode(this)
  }


  trait TemplateElementValue {
    def cooked: String
    def raw: String
  }

  case class TemplateElement(var value: TemplateElementValue, var tail: Boolean) extends Node {

    override def clone = copy().copyNode(this)
  }


  /* ported: added Expression because of parsePrimaryExpression */
  case class TemplateLiteral(var quasis: collection.Seq[TemplateElement], var expressions: collection.Seq[Expression]) extends Node with Expression {

    override def clone = copy().copyNode(this)
  }


  case class ThisExpression() extends Node with Expression {

    override def clone = ThisExpression().copyNode(this)
  }


  case class ThrowStatement(var argument: Expression) extends Node with Statement {

    override def clone = copy().copyNode(this)
  }


  case class TryStatement(var block: BlockStatement, var handler: CatchClause, var finalizer: BlockStatement) extends Node with Statement {

    override def clone = copy().copyNode(this)
  }


  case class UnaryExpression(var operator: String, var argument: Expression) extends Node with Expression {

    var prefix: Boolean = true
    override def clone = copy().copyNode(this)
  }


  case class UpdateExpression(var operator: String, var argument: Expression, var prefix: Boolean) extends Node with Expression {

    override def clone = copy().copyNode(this)
  }


  case class VariableDeclaration(var declarations: collection.Seq[VariableDeclarator], var kind: String) extends Node
    with Declaration with ExportableNamedDeclaration with Statement with ExportableDefaultDeclaration {

    override def clone = copy().copyNode(this)
  }


  case class VariableDeclarator(var id: BindingIdentifierOrPattern, var init: Expression, var `type`: TypeAnnotation) extends Node {

    override def clone = copy().copyNode(this)
  }


  case class WhileStatement(var test: Expression, var body: Statement) extends Node with Statement {

    override def clone = copy().copyNode(this)
  }


  case class WithStatement(var `object`: Expression, var body: Statement) extends Node with Statement {

    override def clone = copy().copyNode(this)
  }


  case class YieldExpression(var argument: Expression, var delegate: Boolean) extends Node with Expression {

    override def clone = copy().copyNode(this)
  }


}
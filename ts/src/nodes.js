"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const syntax_1 = require("./syntax");
/* tslint:disable:max-classes-per-file */
class ArrayExpression {
    constructor(elements) {
        this.type = syntax_1.Syntax.ArrayExpression;
        this.elements = elements;
    }
}
exports.ArrayExpression = ArrayExpression;
class ArrayPattern {
    constructor(elements) {
        this.type = syntax_1.Syntax.ArrayPattern;
        this.elements = elements;
    }
}
exports.ArrayPattern = ArrayPattern;
class ArrowFunctionExpression {
    constructor(params, body, expression) {
        this.type = syntax_1.Syntax.ArrowFunctionExpression;
        this.id = null;
        this.params = params;
        this.body = body;
        this.generator = false;
        this.expression = expression;
        this.async = false;
    }
}
exports.ArrowFunctionExpression = ArrowFunctionExpression;
class AssignmentExpression {
    constructor(operator, left, right) {
        this.type = syntax_1.Syntax.AssignmentExpression;
        this.operator = operator;
        this.left = left;
        this.right = right;
    }
}
exports.AssignmentExpression = AssignmentExpression;
class AssignmentPattern {
    constructor(left, right) {
        this.type = syntax_1.Syntax.AssignmentPattern;
        this.left = left;
        this.right = right;
    }
}
exports.AssignmentPattern = AssignmentPattern;
class AsyncArrowFunctionExpression {
    constructor(params, body, expression) {
        this.type = syntax_1.Syntax.ArrowFunctionExpression;
        this.id = null;
        this.params = params;
        this.body = body;
        this.generator = false;
        this.expression = expression;
        this.async = true;
    }
}
exports.AsyncArrowFunctionExpression = AsyncArrowFunctionExpression;
class AsyncFunctionDeclaration {
    constructor(id, params, body) {
        this.type = syntax_1.Syntax.FunctionDeclaration;
        this.id = id;
        this.params = params;
        this.body = body;
        this.generator = false;
        this.expression = false;
        this.async = true;
    }
}
exports.AsyncFunctionDeclaration = AsyncFunctionDeclaration;
class AsyncFunctionExpression {
    constructor(id, params, body) {
        this.type = syntax_1.Syntax.FunctionExpression;
        this.id = id;
        this.params = params;
        this.body = body;
        this.generator = false;
        this.expression = false;
        this.async = true;
    }
}
exports.AsyncFunctionExpression = AsyncFunctionExpression;
class AwaitExpression {
    constructor(argument) {
        this.type = syntax_1.Syntax.AwaitExpression;
        this.argument = argument;
    }
}
exports.AwaitExpression = AwaitExpression;
class BinaryExpression {
    constructor(operator, left, right) {
        const logical = (operator === '||' || operator === '&&');
        this.type = logical ? syntax_1.Syntax.LogicalExpression : syntax_1.Syntax.BinaryExpression;
        this.operator = operator;
        this.left = left;
        this.right = right;
    }
}
exports.BinaryExpression = BinaryExpression;
class BlockStatement {
    constructor(body) {
        this.type = syntax_1.Syntax.BlockStatement;
        this.body = body;
    }
}
exports.BlockStatement = BlockStatement;
class BreakStatement {
    constructor(label) {
        this.type = syntax_1.Syntax.BreakStatement;
        this.label = label;
    }
}
exports.BreakStatement = BreakStatement;
class CallExpression {
    constructor(callee, args) {
        this.type = syntax_1.Syntax.CallExpression;
        this.callee = callee;
        this.arguments = args;
    }
}
exports.CallExpression = CallExpression;
class CatchClause {
    constructor(param, body) {
        this.type = syntax_1.Syntax.CatchClause;
        this.param = param;
        this.body = body;
    }
}
exports.CatchClause = CatchClause;
class ClassBody {
    constructor(body) {
        this.type = syntax_1.Syntax.ClassBody;
        this.body = body;
    }
}
exports.ClassBody = ClassBody;
class ClassDeclaration {
    constructor(id, superClass, body) {
        this.type = syntax_1.Syntax.ClassDeclaration;
        this.id = id;
        this.superClass = superClass;
        this.body = body;
    }
}
exports.ClassDeclaration = ClassDeclaration;
class ClassExpression {
    constructor(id, superClass, body) {
        this.type = syntax_1.Syntax.ClassExpression;
        this.id = id;
        this.superClass = superClass;
        this.body = body;
    }
}
exports.ClassExpression = ClassExpression;
class ComputedMemberExpression {
    constructor(object, property) {
        this.type = syntax_1.Syntax.MemberExpression;
        this.computed = true;
        this.object = object;
        this.property = property;
    }
}
exports.ComputedMemberExpression = ComputedMemberExpression;
class ConditionalExpression {
    constructor(test, consequent, alternate) {
        this.type = syntax_1.Syntax.ConditionalExpression;
        this.test = test;
        this.consequent = consequent;
        this.alternate = alternate;
    }
}
exports.ConditionalExpression = ConditionalExpression;
class ContinueStatement {
    constructor(label) {
        this.type = syntax_1.Syntax.ContinueStatement;
        this.label = label;
    }
}
exports.ContinueStatement = ContinueStatement;
class DebuggerStatement {
    constructor() {
        this.type = syntax_1.Syntax.DebuggerStatement;
    }
}
exports.DebuggerStatement = DebuggerStatement;
class Directive {
    constructor(expression, directive) {
        this.type = syntax_1.Syntax.ExpressionStatement;
        this.expression = expression;
        this.directive = directive;
    }
}
exports.Directive = Directive;
class DoWhileStatement {
    constructor(body, test) {
        this.type = syntax_1.Syntax.DoWhileStatement;
        this.body = body;
        this.test = test;
    }
}
exports.DoWhileStatement = DoWhileStatement;
class EmptyStatement {
    constructor() {
        this.type = syntax_1.Syntax.EmptyStatement;
    }
}
exports.EmptyStatement = EmptyStatement;
class ExportAllDeclaration {
    constructor(source) {
        this.type = syntax_1.Syntax.ExportAllDeclaration;
        this.source = source;
    }
}
exports.ExportAllDeclaration = ExportAllDeclaration;
class ExportDefaultDeclaration {
    constructor(declaration) {
        this.type = syntax_1.Syntax.ExportDefaultDeclaration;
        this.declaration = declaration;
    }
}
exports.ExportDefaultDeclaration = ExportDefaultDeclaration;
class ExportNamedDeclaration {
    constructor(declaration, specifiers, source) {
        this.type = syntax_1.Syntax.ExportNamedDeclaration;
        this.declaration = declaration;
        this.specifiers = specifiers;
        this.source = source;
    }
}
exports.ExportNamedDeclaration = ExportNamedDeclaration;
class ExportSpecifier {
    constructor(local, exported) {
        this.type = syntax_1.Syntax.ExportSpecifier;
        this.exported = exported;
        this.local = local;
    }
}
exports.ExportSpecifier = ExportSpecifier;
class ExpressionStatement {
    constructor(expression) {
        this.type = syntax_1.Syntax.ExpressionStatement;
        this.expression = expression;
    }
}
exports.ExpressionStatement = ExpressionStatement;
class ForInStatement {
    constructor(left, right, body) {
        this.type = syntax_1.Syntax.ForInStatement;
        this.left = left;
        this.right = right;
        this.body = body;
        this.each = false;
    }
}
exports.ForInStatement = ForInStatement;
class ForOfStatement {
    constructor(left, right, body) {
        this.type = syntax_1.Syntax.ForOfStatement;
        this.left = left;
        this.right = right;
        this.body = body;
    }
}
exports.ForOfStatement = ForOfStatement;
class ForStatement {
    constructor(init, test, update, body) {
        this.type = syntax_1.Syntax.ForStatement;
        this.init = init;
        this.test = test;
        this.update = update;
        this.body = body;
    }
}
exports.ForStatement = ForStatement;
class FunctionDeclaration {
    constructor(id, params, body, generator) {
        this.type = syntax_1.Syntax.FunctionDeclaration;
        this.id = id;
        this.params = params;
        this.body = body;
        this.generator = generator;
        this.expression = false;
        this.async = false;
    }
}
exports.FunctionDeclaration = FunctionDeclaration;
class FunctionExpression {
    constructor(id, params, body, generator) {
        this.type = syntax_1.Syntax.FunctionExpression;
        this.id = id;
        this.params = params;
        this.body = body;
        this.generator = generator;
        this.expression = false;
        this.async = false;
    }
}
exports.FunctionExpression = FunctionExpression;
class Identifier {
    constructor(name) {
        this.type = syntax_1.Syntax.Identifier;
        this.name = name;
    }
}
exports.Identifier = Identifier;
class IfStatement {
    constructor(test, consequent, alternate) {
        this.type = syntax_1.Syntax.IfStatement;
        this.test = test;
        this.consequent = consequent;
        this.alternate = alternate;
    }
}
exports.IfStatement = IfStatement;
class Import {
    constructor() {
        this.type = syntax_1.Syntax.Import;
    }
}
exports.Import = Import;
class ImportDeclaration {
    constructor(specifiers, source) {
        this.type = syntax_1.Syntax.ImportDeclaration;
        this.specifiers = specifiers;
        this.source = source;
    }
}
exports.ImportDeclaration = ImportDeclaration;
class ImportDefaultSpecifier {
    constructor(local) {
        this.type = syntax_1.Syntax.ImportDefaultSpecifier;
        this.local = local;
    }
}
exports.ImportDefaultSpecifier = ImportDefaultSpecifier;
class ImportNamespaceSpecifier {
    constructor(local) {
        this.type = syntax_1.Syntax.ImportNamespaceSpecifier;
        this.local = local;
    }
}
exports.ImportNamespaceSpecifier = ImportNamespaceSpecifier;
class ImportSpecifier {
    constructor(local, imported) {
        this.type = syntax_1.Syntax.ImportSpecifier;
        this.local = local;
        this.imported = imported;
    }
}
exports.ImportSpecifier = ImportSpecifier;
class LabeledStatement {
    constructor(label, body) {
        this.type = syntax_1.Syntax.LabeledStatement;
        this.label = label;
        this.body = body;
    }
}
exports.LabeledStatement = LabeledStatement;
class Literal {
    constructor(value, raw) {
        this.type = syntax_1.Syntax.Literal;
        this.value = value;
        this.raw = raw;
    }
}
exports.Literal = Literal;
class MetaProperty {
    constructor(meta, property) {
        this.type = syntax_1.Syntax.MetaProperty;
        this.meta = meta;
        this.property = property;
    }
}
exports.MetaProperty = MetaProperty;
class MethodDefinition {
    constructor(key, computed, value, kind, isStatic) {
        this.type = syntax_1.Syntax.MethodDefinition;
        this.key = key;
        this.computed = computed;
        this.value = value;
        this.kind = kind;
        this.static = isStatic;
    }
}
exports.MethodDefinition = MethodDefinition;
class Module {
    constructor(body) {
        this.type = syntax_1.Syntax.Program;
        this.body = body;
        this.sourceType = 'module';
    }
}
exports.Module = Module;
class NewExpression {
    constructor(callee, args) {
        this.type = syntax_1.Syntax.NewExpression;
        this.callee = callee;
        this.arguments = args;
    }
}
exports.NewExpression = NewExpression;
class ObjectExpression {
    constructor(properties) {
        this.type = syntax_1.Syntax.ObjectExpression;
        this.properties = properties;
    }
}
exports.ObjectExpression = ObjectExpression;
class ObjectPattern {
    constructor(properties) {
        this.type = syntax_1.Syntax.ObjectPattern;
        this.properties = properties;
    }
}
exports.ObjectPattern = ObjectPattern;
class Property {
    constructor(kind, key, computed, value, method, shorthand) {
        this.type = syntax_1.Syntax.Property;
        this.key = key;
        this.computed = computed;
        this.value = value;
        this.kind = kind;
        this.method = method;
        this.shorthand = shorthand;
    }
}
exports.Property = Property;
class RegexLiteral {
    constructor(value, raw, pattern, flags) {
        this.type = syntax_1.Syntax.Literal;
        this.value = value;
        this.raw = raw;
        this.regex = { pattern, flags };
    }
}
exports.RegexLiteral = RegexLiteral;
class RestElement {
    constructor(argument) {
        this.type = syntax_1.Syntax.RestElement;
        this.argument = argument;
    }
}
exports.RestElement = RestElement;
class ReturnStatement {
    constructor(argument) {
        this.type = syntax_1.Syntax.ReturnStatement;
        this.argument = argument;
    }
}
exports.ReturnStatement = ReturnStatement;
class Script {
    constructor(body) {
        this.type = syntax_1.Syntax.Program;
        this.body = body;
        this.sourceType = 'script';
    }
}
exports.Script = Script;
class SequenceExpression {
    constructor(expressions) {
        this.type = syntax_1.Syntax.SequenceExpression;
        this.expressions = expressions;
    }
}
exports.SequenceExpression = SequenceExpression;
class SpreadElement {
    constructor(argument) {
        this.type = syntax_1.Syntax.SpreadElement;
        this.argument = argument;
    }
}
exports.SpreadElement = SpreadElement;
class StaticMemberExpression {
    constructor(object, property) {
        this.type = syntax_1.Syntax.MemberExpression;
        this.computed = false;
        this.object = object;
        this.property = property;
    }
}
exports.StaticMemberExpression = StaticMemberExpression;
class Super {
    constructor() {
        this.type = syntax_1.Syntax.Super;
    }
}
exports.Super = Super;
class SwitchCase {
    constructor(test, consequent) {
        this.type = syntax_1.Syntax.SwitchCase;
        this.test = test;
        this.consequent = consequent;
    }
}
exports.SwitchCase = SwitchCase;
class SwitchStatement {
    constructor(discriminant, cases) {
        this.type = syntax_1.Syntax.SwitchStatement;
        this.discriminant = discriminant;
        this.cases = cases;
    }
}
exports.SwitchStatement = SwitchStatement;
class TaggedTemplateExpression {
    constructor(tag, quasi) {
        this.type = syntax_1.Syntax.TaggedTemplateExpression;
        this.tag = tag;
        this.quasi = quasi;
    }
}
exports.TaggedTemplateExpression = TaggedTemplateExpression;
class TemplateElement {
    constructor(value, tail) {
        this.type = syntax_1.Syntax.TemplateElement;
        this.value = value;
        this.tail = tail;
    }
}
exports.TemplateElement = TemplateElement;
class TemplateLiteral {
    constructor(quasis, expressions) {
        this.type = syntax_1.Syntax.TemplateLiteral;
        this.quasis = quasis;
        this.expressions = expressions;
    }
}
exports.TemplateLiteral = TemplateLiteral;
class ThisExpression {
    constructor() {
        this.type = syntax_1.Syntax.ThisExpression;
    }
}
exports.ThisExpression = ThisExpression;
class ThrowStatement {
    constructor(argument) {
        this.type = syntax_1.Syntax.ThrowStatement;
        this.argument = argument;
    }
}
exports.ThrowStatement = ThrowStatement;
class TryStatement {
    constructor(block, handler, finalizer) {
        this.type = syntax_1.Syntax.TryStatement;
        this.block = block;
        this.handler = handler;
        this.finalizer = finalizer;
    }
}
exports.TryStatement = TryStatement;
class UnaryExpression {
    constructor(operator, argument) {
        this.type = syntax_1.Syntax.UnaryExpression;
        this.operator = operator;
        this.argument = argument;
        this.prefix = true;
    }
}
exports.UnaryExpression = UnaryExpression;
class UpdateExpression {
    constructor(operator, argument, prefix) {
        this.type = syntax_1.Syntax.UpdateExpression;
        this.operator = operator;
        this.argument = argument;
        this.prefix = prefix;
    }
}
exports.UpdateExpression = UpdateExpression;
class VariableDeclaration {
    constructor(declarations, kind) {
        this.type = syntax_1.Syntax.VariableDeclaration;
        this.declarations = declarations;
        this.kind = kind;
    }
}
exports.VariableDeclaration = VariableDeclaration;
class VariableDeclarator {
    constructor(id, init) {
        this.type = syntax_1.Syntax.VariableDeclarator;
        this.id = id;
        this.init = init;
    }
}
exports.VariableDeclarator = VariableDeclarator;
class WhileStatement {
    constructor(test, body) {
        this.type = syntax_1.Syntax.WhileStatement;
        this.test = test;
        this.body = body;
    }
}
exports.WhileStatement = WhileStatement;
class WithStatement {
    constructor(object, body) {
        this.type = syntax_1.Syntax.WithStatement;
        this.object = object;
        this.body = body;
    }
}
exports.WithStatement = WithStatement;
class YieldExpression {
    constructor(argument, delegate) {
        this.type = syntax_1.Syntax.YieldExpression;
        this.argument = argument;
        this.delegate = delegate;
    }
}
exports.YieldExpression = YieldExpression;

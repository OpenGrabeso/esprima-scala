"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const jsx_syntax_1 = require("./jsx-syntax");
/* tslint:disable:max-classes-per-file */
class JSXClosingElement {
    constructor(name) {
        this.type = jsx_syntax_1.JSXSyntax.JSXClosingElement;
        this.name = name;
    }
}
exports.JSXClosingElement = JSXClosingElement;
class JSXElement {
    constructor(openingElement, children, closingElement) {
        this.type = jsx_syntax_1.JSXSyntax.JSXElement;
        this.openingElement = openingElement;
        this.children = children;
        this.closingElement = closingElement;
    }
}
exports.JSXElement = JSXElement;
class JSXEmptyExpression {
    constructor() {
        this.type = jsx_syntax_1.JSXSyntax.JSXEmptyExpression;
    }
}
exports.JSXEmptyExpression = JSXEmptyExpression;
class JSXExpressionContainer {
    constructor(expression) {
        this.type = jsx_syntax_1.JSXSyntax.JSXExpressionContainer;
        this.expression = expression;
    }
}
exports.JSXExpressionContainer = JSXExpressionContainer;
class JSXIdentifier {
    constructor(name) {
        this.type = jsx_syntax_1.JSXSyntax.JSXIdentifier;
        this.name = name;
    }
}
exports.JSXIdentifier = JSXIdentifier;
class JSXMemberExpression {
    constructor(object, property) {
        this.type = jsx_syntax_1.JSXSyntax.JSXMemberExpression;
        this.object = object;
        this.property = property;
    }
}
exports.JSXMemberExpression = JSXMemberExpression;
class JSXAttribute {
    constructor(name, value) {
        this.type = jsx_syntax_1.JSXSyntax.JSXAttribute;
        this.name = name;
        this.value = value;
    }
}
exports.JSXAttribute = JSXAttribute;
class JSXNamespacedName {
    constructor(namespace, name) {
        this.type = jsx_syntax_1.JSXSyntax.JSXNamespacedName;
        this.namespace = namespace;
        this.name = name;
    }
}
exports.JSXNamespacedName = JSXNamespacedName;
class JSXOpeningElement {
    constructor(name, selfClosing, attributes) {
        this.type = jsx_syntax_1.JSXSyntax.JSXOpeningElement;
        this.name = name;
        this.selfClosing = selfClosing;
        this.attributes = attributes;
    }
}
exports.JSXOpeningElement = JSXOpeningElement;
class JSXSpreadAttribute {
    constructor(argument) {
        this.type = jsx_syntax_1.JSXSyntax.JSXSpreadAttribute;
        this.argument = argument;
    }
}
exports.JSXSpreadAttribute = JSXSpreadAttribute;
class JSXText {
    constructor(value, raw) {
        this.type = jsx_syntax_1.JSXSyntax.JSXText;
        this.value = value;
        this.raw = raw;
    }
}
exports.JSXText = JSXText;

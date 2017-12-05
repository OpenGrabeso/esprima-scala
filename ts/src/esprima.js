"use strict";
/*
  Copyright JS Foundation and other contributors, https://js.foundation/

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
Object.defineProperty(exports, "__esModule", { value: true });
const comment_handler_1 = require("./comment-handler");
const jsx_parser_1 = require("./jsx-parser");
const parser_1 = require("./parser");
const tokenizer_1 = require("./tokenizer");
function parse(code, options, delegate) {
    let commentHandler = null;
    const proxyDelegate = (node, metadata) => {
        if (delegate) {
            delegate(node, metadata);
        }
        if (commentHandler) {
            commentHandler.visit(node, metadata);
        }
    };
    let parserDelegate = (typeof delegate === 'function') ? proxyDelegate : null;
    let collectComment = false;
    if (options) {
        collectComment = (typeof options.comment === 'boolean' && options.comment);
        const attachComment = (typeof options.attachComment === 'boolean' && options.attachComment);
        if (collectComment || attachComment) {
            commentHandler = new comment_handler_1.CommentHandler();
            commentHandler.attach = attachComment;
            options.comment = true;
            parserDelegate = proxyDelegate;
        }
    }
    let isModule = false;
    if (options && typeof options.sourceType === 'string') {
        isModule = (options.sourceType === 'module');
    }
    let parser;
    if (options && typeof options.jsx === 'boolean' && options.jsx) {
        parser = new jsx_parser_1.JSXParser(code, options, parserDelegate);
    }
    else {
        parser = new parser_1.Parser(code, options, parserDelegate);
    }
    const program = isModule ? parser.parseModule() : parser.parseScript();
    const ast = program;
    if (collectComment && commentHandler) {
        ast.comments = commentHandler.comments;
    }
    if (parser.config.tokens) {
        ast.tokens = parser.tokens;
    }
    if (parser.config.tolerant) {
        ast.errors = parser.errorHandler.errors;
    }
    return ast;
}
exports.parse = parse;
function parseModule(code, options, delegate) {
    const parsingOptions = options || {};
    parsingOptions.sourceType = 'module';
    return parse(code, parsingOptions, delegate);
}
exports.parseModule = parseModule;
function parseScript(code, options, delegate) {
    const parsingOptions = options || {};
    parsingOptions.sourceType = 'script';
    return parse(code, parsingOptions, delegate);
}
exports.parseScript = parseScript;
function tokenize(code, options, delegate) {
    const tokenizer = new tokenizer_1.Tokenizer(code, options);
    let tokens;
    tokens = [];
    try {
        while (true) {
            let token = tokenizer.getNextToken();
            if (!token) {
                break;
            }
            if (delegate) {
                token = delegate(token);
            }
            tokens.push(token);
        }
    }
    catch (e) {
        tokenizer.errorHandler.tolerate(e);
    }
    if (tokenizer.errorHandler.tolerant) {
        tokens.errors = tokenizer.errors();
    }
    return tokens;
}
exports.tokenize = tokenize;
var syntax_1 = require("./syntax");
exports.Syntax = syntax_1.Syntax;
// Sync with *.json manifests.
exports.version = '4.0.0-dev';
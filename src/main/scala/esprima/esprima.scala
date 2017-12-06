/*
ScalaFromJS: 2017-12-05 14:48:54.460
esprima.js
*/

package esprima

import esprima.Parser.TokenEntry
import esprima.Scanner.Metadata

import scala.collection.mutable.ArrayBuffer

object Esprima {
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

def parse(code: String, options: Parser.Options, delegate: (Node.Node, Metadata) => Unit) = {
  var commentHandler: CommentHandler = null
  def proxyDelegate(node: Node.Node, metadata: Metadata) = {
    if (delegate) {
      delegate(node, metadata)
    }
    if (commentHandler) {
      commentHandler.visit(node, metadata)
    }
  }
  var parserDelegate = if (delegate != null) proxyDelegate _ else null
  var collectComment = false
  if (options) {
    collectComment = options.comment
    val attachComment = options.attachComment
    if (collectComment || attachComment) {
      commentHandler = new CommentHandler()
      commentHandler.attach = attachComment
      options.comment = true
      parserDelegate = proxyDelegate
    }
  }
  var isModule = options.sourceType == "module"

  var parser = new Parser(code, options, parserDelegate)
  val program = if (isModule) parser.parseModule() else parser.parseScript()
  val ast = program
  if (collectComment && commentHandler) {
    ast.comments = commentHandler.comments
  }
  if (parser.config.tokens) {
    ast.tokens = parser.tokens
  }
  if (parser.config.tolerant) {
    ast.errors = parser.errorHandler.errors
  }
  ast
}

def parseModule(code: String, options: Parser.Options, delegate: (Node.Node, Metadata) => Unit) = {
  val parsingOptions = options
  parsingOptions.sourceType = "module"
  parse(code, parsingOptions, delegate)
}

def parseScript(code: String, options: Parser.Options, delegate: (Node.Node, Metadata) => Unit) = {
  val parsingOptions = options
  parsingOptions.sourceType = "script"
  parse(code, parsingOptions, delegate)
}

def tokenize(code: String, options: Parser.Options, delegate: (TokenEntry) => TokenEntry): (Array[TokenEntry], Array[ErrorHandler.Error]) = {
  val tokenizer = new Tokenizer(code, options)
  val tokens = ArrayBuffer.empty[Parser.TokenEntry]
  val errors = ArrayBuffer.empty[ErrorHandler.Error]
  try {
    while (true) {
      var token = tokenizer.getNextToken()
      if (!token) {
        /* Unsupported: Break */ break;
      }
      if (delegate) {
        token = delegate(token)
      }
      tokens.push(token)
    }
  } catch {
    case e: ErrorHandler.Error =>
      tokenizer.errorHandler.tolerate(e)
  }
  if (tokenizer.errorHandler.tolerant) {
    errors ++= tokenizer.errors()
  }
  tokens.toArray -> errors.toArray
}
val version = "4.0.0-dev"

}
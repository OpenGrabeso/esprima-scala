/*
ScalaFromJS: Dev 2018-01-16 17:57:51
comment-handler.js
*/

package com.github.opengrabeso.esprima
/* import { Syntax } from './syntax' */
class CommentHandler() {
  var attach: Boolean = false
  var comments = Array.empty[Any]
  var stack = Array.empty[Any]
  var leading = Array.empty[Any]
  var trailing = Array.empty[Any]
  def insertInnerComments(node: Node, metadata: SourceLocation) = {
    //  innnerComments for properties empty block
    //  `function a() {/** comments **\/}`
    if (node.isInstanceOf[BlockStatement] && node.asInstanceOf[BlockStatement].body.length == 0) {
      val node_cast = node.asInstanceOf[BlockStatement]
      val innerComments = Array.empty[Unit]
      for (i <- this.leading.length - 1 to 0 by -1) {
        val entry = this.leading(i)
        if (metadata.end.offset >= entry.start) {
          innerComments.unshift(entry.comment)
          this.leading.splice(i, 1)
          this.trailing.splice(i, 1)
        }
      }
      if (innerComments.length) {
        node_cast.innerComments = innerComments
      }
    }
  }
  
  def findTrailingComments(metadata: SourceLocation) = {
    var trailingComments = Array.empty[Unit]
    if (this.trailing.length > 0) {
      for (i <- this.trailing.length - 1 to 0 by -1) {
        val entry = this.trailing(i)
        if (entry.start >= metadata.end.offset) {
          trailingComments.unshift(entry.comment)
        }
      }
      this.trailing.length = 0
      return trailingComments
    }
    val entry = this.stack(this.stack.length - 1)
    if (entry && entry.node.trailingComments) {
      val firstComment = entry.node.trailingComments(0)
      if (firstComment && firstComment.range(0) >= metadata.end.offset) {
        trailingComments = entry.node.trailingComments
        delete entry.node.trailingComments
      }
    }
    trailingComments
  }
  
  def findLeadingComments(metadata: SourceLocation) = {
    val leadingComments = Array.empty[Unit]
    var target: Node = _
    while (this.stack.length > 0) {
      val entry = this.stack(this.stack.length - 1)
      if (entry && entry.start >= metadata.start.offset) {
        target = entry.node
        this.stack.pop()
      } else {
        /* Unsupported: Break */ break;
      }
    }
    if (target) {
      val count = if (target.leadingComments) target.leadingComments.length else 0
      for (i <- count - 1 to 0 by -1) {
        val comment = target.leadingComments(i)
        if (comment.range(1) <= metadata.start.offset) {
          leadingComments.unshift(comment)
          target.leadingComments.splice(i, 1)
        }
      }
      if (target.leadingComments && target.leadingComments.length == 0) {
        delete target.leadingComments
      }
      return leadingComments
    }
    for (i <- this.leading.length - 1 to 0 by -1) {
      val entry = this.leading(i)
      if (entry.start <= metadata.start.offset) {
        leadingComments.unshift(entry.comment)
        this.leading.splice(i, 1)
      }
    }
    leadingComments
  }
  
  def visitNode(node: Node, metadata: SourceLocation) = {
    if (node.isInstanceOf[Program] && node.asInstanceOf[Program].body.length > 0) {
      return
    }
    this.insertInnerComments(node, metadata)
    val trailingComments = this.findTrailingComments(metadata)
    val leadingComments = this.findLeadingComments(metadata)
    if (leadingComments.length > 0) {
      node.leadingComments = leadingComments
    }
    if (trailingComments.length > 0) {
      node.trailingComments = trailingComments
    }
    this.stack.push(new {
      var node = node
      var start = metadata.start.offset
    })
  }
  
  def visitComment(node: Node, metadata: SourceLocation) = {
    val `type` = if (node.`type`(0) == "L") "Line" else "Block"
    object comment {
      var `type` = `type`
      var value = node.value
    }
    if (node.range) {
      comment.range = node.range
    }
    if (node.loc) {
      comment.loc = node.loc
    }
    this.comments.push(comment)
    if (this.attach) {
      object entry {
        var comment = new {
          var `type` = `type`
          var value = node.value
          var range = Array(metadata.start.offset, metadata.end.offset)
        }
        var start = metadata.start.offset
      }
      if (node.loc) {
        entry.comment.loc = node.loc
      }
      node.`type` = `type`
      this.leading.push(entry)
      this.trailing.push(entry)
    }
  }
  
  def visit(node: Node, metadata: SourceLocation) = {
    if (node.`type` == "LineComment") {
      this.visitComment(node, metadata)
    } else if (node.`type` == "BlockComment") {
      this.visitComment(node, metadata)
    } else if (this.attach) {
      this.visitNode(node, metadata)
    }
  }
  
}


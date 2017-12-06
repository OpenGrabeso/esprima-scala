/*
ScalaFromJS: 2017-12-05 14:48:54.460
comment-handler.js
*/

package esprima

import Scanner.SourceLocation
import Scanner.Metadata

import scala.collection.mutable.ArrayBuffer

object CommentHandler {
  trait Comment {
    def `type`: String
    def value: String
    var range: (Int, Int)
    var loc: SourceLocation
  }

  trait Entry {
    def comment: Comment
    def start: Int
  }

  trait NodeInfo {
    def node: Node.Node
    def start: Int
  }

}

import CommentHandler._

class CommentHandler() {
  var attach: Boolean = false
  var comments = ArrayBuffer.empty[Comment]
  var stack = ArrayBuffer.empty[NodeInfo]
  var leading = ArrayBuffer.empty[Entry]
  var trailing = ArrayBuffer.empty[Entry]
  def insertInnerComments(node: Node.Node, metadata: Metadata) = {
    //  innnerComments for properties empty block
    //  `function a() {/** comments **\/}`
    if (node.`type` == Syntax.BlockStatement && node.body.length == 0) {
      val innerComments = ArrayBuffer.empty[Comment]
      for (i <- this.leading.length - 1 to 0 by -1) {
        val entry = this.leading(i)
        if (metadata.end.offset >= entry.start) {
          innerComments.unshift(entry.comment)
          this.leading.splice(i, 1)
          this.trailing.splice(i, 1)
        }
      }
      if (innerComments.length) {
        node.innerComments = innerComments
      }
    }
  }
  
  def findTrailingComments(metadata: Metadata): ArrayBuffer[Comment] = {
    var trailingComments = ArrayBuffer.empty[Comment]
    if (this.trailing.length > 0) {
      for (i <- this.trailing.length - 1 to 0 by -1) {
        val entry = this.trailing(i)
        if (entry.start >= metadata.end.offset) {
          trailingComments.unshift(entry.comment)
        }
      }
      this.trailing.setLength = 0
      return trailingComments
    }
    val entry = this.stack(this.stack.length - 1)
    if (entry && entry.node.trailingComments) {
      val firstComment = entry.node.trailingComments(0)
      if (firstComment && firstComment.range._1 >= metadata.end.offset) {
        trailingComments = entry.node.trailingComments
        entry.node.trailingComments = null
      }
    }
    trailingComments
  }
  
  def findLeadingComments(metadata: Metadata): ArrayBuffer[Comment] = {
    val leadingComments = ArrayBuffer.empty[Comment]
    var target: Node.Node = null
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
        if (comment.range._2 <= metadata.start.offset) {
          leadingComments.unshift(comment)
          target.leadingComments.splice(i, 1)
        }
      }
      if (target.leadingComments && target.leadingComments.length == 0) {
        target.leadingComments = null
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
  
  def visitNode(node: Node.Node, metadata: Metadata): Unit = {
    if (node.`type` == Syntax.Program && node.body.length > 0) {
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
    this.stack.push(new NodeInfo {
      def node = node
      def start = metadata.start.offset
    })
  }
  
  def visitComment(node: Node.Node, metadata: Metadata) = {
    val `type_` = if (node.`type`(0) == "L") "Line" else "Block"
    object comment extends Comment {
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
      object entry extends Entry {
        var comment = new Comment {
          var `type` = `type_`
          var value = node.value
          var range = (metadata.start.offset, metadata.end.offset)
        }
        var start = metadata.start.offset
      }
      if (node.loc) {
        entry.comment.loc = node.loc
      }
      node.`type` = `type_`
      this.leading.push(entry)
      this.trailing.push(entry)
    }
  }
  
  def visit(node: Node.Node, metadata: Metadata) = {
    if (node.`type` == "LineComment") {
      this.visitComment(node, metadata)
    } else if (node.`type` == "BlockComment") {
      this.visitComment(node, metadata)
    } else if (this.attach) {
      this.visitNode(node, metadata)
    }
  }
  
}


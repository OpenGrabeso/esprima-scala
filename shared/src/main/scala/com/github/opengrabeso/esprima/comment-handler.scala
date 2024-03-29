/*
ScalaFromJS: Dev
comment-handler.ts
*/

package com.github.opengrabeso.esprima
/* import { SourceLocation } from './scanner' */
/* import { Syntax } from './syntax' */

import Scanner.SourceLocation

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

object CommentHandler {
  trait Comment {
    val `type`: String
    val value: String
    var range: (Int, Int) = _
    var loc: SourceLocation = _
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
  def insertInnerComments(node: Node.Node, metadata: SourceLocation) = {
    //  innnerComments for properties empty block
    //  `function a() {/** comments **\/}`
    if (node.isInstanceOf[Node.BlockStatement] && node.asInstanceOf[Node.BlockStatement].body.length == 0) {
      val node_cast = node.asInstanceOf[Node.BlockStatement]
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
  
  def findTrailingComments(metadata: SourceLocation): ArrayBuffer[Comment] = {
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
    if (this.stack.length > 0) {
      val last = this.stack(this.stack.length - 1)
      if (last && last.node.trailingComments) {
        val firstComment = last.node.trailingComments(0)
        if (firstComment && firstComment.range._1 >= metadata.end.offset) {
          trailingComments = last.node.trailingComments
          last.node.trailingComments = null
        }
      }
    }
    trailingComments
  }
  
  def findLeadingComments(metadata: SourceLocation): ArrayBuffer[Comment] = {
    val leadingComments = ArrayBuffer.empty[Comment]
    var target: Node.Node = null
    breakable {
      while (this.stack.length > 0) {
        val entry = this.stack(this.stack.length - 1)
        if (entry && entry.start >= metadata.start.offset) {
          target = entry.node
          this.stack.pop()
        } else {
          break()
        }
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
  
  def visitNode(node: Node.Node, metadata: SourceLocation): Unit = {
    if (node.isInstanceOf[Node.Program] && node.asInstanceOf[Node.Program].body.length > 0) {
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
    val node_ = node
    this.stack.push(new NodeInfo {
      val node = node_
      val start = metadata.start.offset
    })
  }
  
  def visitComment(node: Node.CommentNode, metadata: SourceLocation) = {
    val `type_` = if (!node.multiline) "Line" else "Block"
    object comment extends Comment {
      val `type` = `type_`
      val value = node.value
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
          override val `type` = `type_`
          override val value = node.value
          range = (metadata.start.offset, metadata.end.offset)
        }
        var start = metadata.start.offset
      }
      if (node.loc) {
        entry.comment.loc = node.loc
      }
      //node.`type` = `type_` // PORT: Node type changed. Why?
      this.leading.push(entry)
      this.trailing.push(entry)
    }
  }
  
  def visit(node: Node.Node, metadata: SourceLocation) = {
    node match {
      case node: Node.CommentNode  =>
        this.visitComment(node, metadata)
      case _ =>
        this.visitNode(node, metadata)
    }
  }
  
}


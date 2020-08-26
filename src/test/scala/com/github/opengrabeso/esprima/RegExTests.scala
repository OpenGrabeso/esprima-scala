package com.github.opengrabeso.esprima

import Esprima._
import com.github.opengrabeso.esprima.Parser.TokenEntry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RegExTests extends AnyFlatSpec with Matchers {
  // examples from https://github.com/sweet-js/sweet-core/wiki/design
  object IsToken {
    def unapply(tokenEntry: TokenEntry): Option[(String, String)] = Some(tokenEntry.`type`, tokenEntry.value)
  }

  "Tokenizer" should "tokenize regex" in {
    // a declaration so / is regex
    val source = "f(); function foo() {} /42/i"
    val ast = tokenize(source)
    ast._1.last should matchPattern {
      case IsToken("RegularExpression", "/42/i") =>
    }
  }

  it should "not tokenize function divide as regex " in {
    // an expression so / is divide
    val source = "x = function foo() {} /42/i"
    val tokens = tokenize(source)
    tokens._1.toSeq.takeRight(4) should matchPattern {
      case Seq(
        IsToken("Punctuator", "/"),IsToken("Numeric", "42"),
        IsToken("Punctuator", "/"),IsToken("Identifier", "i")
      ) =>
    }
  }

}

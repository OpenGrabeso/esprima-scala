package com.github.opengrabeso.esprima

import com.github.opengrabeso.esprima.Esprima.{parse, tokenize}
import org.scalatest.FlatSpec

class BasicTests extends FlatSpec with TestInputs {
  "Tokenizer" should "process simple input" in {
    val tokens = tokenize(answer42)
    assert(tokens._1.nonEmpty)
  }

  it should "process more complex es6 input" in {
    val tokens = tokenize(es6)
    assert(tokens._1.nonEmpty)
  }

  "Parser" should "process simple input" in {
    val tree = parse(answer42)
    assert(tree.body.nonEmpty)
  }

  it should "process more complex es6 input" in {
    val tree = parse(es6)
    assert(tree.body.nonEmpty)
  }

  it should "process esprima.js" in {
    val tree = parse(esprimaSource)
    assert(tree.body.nonEmpty)
  }

  it should "process three.js" in {
    val tree = parse(threeSource)
    assert(tree.body.nonEmpty)
  }

  behavior of "Parser"

  it should "parse end of file a++" in {
    parse("a++") // was throwing index out of range
  }

  it should "parse inputs with single line comments" in {
    object EnableComments extends Parser.Options {
      range = true
      attachComment = true
    }
    parse("""
    var i; // assigned once
    var ii; // assigned multiple times,
    """, EnableComments)
  }

  it should "parse inputs with multiple single line comments correctly" in {
    object EnableComments extends Parser.Options {
      range = true
      attachComment = true
    }
    val ast = parse("""
    // Comment 1
    // Comment 2
    var i;
    var ii;
    """, EnableComments)
    val firstStatement = ast.body.head
    assert(firstStatement.leadingComments.size == 2)
    assert(firstStatement.leadingComments(0).value.contains("Comment 1"))
    assert(firstStatement.leadingComments(1).value.contains("Comment 2"))
    assert(!firstStatement.leadingComments(0).value.contains("Comment 2"))
    assert(!firstStatement.leadingComments(1).value.contains("Comment 1"))
  }


  it should "parse input with arrow function passed as an argument" in {
    parse("""
    f( 'G', () => {
	    g( 'P', ( arg ) => {} );
    } );
    """)
  }

  it should "parse input with rest arguments" in {
    parse("""
    function sum(...theArgs) {
      return theArgs.reduce((previous, current) => {
        return previous + current;
      });
    }
    """)
  }

}

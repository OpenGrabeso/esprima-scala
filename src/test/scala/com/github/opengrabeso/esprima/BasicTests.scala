package com.github.opengrabeso.esprima

import com.github.opengrabeso.esprima.Esprima.{parse, tokenize}
import org.scalatest.FlatSpec

class BasicTests extends FlatSpec with TestInputs {
  "Tokenizer" should "process simple input" in {
    val tokens = tokenize(answer42)
    println(tokens)
  }

  it should "process more complex es6 input" in {
    val tokens = tokenize(es6)
    println(tokens)
  }

  "Parser" should "process simple input" in {
    val tree = parse(answer42)
    println(tree)
  }

  it should "process more complex es6 input" in {
    val tree = parse(es6)
    println(tree)
  }

  it should "process esprima.js" in {
    val tree = parse(esprimaSource)
    println(tree)
  }

  it should "process three.js" in {
    val tree = parse(threeSource)
    println(tree)
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

  it should "parse input with arrow function passed as an argument" in {
    parse("""
    f( 'G', () => {
	    g( 'P', ( arg ) => {} );
    } );
    """)
  }

}

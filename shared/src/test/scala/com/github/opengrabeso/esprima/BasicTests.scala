package com.github.opengrabeso.esprima

import com.github.opengrabeso.esprima.Esprima.{parse, tokenize}
import org.scalatest.flatspec.AnyFlatSpec

class BasicTests extends AnyFlatSpec with TestInputs with TestOptions {
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

  it should "parse single line comments" in {
    object EnableComments extends Parser.Options {
      range = true
      attachComment = true
    }
    parse("""
    var i; // assigned once
    var ii; // assigned multiple times,
    """, EnableComments)
  }

  it should "parse multiple single line comments correctly" in {
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

  it should "parse arrow function" in {
    parse("""
    var f = (s) =>{};
    """)
  }

  it should "parse arrow function without parens" in {
    pendingUntilFixed {
      parse(
        """
          var f = s =>{};
          """)
    }
  }

  it should "parse arrow function without braces" in {
    parse("""
    var f = (s) => s;
    """)
  }

  it should "parse arrow function passed as an argument" in {
    parse("""
    f( 'G', () => {
	    g( 'P', ( arg ) => {} );
    } );
    """)
  }

  it should "parse arrow function with parens passed as an argument (a different example)" in {
    pendingUntilFixed {
      parse(
        """
          function f() {
            ss.forEach( s =>{
              s.call();
            } );
          }
          """)
    }
  }



  it should "parse rest arguments" in {
    parse("""
    function sum(...theArgs) {
      return theArgs.reduce((previous, current) => {
        return previous + current;
      });
    }
    """)
  }


}

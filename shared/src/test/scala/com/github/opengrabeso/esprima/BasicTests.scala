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
    parse(
      """
        var f = s =>{};
        """)
  }

  it should "parse arrow function without braces" in {
    parse("""
    var f = (s) => s;
    """)
  }

  it should "preserve aliased object properties with default values in arrow parameters" in {
    val tree = parse("""
      const f = ({ normalView: normalViewAlias = normalView }) => normalViewAlias;
    """)

    val declaration = tree.body.head.asInstanceOf[Node.VariableDeclaration]
    val arrow = declaration.declarations.head.init.asInstanceOf[Node.ArrowFunctionExpression]
    val pattern = arrow.params.head.asInstanceOf[Node.ObjectPattern]
    val property = pattern.properties.head.asInstanceOf[Node.Property]
    val assignment = property.value.asInstanceOf[Node.AssignmentPattern]

    assert(property.key.asInstanceOf[Node.Identifier].name == "normalView")
    assert(assignment.left.asInstanceOf[Node.Identifier].name == "normalViewAlias")
    assert(assignment.right.asInstanceOf[Node.Identifier].name == "normalView")
  }

  it should "parse nullish coalescing expressions" in {
    val tree = parse("const value = input ?? fallback;")
    val declaration = tree.body.head.asInstanceOf[Node.VariableDeclaration]
    val expression = declaration.declarations.head.init.asInstanceOf[Node.BinaryExpression]

    assert(expression.operator == "??")
  }

  it should "parse class static initialization blocks" in {
    val tree = parse("class Vector2 { static { Vector2.prototype.isVector2 = true; } }")
    val declaration = tree.body.head.asInstanceOf[Node.ClassDeclaration]
    val block = declaration.body.body.head.asInstanceOf[Node.StaticBlock]

    assert(block.body.size == 1)
    assert(block.body.head.isInstanceOf[Node.ExpressionStatement])
  }

  it should "parse arrow function passed as an argument" in {
    parse("""
    f( 'G', () => {
	    g( 'P', ( arg ) => {} );
    } );
    """)
  }

  it should "parse arrow function without parens passed as an argument (a different example)" in {
    parse(
      """
        function f() {
          ss.forEach( s =>{
            s.call();
          } );
        }
        """)
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

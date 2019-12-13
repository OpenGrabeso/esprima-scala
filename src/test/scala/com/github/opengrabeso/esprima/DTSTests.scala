package com.github.opengrabeso.esprima

import com.github.opengrabeso.esprima.Esprima._
import org.scalatest.{FlatSpec, Matchers}

class DTSTests extends FlatSpec with TestInputs with Matchers {
  object DTSOptions extends Parser.Options {
    range = true
    attachComment = true
    tolerant = true
  }

  behavior of "Parsing simple d.ts"

  it should "Parse a variable with a type annotation" in {
    val input ="var answer: number = 42"
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    tree.body.head should matchPattern {
      case Node.VariableDeclaration(Seq(Node.VariableDeclarator(Node.Identifier("answer"), _, Node.SimpleType(Node.TypeScriptType.number))), _) =>
    }
  }

  it should "Parse a class with a typed member" in {
    val input ="""
        export class Range {
          max: number;
          min: number;
        }
        """.stripMargin

    pendingUntilFixed {
      val tree = parse(input, DTSOptions)
      assert(tree.body.nonEmpty)
    }

  }

  it should "Parse a class with a constructor" in {
    val input ="""
        export class Range {
          constructor( min: number, max: number );
        }
        """

    pendingUntilFixed {
      val tree = parse(input, DTSOptions)
      assert(tree.body.nonEmpty)
    }

  }

  it should "Parse a class with a typed member functions" in {
    val input ="""
        export class Range {
          set(min: number, max: number): boolean;
          isEmpty(): boolean;
          clone(): Range;
          copy(box: Range): Range;
        	equals( box: Range ): boolean;
        }
        """

    pendingUntilFixed {
      val tree = parse(input, DTSOptions)
      assert(tree.body.nonEmpty)
    }

  }

  behavior of "Parsing Three.js d.ts"

  it should "process Box2" in {
    val input = fromResource("/threejs/d.ts/Box2.d.ts")
    pendingUntilFixed {
      val tree = parse(input, DTSOptions)
      assert(tree.body.nonEmpty)
    }
  }

  ignore should "process Quaternion" in {
    val input = fromResource("/threejs/d.ts/Quaternion.d.ts")
    pendingUntilFixed {
      val tree = parse(input, DTSOptions)
      assert(tree.body.nonEmpty)
    }
  }

  ignore should "process Object3D" in {
    val input = fromResource("/threejs/d.ts/Object3D.d.ts")
    pendingUntilFixed {
      val tree = parse(input, DTSOptions)
      assert(tree.body.nonEmpty)
    }
  }

}

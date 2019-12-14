package com.github.opengrabeso.esprima

import Esprima._
import Node._
import org.scalatest.{FlatSpec, Matchers}

class DTSTests extends FlatSpec with TestInputs with Matchers {
  object DTSOptions extends Parser.Options {
    range = true
    attachComment = true
    tolerant = true
    sourceType = "module" // allow exports
  }

  behavior of "Parsing simple d.ts"

  it should "Parse a variable with a type annotation" in {
    val input ="var answer: number = 42"
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    tree.body.head should matchPattern {
      case VariableDeclaration(Seq(VariableDeclarator(Identifier("answer"), _, TypeName(Identifier("number")))), _) =>
    }
    assert(tree.errors.isEmpty)
  }

  object Method {
    def unapply(arg: MethodDefinition) = arg match {
      case MethodDefinition(Identifier(name), _, _, FunctionExpression(_, pars, _, _, ret), kind, false) =>
        val p = pars.map {
          case FunctionParameterWithType(Identifier(name), t, defValue, optional) =>
            (name, t, defValue, optional)
        }
        Some(name, p, ret, kind)
      case _ =>
        None
    }
  }
  object NamedType {
    def unapply(arg: TypeName): Option[String] = arg match {
      case TypeName(Identifier(name)) =>
        Some(name)
      case _ =>
        None
    }
  }

  it should "Parse a class with a typed member" in {
    val input ="""
        export class Range {
          max: number;
          min: number;
        }
        """.stripMargin

    val tree = parse(input, DTSOptions)

    tree.body.head should matchPattern {
      case ExportNamedDeclaration(ClassDeclaration(Identifier("Range"), null, ClassBody(Seq(
        MethodDefinition(Identifier("max"), NamedType("number"), _, _, _, false),
        MethodDefinition(Identifier("min"), NamedType("number"), _, _, _, false)
      ))), _, _) =>
    }
    assert(tree.errors.isEmpty)
  }

  it should "Parse a class with a constructor" in {
    val input ="""
        export class Range {
          constructor( min: number, max: number );
        }
        """

    val tree = parse(input, DTSOptions)
    tree.body.head should matchPattern {
      case ExportNamedDeclaration(ClassDeclaration(Identifier("Range"), null, ClassBody(Seq(
        Method("constructor", Seq(("min", NamedType("number"), null, false), ("max", NamedType("number"), null, false)), null, "constructor"),
      ))), _, _) =>
    }
    assert(tree.errors.isEmpty)
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

    val tree = parse(input, DTSOptions)
    tree.body.head should matchPattern {
      case ExportNamedDeclaration(ClassDeclaration(Identifier("Range"), null, ClassBody(Seq(
        Method("set", Seq(("min", NamedType("number"), null, false), ("max", NamedType("number"), null, false)), NamedType("boolean"), _),
        Method("isEmpty", Seq(), NamedType("boolean"), _),
        Method("clone", Seq(), NamedType("Range"), _),
        Method("copy", Seq(("box", NamedType("Range"), null, false)), NamedType("Range"), _),
        Method("equals", Seq(("box", NamedType("Range"), null, false)), NamedType("boolean"), _),
      ))), _, _) =>
    }
    assert(tree.errors.isEmpty)

  }

  it should "Parse a class member with optional parameters" in {
    val input ="""
        export class Range {
          set(min?: number, max?: number): boolean;
        }
        """

    val tree = parse(input, DTSOptions)
    tree.body.head should matchPattern {
      case ExportNamedDeclaration(ClassDeclaration(Identifier("Range"), null, ClassBody(Seq(
        Method("set", Seq(("min", NamedType("number"), null, true), ("max", NamedType("number"), null, true)), NamedType("boolean"), _),
      ))), _, _) =>
    }
    assert(tree.errors.isEmpty)

  }

  it should "Parse a class with an array members" in {
    val input ="""
        export class A {
          a: number[];
          set(v: number[]): void;
        }
        """

    val tree = parse(input, DTSOptions)
    tree.body.head should matchPattern {
      case ExportNamedDeclaration(ClassDeclaration(Identifier("A"), null, ClassBody(Seq(
        MethodDefinition(Identifier("a"), ArrayType(NamedType("number")), _, _, _, false),
        Method("set", Seq(("v", ArrayType(NamedType("number")), null, false)), NamedType("void"), _),
      ))), _, _) =>
    }
    assert(tree.errors.isEmpty)

  }

  it should "Parse a class member with generic parameters" in {
    val input ="""
        export class C {
          set(a: ArrayLike<number>): void;
        }
        """

    val tree = parse(input, DTSOptions)
    tree.body.head should matchPattern {
      case ExportNamedDeclaration(ClassDeclaration(Identifier("C"), null, ClassBody(Seq(
      Method("set", Seq(("a", TypeReference(NamedType("ArrayLike"), NamedType("number")), null, false)), NamedType("void"), _),
      ))), _, _) =>
    }
    assert(tree.errors.isEmpty)

  }

  it should "Parse a class with a union type members" in {
    val input ="""
        export class A {
          a: number | null;
          b: number | string | A;
        }
        """

    val tree = parse(input, DTSOptions)
    tree.body.head should matchPattern {
      case ExportNamedDeclaration(ClassDeclaration(Identifier("A"), null, ClassBody(Seq(
      MethodDefinition(Identifier("a"), UnionType(NamedType("number"), NamedType("null")), _, _, _, false),
      MethodDefinition(Identifier("b"), UnionType(UnionType(NamedType("number"), NamedType("string")), NamedType("A")), _, _, _, false),
      ))), _, _) =>
    }
    assert(tree.errors.isEmpty)

  }

  behavior of "Parsing Three.js d.ts"

  it should "process Box2" in {
    val input = fromResource("/threejs/d.ts/Box2.d.ts")
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
  }

  it should "process Quaternion" in {
    val input = fromResource("/threejs/d.ts/Quaternion.d.ts")
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
  }

  it should "process Object3D" in {
    val input = fromResource("/threejs/d.ts/Object3D.d.ts")
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
  }

}

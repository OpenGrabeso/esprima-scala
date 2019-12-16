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

  def extractPars(pars: Seq[FunctionParameter]) = {
    pars.map {
      case FunctionParameterWithType(Identifier(name), t, defValue, optional) =>
        (name, t, defValue, optional)
      case Identifier(name) =>
        (name, null, null, false)
    }
  }

  object Method {
    def unapply(arg: MethodDefinition) = arg match {
      case MethodDefinition(Identifier(name), _, _, FunctionExpression(_, pars, _, _, ret), kind, false) =>
        Some(name, extractPars(pars), ret, kind)
      case _ =>
        None
    }
  }
  object FunctionDecl {
    def unapply(arg: FunctionDeclaration) = arg match {
      case FunctionDeclaration(Identifier(name), pars, _, _, ret) =>
        Some(name, extractPars(pars), ret)
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



  behavior of "Parsing simple d.ts"

  it should "Parse a variable with a type annotation" in {
    val input = "var answer: number = 42"
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
    tree.body.head should matchPattern {
      case VariableDeclaration(Seq(VariableDeclarator(Identifier("answer"), _, TypeName(Identifier("number")))), _) =>
    }
  }

  it should "Parse exported variables with type annotations" in {
    val input = """
        export let a: number;
        export var b: string;
        export let c: {
          o: {
            s: string;
            n: number;
          };
        };
        """
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
  }

  it should "Parse a function with a type annotation" in {
    val input =
      """
        export function f(): number;
        export function g();
        export function h(x: string): void;
        """
    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
    assert(tree.body.nonEmpty)
    val exports = tree.body.collect {
      case e: ExportNamedDeclaration =>
        e.declaration
    }
    exports(0) should matchPattern {
      case FunctionDecl("f", Nil, NamedType("number")) =>
    }
    exports(1) should matchPattern {
      case FunctionDecl("g", Nil, null) =>
    }
    exports(2) should matchPattern {
      case FunctionDecl("h", Seq(("x", NamedType("string"), null, false) ), NamedType("void")) =>
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

  it should "Parse a class with optional members" in {
    val input ="""
        export class Range {
          memberA ?: number;
          memberB ? : number;
        }
        """

    val tree = parse(input, DTSOptions)
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

  it should "Parse a type declaration" in {
    val input = "type T = A | B"
    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
    tree.body.head should matchPattern {
      case TypeAliasDeclaration(Identifier("T"), UnionType(NamedType("A"), NamedType("B"))) =>
    }
  }

  it should "Parse interface declarations" in {
    val input = """
        interface A {
          a: number | null;
        }

        export interface B {
          b: string;
        }
        """

    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
  }

  it should "Parse a class implementing interfaces" in {
    val input = """
        class C extends A implements B {
          b: string;
        }
        class D implements B {
          b: string;
        }
        interface I extends A, B {
          i: number;
        }
        """

    val tree = parse(input, DTSOptions)
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

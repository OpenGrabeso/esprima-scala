package com.github.opengrabeso.esprima

import Esprima._
import Node._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import collection.Seq
import Resources._

class DTSTests extends AnyFlatSpec with TestInputs with Matchers {
  object DTSOptions extends Parser.Options {
    range = true
    attachComment = true
    tolerant = true
    typescript = true
    sourceType = "module" // allow exports
  }

  def extractPars(pars: collection.Seq[FunctionParameter]) = {
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
        Some((name, extractPars(pars), ret, kind))
      case _ =>
        None
    }
  }
  object FunctionDecl {
    def unapply(arg: FunctionDeclaration) = arg match {
      case FunctionDeclaration(Identifier(name), pars, _, _, ret) =>
        Some((name, extractPars(pars), ret))
      case _ =>
        None
    }
  }
  object NamedType {
    def unapply(arg: TypeName): Option[String] = arg match {
      case TypeName(Seq(Identifier(name))) =>
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
      case VariableDeclaration(Seq(VariableDeclarator(Identifier("answer"), _, TypeName(Seq(Identifier("number"))))), _) =>
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

  it should "Parse exported namespace" in {
    val input = """
        export namespace A {
          export function f(u: any[]): any;
          export function g(u: any): any;
          export class C {
            m: number;
          };
        };
        """
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
  }

  it should "Parse a variable with a qualified type annotation" in {
    val input = "var answer: Some.Value"
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
    assert(tree.errors.isEmpty)

    tree.body.head should matchPattern {
      case ExportNamedDeclaration(ClassDeclaration(Identifier("Range"), null, Nil, ClassBody(Seq(
        MethodDefinition(Identifier("max"), NamedType("number"), _, _, _, false),
        MethodDefinition(Identifier("min"), NamedType("number"), _, _, _, false)
      )), _), _, _) =>
    }
  }

  it should "Parse a class with a constructor" in {
    val input ="""
        export class Range {
          constructor( min: number, max: number );
        }
        """

    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
    tree.body.head should matchPattern {
      case ExportNamedDeclaration(ClassDeclaration(Identifier("Range"), null, Nil, ClassBody(Seq(
        Method("constructor", Seq(("min", NamedType("number"), null, false), ("max", NamedType("number"), null, false)), null, "constructor")
      )), _), _, _) =>
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

    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
    tree.body.head should matchPattern {
      case ExportNamedDeclaration(ClassDeclaration(Identifier("Range"), null, Nil, ClassBody(Seq(
        Method("set", Seq(("min", NamedType("number"), null, false), ("max", NamedType("number"), null, false)), NamedType("boolean"), _),
        Method("isEmpty", Seq(), NamedType("boolean"), _),
        Method("clone", Seq(), NamedType("Range"), _),
        Method("copy", Seq(("box", NamedType("Range"), null, false)), NamedType("Range"), _),
        Method("equals", Seq(("box", NamedType("Range"), null, false)), NamedType("boolean"), _)
      )), _), _, _) =>
    }

  }

  it should "Parse a class member with optional parameters" in {
    val input ="""
        export class Range {
          set(min?: number, max?: number): boolean;
        }
        """

    val tree = parse(input, DTSOptions)
    tree.body.head should matchPattern {
      case ExportNamedDeclaration(ClassDeclaration(Identifier("Range"), null, Nil, ClassBody(Seq(
        Method("set", Seq(("min", NamedType("number"), null, true), ("max", NamedType("number"), null, true)), NamedType("boolean"), _)
      )), _), _, _) =>
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

  it should "Parse a class with index signature" in {
    val input ="""
        var a = {
          [name]: "value"
        };
        export class A {
          [data: string]: string;
        }
        export class B {
          [data: number]: any;
        }
        """

    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
  }

  it should "Parse a class with array members" in {
    val input ="""
        export class A {
          a: number[];
          set(v: number[]): void;
        }
        """

    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
    tree.body.head should matchPattern {
      case ExportNamedDeclaration(ClassDeclaration(Identifier("A"), null, Nil, ClassBody(Seq(
        MethodDefinition(Identifier("a"), ArrayType(NamedType("number")), _, _, _, false),
        Method("set", Seq(("v", ArrayType(NamedType("number")), null, false)), NamedType("void"), _)
      )), _), _, _) =>
    }

  }

  it should "Parse a class with generic parameters" in {
    val input ="""
        export class C<T> {
          member: T;
        }
        var cs = new C<string>();
        var cn = new C<number>();
        """

    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
    assert(tree.body.nonEmpty)
  }

  it should "Parse a class member with generic parameters" in {
    val input ="""
        export class C {
          set(a: ArrayLike<number>): void;
        }
        """

    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
    tree.body.head should matchPattern {
      case ExportNamedDeclaration(ClassDeclaration(Identifier("C"), null, Nil, ClassBody(Seq(
      Method("set", Seq(("a", TypeReference(NamedType("ArrayLike"), Seq(NamedType("number"))), null, false)), NamedType("void"), _)
      )), _), _, _) =>
    }
  }

  it should "Parse a class with static member methods" in {
    val input ="""
      export class C {
        static fun(data: string): string;
      }
      """

    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
  }

  it should "Parse a class with generic member methods" in {
    val input ="""
      export class C {
        fun<T extends Object3D>(data: T): T;
      }
      """

    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
  }

  it should "Parse a generic class" in {
    val input ="""
      export class A<T> extends B<T> {
        fun(data: T): T;
      }
      class C<T> extends B<T> {
        fun(data: T): T;
      }
      class D<T extends I, X, Y extends I> extends B<T> {
        fun(data: T): X;
      }
      class D<T extends I = X, Y = B> {
      }
      """

    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
  }

  it should "Parse a class with union type members" in {
    val input ="""
        export class A {
          a: number | null;
          b: number | string | A;
        }
        """

    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
    tree.body.head should matchPattern {
      case ExportNamedDeclaration(ClassDeclaration(Identifier("A"), null, Nil, ClassBody(Seq(
      MethodDefinition(Identifier("a"), UnionType(NamedType("number"), LiteralType(Literal(_, "null"))), _, _, _, false),
      MethodDefinition(Identifier("b"), UnionType(UnionType(NamedType("number"), NamedType("string")), NamedType("A")), _, _, _, false)
      )), _), _, _) =>
    }

  }

  it should "Parse a class with intersection type members" in {
    val input ="""
        export class A {
          a: string & {};
          b: X | (string & {});
          c: X | (B & C);
        }
        """

    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
    tree.body.head should matchPattern {
      case ExportNamedDeclaration(ClassDeclaration(Identifier("A"), null, Nil, ClassBody(Seq(
      MethodDefinition(Identifier("a"), IntersectionType(NamedType("string"), ObjectType(Nil)), _, _, _, false),
      MethodDefinition(Identifier("b"), UnionType(NamedType("X"), IntersectionType(NamedType("string"), ObjectType(Nil))), _, _, _, false),
      MethodDefinition(Identifier("c"), UnionType(NamedType("X"), IntersectionType(NamedType("B"), NamedType("C"))), _, _, _, false)
      )), _), _, _) =>
    }

  }

  it should "Parse a class with function members" in {
    val input ="""
        export class A {
          a: () => void;
          b: (x: number) => number;
        }
        """

    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
    assert(tree.body.nonEmpty)
  }

  it should "Parse a type declaration" in {
    val input = "type T = A | B"
    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
    tree.body.head should matchPattern {
      case TypeAliasDeclaration(Identifier("T"), UnionType(NamedType("A"), NamedType("B"))) =>
    }
  }

  it should "Parse a parenthesised type" in {
    val input = "type T = (A | B)[]"
    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
    assert(tree.body.nonEmpty)
  }

  it should "Parse a tuple" in {
    val input =
      """
        type T = [number, number];
        var x: [number, string, boolean];
        export class C {
          member: [number, number];
        };
        """
    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
    assert(tree.body.nonEmpty)
  }

  it should "Parse an object type" in {
    val input =
      """
        export class C {
          member: {n: number; s: string};
        };
        """
    val tree = parse(input, DTSOptions)
    assert(tree.errors.isEmpty)
    assert(tree.body.nonEmpty)
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
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
  }

  it should "Parse abstract class declarations" in {
    val input = """
        abstract class A {
          a: number | null;
        }

        export abstract class B {
          b: string;
        }
        """

    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
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

  it should "Parse export const declarations" in {
    val input = """
      export const A: number;
      export const B: number;
      """

    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
  }

  it should "Parse export enum declarations" in {
    val input = """
      export enum A {
        A0,
        A1
      };
      enum B {
        B0 = 100,
        B1
      };
      export enum E {};
      enum F {};
      """

    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
  }

  it should "Parse as casting" in {
    val input = """
      var x = 0;
      var y = x as C;
      """
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
  }

  it should "Parse class readonly properties" in {
    val input = """
      interface C {
        readonly x: number;
      };
      """
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
  }

  it should "Parse object readonly properties" in {
    val input = """
      var x = {
        readonly x: number,
        readonly s: string
      };
      type T = {
        readonly t: number,
        readonly b: boolean
      };
      """
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
  }

  it should "Parse generic function types" in {
    val input = """
    export class O {
      onA?: <T>( object: T ) => void;
      onB?: <T extends O3D>( object: T ) => void;
      onC?: ( event: ProgressEvent ) => void;

      l(
        url: string,
        onL?: <OT extends O3D>( object: OT ) => void,
        onP?: ( event: ProgressEvent ) => void,
      ): void;
    }
    """
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
  }

  it should "Parse declare class (including export)" in {
    val input = """
    export declare class A {
    }

    declare class B {
    }
    """
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)

  }

  it should "Parse declare type (including export)" in {
    val input = """
         export declare type Side = 'none' | 'left' | 'right';
    """
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
  }

  it should "Parse readonly class value members" in {
    val input = """
      export declare class XRHand {
      	static readonly WRIST = 0;
      }
    """
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
  }

  it should "Parse override properties" in {
    val input =
      """
      export declare class C {
        readonly s: string;
        n: number;
      }
      export declare class D extends C {
        override readonly s: string | "S";
        override n: number | 0.0;
      }
  """
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
  }

  it should "Parse a generic type" in {
    val input =
      """
      declare type Constructor<T = object> = {
        new ( ...args: any[] ): T,
        prototype: T
      };
    """
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
  }

  it should "Parse Array<X<T>>" in {
    val input =
      """
      export class AX<T> extends X<T> {
          constructor();

          type: string;

          xs: Array<X<T>>;

          flag: boolean;
      }
    """
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)

  }

  it should "Parse extends keyof" in {
    val input =
      """
      export class B<
        A extends C = D,
      > extends ED {
        setAttribute<K extends keyof A>(name: K, attribute: A[K]): this;
      }
      """
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)

  }




  it should "Parse class and function in export namespace" in {
    val input =
      """
      export namespace N {
          class C {
              constructor();

              getValue(): any;
              setValue(v: any): void;
          }
          function f(a: any): any;
      }
    """
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)

  }



  it should "Parse arrow function members of class with a trailing space" in {
    val input =
      """
      export class C {
          callback: (a: string, b: number, ) => void;
      }
    """
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)

  }

  it should "Parse conditional types" in {
    val input =
      """
      class X {
          get<T>(t: T): T extends Text ? Text : T;
      }
      """
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
  }


  it should "parse type guards" in {
    parse("""
    function isFish(pet: Fish | Bird): pet is Fish {
      return (pet as Fish).swim !== undefined;
    }
    function isDecimalDigitChar(ch: string): ch is '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' {
        return ch.length === 1 && Character.isDecimalDigit(ch.charCodeAt(0));
    }
    """, DTSOptions)

  }

  it should "parse arrow function with explicit return type" in {
    parse("""
    const f = (par) => {};
    const fn = (par): number => {};
    const fv = (p1, p2): void => {};
    """, DTSOptions)
  }

  it should "parse a conditional expression resembling an arrow function" in {
    parse("""
    var x = c ? (true) : false;
    """, DTSOptions)
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

  it should "process WebXR" in {
    val input = fromResource("/threejs/d.ts/WebXR.d.ts")
    val tree = parse(input, DTSOptions)
    assert(tree.body.nonEmpty)
    assert(tree.errors.isEmpty)
  }
}

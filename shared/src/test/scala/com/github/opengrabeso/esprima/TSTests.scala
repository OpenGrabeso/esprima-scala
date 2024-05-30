package com.github.opengrabeso.esprima

import Esprima._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TSTests extends AnyFlatSpec with TestInputs with Matchers {
  object TSOptions extends Parser.Options {
    range = true
    attachComment = true
    tolerant = true
    typescript = true
    sourceType = "module" // allow exports
  }

  behavior of "Typescript parser"

  it should "parse abstract, readOnly or override functions" in {
    parse("""
    abstract class X extends Y {
      member() {}
      readonly r() {}
      abstract a(): void;
      override o() {}
    }
    """, TSOptions)

    }

  it should "parse normal and async member function with types" in {
    parse("""
   abstract class X {
     f(x: string): void {}
     abstract fAbstract(x: string): void
     async fAsync(x: string): void {}
   }
    """, TSOptions)

  }

  it should "parse arrow function with parameter types" in {
    parse("""
    const a = (t: number) => {};
    const b = (t: number, n: N) => {}
    const c = (t: number): void => {};
    const d = (t: number, n: N): void => {};
      """, TSOptions)
  }

  it should "parse generic class constructor" in {
    parse("""
      let mb = new X<number>();
      let mb = new Map<number, number>();
      """, TSOptions)

  }
  it should "parse class with typed members with initial values" in {
    parse("""
    export class C {
      public os: M[] = [];
      public oc = 0;
      public readonly aos: M[] = [];
      private n = 0x90;
      private ma = new Map();
      private mb = new Map<number>();
      private mc = new Map<number, number>();
    }
      """, TSOptions)
  }

  it should "parse functions with readonly parameters" in {
    parse ("""
    export function f(a: readonly number): number

    export class C {
        m(x: readonly number): string;
    }
    """, TSOptions)
  }

  it should "parse complex declarations with named tuples" in {
    parse ("""
    export class C {
        set(...args: [color: CR] | [r: number, g: number, b: number]): this;
    }
    """, TSOptions)
  }

  it should "parse complex declarations with this type" in {
    parse ("""
    declare class BM {
        cs: ((this: this, list: Array<{ s: number; c: number; z: number }>, c: Cls) => void) | null;
    }

    export { BM };
    """, TSOptions)
  }

  it should "parse generic functions" in {
    parse ("""
    export function cu<T extends { [u: string]: IU }>(uSrc: T): T;
    """, TSOptions)

  }
}

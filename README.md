# esprima-scala
Port of Esprima JS parser to Scala

Converted from https://github.com/jquery/esprima with help of https://github.com/OndrejSpanel/ScalaFromJS

The main purpose is to be used in https://github.com/OndrejSpanel/ScalaFromJS so that the tool can be used on JVM.

The tool is currently in alpha.

No release version exists so far.

To use the snapshot version in your build use:

```
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "com.github.opengrabeso" %% "esprimascala" % "0.0.1-SNAPSHOT"
```


### Conversion workflow

- switch to branch `untouched`
- place current Esprima TypeScript sources in `/ts/src`
- compile TypeScript to JavaScript (using `tsconfig.json`, in IntelliJ IDEA use context action on this file to compile)
- use `ScalaFromJS` to convert the files, result will be placed in `/ts/scala`
 - `java -jar ScalaFromJS.jar ts/esprima-convert.js ts/scala/esprima-convert.scala`
- switch to branch `port`
- merge `untouched` to `port`
- cleanup as necessary
 - once it compiles and passes tests,  commit

...
- merge into master, publish
 

---
title: Home
---

# ScalaCheck: Property-based testing for Scala

ScalaCheck is a library written in <a href="https://www.scala-lang.org/">Scala</a> and used for automated property-based testing of Scala or Java programs. ScalaCheck was originally inspired by the Haskell library <a href="https://hackage.haskell.org/package/QuickCheck">QuickCheck</a>, but has also ventured into its own.

ScalaCheck has no external dependencies other than the Scala runtime, and <a href="download.html#sbt">works</a> great with <a href="http://www.scala-sbt.org/">sbt</a>, the Scala build tool. It is also fully integrated in the test frameworks <a href="http://www.scalatest.org/">ScalaTest</a>, <a href="https://etorreborre.github.io/specs2/">specs2</a> and <a href="https://github.com/47deg/LambdaTest">LambdaTest</a>. You can also use ScalaCheck completely standalone, with its built-in test runner.

ScalaCheck is used by several prominent Scala projects, for example the <a href="https://www.scala-lang.org/">Scala compiler</a> and the <a href="http://akka.io/">Akka</a> concurrency framework.

## News

- 2019-09-18: ScalaCheck 1.14.1 released! This is the first release since the ScalaCheck repository was moved to the <a href="https://typelevel.org/">Typelevel</a> organisation. See the <a href="https://github.com/rickynils/scalacheck/releases/tag/1.14.1">release notes</a>.</li>
- 2018-04-22: <a href="download/1.14.0.html">ScalaCheck 1.14.0</a>! This release introduces support for deterministic testing. See <a href="https://github.com/rickynils/scalacheck/tree/1.14.0/RELEASE.markdown">release notes</a> and <a href="https://github.com/rickynils/scalacheck/tree/1.14.0/CHANGELOG.markdown">change log</a>.</li>
- 2016-11-01: <a href="download/1.13.4.html">ScalaCheck 1.13.4</a> and <a href="download/1.12.6.html">ScalaCheck 1.12.6</a> released! These releases fix a <a href="https://github.com/rickynils/scalacheck/issues/290">deadlock problem</a> with Scala 2.12.0. Also, as a problem was discovered with the previously released ScalaCheck 1.12.5 build, it is recommended that you upgrade to 1.12.6 or 1.13.4 even if you’re not using Scala 2.12.</li>
- 2016-10-19: <a href="download/1.13.3.html">ScalaCheck 1.13.3</a> released! See <a href="https://github.com/rickynils/scalacheck/tree/1.13.3/RELEASE">release notes</a>.</li>
- 2016-07-11: <a href="download/1.13.2.html">ScalaCheck 1.13.2</a> released! See <a href="https://github.com/rickynils/scalacheck/tree/1.13.2/RELEASE">release notes</a>.</li>
- 2016-04-14: <a href="download/1.13.1.html">ScalaCheck 1.13.1</a> released! See <a href="https://github.com/rickynils/scalacheck/tree/1.13.1/RELEASE">release notes</a>.</li>
- 2016-02-03: <a href="download/1.13.0.html">ScalaCheck 1.13.0</a> released! See <a href="https://github.com/rickynils/scalacheck/tree/1.13.0/RELEASE">release notes</a>.</li>
- 2015-09-10: Snapshot builds are no longer published on this site. Instead <a href="https://travis-ci.org/rickynils/scalacheck">Travis</a> automatically publishes all successful builds of the master branch. See <a href="download.html#snapshot">documentation</a> for more information.</li>

## Quick start

Specify some of the methods of `java.lang.String` like this:

```scala
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object StringSpecification extends Properties("String") {

  property("startsWith") = forAll { (a: String, b: String) =>
    (a+b).startsWith(a)
  }

  property("concatenate") = forAll { (a: String, b: String) =>
    (a+b).length > a.length && (a+b).length > b.length
  }

  property("substring") = forAll { (a: String, b: String, c: String) =>
    (a+b+c).substring(a.length, a.length+b.length) == b
  }

}
```

If you use <a href="http://www.scala-sbt.org/">sbt</a> add the following dependency to your build file:

```scala
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
```

Put your ScalaCheck properties in `src/test/scala`, then use the `test` task to check them:

```
$ sbt test
+ String.startsWith: OK, passed 100 tests.
! String.concat: Falsified after 0 passed tests.
> ARG_0: ""
> ARG_1: ""
+ String.substring: OK, passed 100 tests.
```

As you can see, the second property was not quite right. ScalaCheck discovers this and presents the arguments that make the property fail, two empty strings. The other two properties both pass 100 test rounds, each with a randomized set of input parameters.

You can also use ScalaCheck standalone, since it has a built-in command line test runner. Compile and run like this:

```
$ scalac -cp scalacheck_2.11-1.14.1.jar StringSpecification.scala

$ scala -cp .:scalacheck_2.11-1.14.1.jar StringSpecification
```

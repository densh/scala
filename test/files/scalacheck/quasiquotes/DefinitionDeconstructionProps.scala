import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object DefinitionDeconstructionProps extends QuasiquoteProperties("definition deconstruction") {

  property("exhaustive class matcher") = test {
    List(
      "class Foo",
      "class Foo[T]",
      "class Foo[T] @annot",
      "class Foo extends Bar with Baz",
      "class Foo { body }",
      "class Foo extends { val early = 0 } with Any",
      "abstract class Foo",
      "private[Baz] class Foo",
      "class Foo(first: A)(second: B)",
      "class Foo(first: A) extends Bar(first) with Baz",
      "class Foo private (first: A) { def bar }",
      "class Foo { self => bar(self) }"
    ).foreach { line =>
      val q"""$classMods class $name[..$targs] $ctorMods(...$argss)
              extends { ..$early } with ..$parents { $self => ..$body }""" = parse(line)
    }
  }

  property("exhaustive trait matcher") = test {
    List(
      "trait Foo",
      "trait Foo[T]",
      "trait Foo { def bar }",
      "trait Foo extends Bar with Baz",
      "trait Foo { self: Bippy => val x: Int = 1}",
      "trait Foo extends { val early: Int = 1 } with Bar { val late = early }",
      "private[Gap] trait Foo"
    ).foreach { line =>
      val q"""$mods trait $name[..$targs]
              extends { ..$early } with ..$parents { $self => ..$body }""" = parse(line)
    }
  }
}
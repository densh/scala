import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object DefinitionConstructionProps extends QuasiquoteProperties("definition construction") {

  property("splice name into trait def") = test {
    val Foo = TypeName("Foo")
    assert(q"trait $Foo" ≈ q"trait Foo")
  }

  property("splice type params into trait def") = test {
    val tparams = q"type A" :: q"type B" :: Nil
    assert(q"trait Foo[..$tparams]" ≈ q"trait Foo[A, B]")
  }

  property("splice defs into trait body") = test {
    val body = q"def foo" :: q"val bar: Baz" :: Nil
    assert(q"trait Foo { ..$body }" ≈ q"trait Foo { def foo; val bar: Baz }")
  }

  property("splice parents into trait") = test {
    val parents = tq"A" :: tq"B" :: Nil
    assert(q"trait Foo extends ..$parents" ≈ q"trait Foo extends A with B")
  }

  property("splice early valdef into trait") = test {
    val x = q"val x: Int = 1"
    assert(q"trait T extends { $x } with Any" ≈ parse("trait T extends { val x: Int = 1} with Any"))
  }

  property("construct trait with early valdef") = test {
    assert(q"trait T extends { val x: Int = 1 } with Any" ≈ parse("trait T extends { val x: Int = 1 } with Any"))
  }

  property("splice defs into early block") = test {
    val defs = q"val x: Int = 0" :: q"type Foo = Bar" :: Nil
    assert(q"trait T extends { ..$defs } with Bippy" ≈
           q"trait T extends { val x: Int = 0; type Foo = Bar} with Bippy")
  }

  property("fail on splicing of non-valid early tree") = test {
    val defn = q"def x: Int = 0"
    assertThrows[IllegalArgumentException] { q"trait T extends { $defn } with Bar" }
  }
}
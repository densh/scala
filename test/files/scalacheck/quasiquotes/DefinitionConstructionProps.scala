import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object DefinitionConstructionProps extends QuasiquoteProperties("errors") {

  property("splice name into trait def") = test {
    val Foo = TypeName("Foo")
    assert(q"trait $Foo" ≈ q"trait Foo")
  }

  // property("splice early valdef into trait") = test {
  //   val x = q"val x: Int = 1"
  //   assert(q"trait T extends { val $x } with Any" ≈ q"trait T extends { val x: Int = 1} with Any")
  // }

  property("splice type params into trait def") = test {
    val tparams = q"type A" :: q"type B" :: Nil
    assert(q"trait Foo[..$tparams]" ≈ q"trait Foo[A, B]")
  }

  property("splice defs into trait body") = test {
    val body = q"def foo" :: q"val bar: Baz" :: Nil
    assert(q"trait Foo { ..$body }" ≈ q"trait Foo { def foo; val bar: Baz }")
  }
}
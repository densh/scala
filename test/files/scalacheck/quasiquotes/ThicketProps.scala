import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object ThicketProps extends QuasiquoteProperties("term construction") {
  property("splice thicket with zero card") = test {
    val thicket = Thicket(List(q"foo", q"bar"))
    assertEqAst(q"$thicket", "{ foo; bar }")
  }

  property("splice thicket with .. card") = test {
    val thicket = Thicket(List(q"foo", q"bar"))
    assertEqAst(q"{ ..$thicket; baz }", "{ foo; bar; baz }")
  }

  property("q wraps into thickets") = test {
    def isThicket(t: Any) = t match { case _: Thicket => true case _ => false }
    assert(isThicket(q"foo"))
    assert(isThicket(q"foo; bar"))
  }
}

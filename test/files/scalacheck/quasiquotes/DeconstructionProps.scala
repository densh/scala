import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object DeconstructionProps extends Properties("deconstruction")
                              with TreeSimiliarity
                              with ArbitraryTreesAndNames {

  property("f(x)") = forAll { (f: Tree, x: Tree) =>
    val q"$f1($x1)" = q"$f($x)"
    f1 == f && x1 == x
  }

  property("f(..xs)") = forAll { (f: Tree, x1: Tree, x2: Tree) =>
    val q"$f1(..$xs)" = q"$f($x1, $x2)"
    f1 == f && xs == List(x1, x2)
  }

  property("f(y, ..ys)") = forAll { (f: Tree, x1: Tree, x2: Tree, x3: Tree) =>
    val q"$f1($y, ..$ys)" = q"$f($x1, $x2, $x3)"
    f1 == f && y == x1 && ys == List(x2, x3)
  }

  property("f(y1, y2, ..ys)") = forAll { (f: Tree, x1: Tree, x2: Tree, x3: Tree) =>
    val q"$f1($y1, $y2, ..$ys)" = q"$f($x1, $x2, $x3)"
    f1 == f && y1 == x1 && y2 == x2 && ys == List(x3)
  }

  // this should fail due to incorrect usage of ".."
  property("f(..xs, ..ys)") = forAll { (f: Tree, x1: Tree, x2: Tree, x3: Tree) =>
    val q"$f1(..$xs, ..$ys)" = q"$f($x1, $x2, $x3)"
    false
  }

  property ("f(...xss)") = forAll { (f: Tree, x1: Tree, x2: Tree) =>
    val q"$f(...$xss)" = q"$f($x1)($x2)"
    xss == List(List(x1), List(x2))
  }

}
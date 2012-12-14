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

}
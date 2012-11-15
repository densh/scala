import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._


object Test extends Properties("quasiquotes") with TreeSimiliarity with ArbitraryTrees  {

  property("splicing single tree return tree itself") =
    forAll((t: Tree) =>
      q"$t" ≈ t)

  property("splicing trees into if expression") =
    forAll((t1: Tree, t2: Tree, t3: Tree) =>
      q"if($t1) $t2 else $t3" ≈ If(t1, t2, t3))

}
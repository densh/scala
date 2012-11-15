import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._


trait ArbitraryTrees {

  implicit val arbitraryTree: Arbitrary[Tree] =
    Arbitrary(oneOf(Literal(Constant(0)), Literal(Constant("0"))))
}
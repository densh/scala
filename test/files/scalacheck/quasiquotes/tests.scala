import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._


object Test extends Properties("quasiquotes")
               with TreeSimiliarity
               with ArbitraryTreesAndNames {

  val anyRef = Select(Ident(newTermName("scala")), newTypeName("AnyRef"))

  val emtpyConstructor =
    DefDef(
      Modifiers(), nme.CONSTRUCTOR, List(),
      List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(()))))

  def classWithMethods(name: TypeName, methods: List[DefDef] = Nil) =
    ClassDef(
      Modifiers(),  name, List(),
      Template(List(anyRef), emptyValDef, List(emtpyConstructor) ++ methods))

  property("splicing single tree return tree itself") = forAll { (t: Tree) =>
    q"$t" ≈ t
  }

  property("splicing trees into if expression") = forAll { (t1: Tree, t2: Tree, t3: Tree) =>
    q"if($t1) $t2 else $t3" ≈ If(t1, t2, t3)
  }

  property("splicing term name into val") = forAll { (name: TermName) =>
    q"val $name = 0" ≈ ValDef(Modifiers(), name, TypeTree(), Literal(Constant(0)))
  }

  property("splicing type name into typedef") = forAll { (name1: TypeName, name2: TypeName) =>
    q"type $name1 = $name2" ≈ TypeDef(Modifiers(), name1, List(), Ident(name2))
  }

  property("splicing term name into class") = forAll { (name: TypeName) =>
    q"class $name" ≈ classWithMethods(name)
  }

  property("splice term name into assign") = forAll { (name: TermName) =>
    q"$name = 0" ≈ Assign(Ident(name), Literal(Constant(0)))
  }

  property("splice method into class") = forAll { (name: TypeName, method: DefDef) =>
    q"class $name { $method }" ≈ classWithMethods(name, List(method))
  }

  property("splice trees into ascriptiopn") = forAll { (t1: Tree, t2: Tree) =>
    q"$t1 : $t2" ≈ Typed(t1, t2)
  }
}

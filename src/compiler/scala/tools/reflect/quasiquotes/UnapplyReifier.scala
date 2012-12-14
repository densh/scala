package scala.tools.reflect
package quasiquotes

import scala.reflect.reify.{Reifier => ReflectReifier}
import scala.reflect.macros
import scala.collection.mutable


abstract class UnapplyReifier extends ReflectReifier with Types {
  import global._

  val placeholders: Set[String]
  val correspondingTypes: mutable.Map[String, Type] = mutable.Map()

  override def reifyTree(tree: Tree) = reifyBasicTree(tree)

  override def reifyBasicTree(tree: Tree): Tree = tree match {
    case Ident(name) if placeholders.contains(name.toString) =>
      correspondingTypes(name.toString) = treeType
      Bind(name, Ident(nme.WILDCARD))
    case global.emptyValDef =>
      mirrorFactoryCall("EmptyValDefLike")
    case global.pendingSuperCall =>
      mirrorFactoryCall("PendingSuperCallLike")
    case _ =>
      super.reifyBasicTree(tree)
  }

  override def scalaFactoryCall(name: String, args: Tree*): Tree =
    call("scala." + name, args: _*)

  override def reifyName(name: Name): Tree = {
    if(!placeholders.contains(name.toString))
      super.reifyName(name)
    else {
      correspondingTypes(name.toString) = nameType
      Bind(TermName(name.toString), Ident(nme.WILDCARD))
    }
  }
  override def reifyModifiers(m: global.Modifiers) =
    mirrorFactoryCall(nme.Modifiers, mirrorFactoryCall("FlagsAsBits", reify(m.flags)), reify(m.privateWithin), reify(m.annotations))
}


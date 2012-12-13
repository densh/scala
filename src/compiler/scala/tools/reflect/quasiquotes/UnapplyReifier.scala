package scala.tools.reflect
package quasiquotes

import scala.reflect.reify.{Reifier => ReflectReifier}
import scala.reflect.macros
import scala.collection.mutable


abstract class UnapplyReifier extends ReflectReifier with Types {
  import global._

  val placeholders: Set[String]
  val correspondingTypes: mutable.Map[String, Type] = mutable.Map()

  override def reifyTreeCore(tree: Tree): Tree = tree match {
    case Ident(name) if placeholders.contains(name.toString) =>
      correspondingTypes(name.toString) = treeType
      Bind(name, Ident(nme.WILDCARD))
    case _ =>
      super.reifyTreeCore(tree)
  }

  override def scalaFactoryCall(name: String, args: Tree*): Tree =
    call("scala." + name, args: _*)

  override def reifyName(name: Name): Tree = {
    if(!placeholders.contains(name.toString))
      super.reifyName(name)
    else {
      correspondingTypes(name.toString) =
        if(name.isTypeName)
          typeNameType
        else
          termNameType
      Bind(name, Ident(nme.WILDCARD))
    }
  }
}


package scala.tools.reflect
package quasiquotes

import scala.reflect.api.Universe


trait Types {

  val currentUniverse: Universe
  import currentUniverse._
  val currentMirror: Mirror
  val universe: Tree

  lazy val universeType = universe.tpe
  lazy val nameType = memberType(universeType, "Name")
  lazy val treeType = memberType(universeType, "Tree")
  lazy val typeDefType = memberType(universeType, "TypeDef")
  lazy val liftableType = currentMirror.staticClass("scala.reflect.api.Liftable").toType.asInstanceOf[Type]
  lazy val listType = currentMirror.staticClass("scala.collection.immutable.List").toType.asInstanceOf[Type]
  lazy val listTreeType = appliedType(listType, List(treeType))
  lazy val typeDefListType = appliedType(listType, List(typeDefType))

  def memberType(thistype: Type, name: String): Type = {
    val sym = thistype.typeSymbol.typeSignature.member(newTypeName(name))
    sym.asType.toType.typeConstructor.asSeenFrom(thistype, sym.owner)
  }
}
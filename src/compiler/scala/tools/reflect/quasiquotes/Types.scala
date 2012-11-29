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
  lazy val liftableType = currentMirror.staticClass("scala.reflect.api.Liftable").toType
  lazy val listType = currentMirror.staticClass("scala.collection.immutable.List").toType
  lazy val optionType = currentMirror.staticClass("scala.Option").toType
  lazy val listTreeType = appliedType(listType, List(treeType))
  lazy val typeDefListType = appliedType(listType, List(typeDefType))
  lazy val optionTreeType = appliedType(optionType, List(treeType))
  lazy val optionNameType = appliedType(optionType, List(nameType))

  def memberType(thistype: Type, name: String): Type = {
    val sym = thistype.typeSymbol.typeSignature.member(newTypeName(name))
    sym.asType.toType.typeConstructor.asSeenFrom(thistype, sym.owner)
  }
}
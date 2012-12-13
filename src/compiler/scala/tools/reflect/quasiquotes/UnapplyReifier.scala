package scala.tools.reflect
package quasiquotes

import scala.reflect.reify.{Reifier => ReflectReifier}
import scala.reflect.macros


abstract class UnapplyReifier extends ReflectReifier with Types {
  import global._

  val subsmap: Map[String, Tree]
  val ctx: macros.Context

  object SubsToTree {
    def unapply(name: Name): Option[Tree] =
      subsmap.get(name.encoded)
  }

  override def reifyTree(tree: Tree): Tree = tree match {
    case Ident(SubsToTree(tree)) => tree
    // case Ident(SubsToLiftable(tree)) => tree
    // case Block(List(), SubsToListTree(listtree)) =>
    //   Block(Select(listtree, newTermName("init")), Select(listtree, newTermName("last")))
    case emptyValDef: AnyRef if emptyValDef eq ctx.universe.emptyValDef =>
      mirrorBuildSelect("emptyValDef")
    case EmptyTree =>
      reifyMirrorObject(EmptyTree)
    case Literal(const @ Constant(_)) =>
      mirrorCall("Literal", reifyProduct(const))
    case Import(tree, selectors) =>
      val args = mkList(selectors.map(s => reifyProduct(s)))
      mirrorCall("Import", reify(tree), args)
    case _ =>
      reifyProduct(tree)
  }

  override def scalaFactoryCall(name: String, args: Tree*): Tree =
    call("scala." + name, args: _*)

  // override def reifyName(name: Name): Tree = {
  //   if(!subsmap.contains(name.encoded)) {
  //     val factory =
  //       if (name.isTypeName)
  //         "newTypeName"
  //       else
  //         "newTermName"
  //     mirrorCall(factory, Literal(Constant(name.toString)))
  //   } else
  //     name match {
  //       case SubsToNameTree(tree) => tree
  //       case _ => throw new Exception(s"Name expected but ${subsmap(name.encoded).tpe} found")
  //     }
  // }

  // override def reifyList(xs: List[Any]): Tree =
  //   Select(
  //     mkList(xs.map { _ match {
  //       case SubsToTypeDef(typedef) => mkList(List(typedef))
  //       case SubsToTypeDefList(typedefs) => typedefs
  //       case Ident(SubsToListTree(listtree)) => listtree
  //       case x @ _ => mkList(List(reify(x)))
  //     }}),
  //     newTermName("flatten"))

}
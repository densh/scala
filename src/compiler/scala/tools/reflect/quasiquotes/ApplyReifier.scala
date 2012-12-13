package scala.tools.reflect
package quasiquotes

import scala.reflect.reify.{Reifier => ReflectReifier}
import scala.reflect.macros


abstract class ApplyReifier extends ReflectReifier with Types {
  import global._

  val subsmap: Map[String, Tree]
  val ctx: macros.Context

  object SubsToLiftable {

    def unapply(name: Name): Option[Tree] =
      subsmap.get(name.encoded) match {
        case Some(tree) =>
          val liftType = appliedType(liftableType, List(tree.tpe))
          val lift = ctx.inferImplicitValue(liftType.asInstanceOf[ctx.Type], silent = true).asInstanceOf[Tree]
          if(lift != EmptyTree) {
            Some(wrapLift(lift, tree))
          } else
            None
        case None => None
      }

    def wrapLift(lift: Tree, tree: Tree) =
      TypeApply(
        Select(Apply(lift, List(universe, tree)), newTermName("asInstanceOf")),
        List(Select(Ident(newTermName("$u")), newTypeName("Tree"))))
  }

  object SubsToTree {

    def unapply(name: Name): Option[Tree] =
      subsmap.get(name.encoded).flatMap { tree =>
        if(tree.tpe <:< treeType)
          Some(tree)
        else
          None
      }
  }

  object SubsToNameTree {

    def unapply(name: Name): Option[Tree] =
      subsmap.get(name.encoded).flatMap { tree =>
        if(tree.tpe <:< nameType)
          Some(tree)
        else {
          println("not a name")
          println(showRaw(tree.tpe, printIds=true, printTypes=true))
          None
        }
      }
  }

  object SubsToListTree {

    def unapply(name: Name): Option[Tree] =
      subsmap.get(name.encoded).flatMap { tree =>
        if(tree.tpe <:< listTreeType)
          Some(tree)
        else
          None
      }
  }

  def isNothingTree(t: Tree) =
    t.equalsStructure(Select(Select(Ident(nme.ROOTPKG), newTermName("scala")), newTypeName("Nothing")))

  def isAnyTree(t: Tree) =
    t.equalsStructure(Select(Select(Ident(nme.ROOTPKG), newTermName("scala")), newTypeName("Any")))

  object SubsToTypeDefList {

    def unapply(any: Any): Option[Tree] = any match {
      case TypeDef(_, name, List(), TypeBoundsTree(lo, hi)) if isNothingTree(lo) && isAnyTree(hi) =>
        subsmap.get(name.encoded).flatMap { tree =>
          if(tree.tpe <:< typeDefListType)
            Some(tree)
          else
            None
        }
      case _ => None
    }
  }

  object SubsToTypeDef {

    def unapply(any: Any): Option[Tree] = any match {
      case TypeDef(_, name, List(), TypeBoundsTree(lo, hi)) if isNothingTree(lo) && isAnyTree(hi) =>
        subsmap.get(name.encoded).flatMap { tree =>
          if(tree.tpe <:< typeDefType)
            Some(tree)
          else
            None
        }
      case _ => None
    }
  }

  override def reifyTreeCore(tree: Tree): Tree = tree match {
    case Ident(SubsToTree(tree)) => tree
    case Ident(SubsToLiftable(tree)) => tree
    case _ => super.reifyTreeCore(tree)
  }

  override def reifyName(name: Name): Tree =
    if(!subsmap.contains(name.encoded))
      super.reifyName(name)
    else
      name match {
        case SubsToNameTree(tree) => tree
        case _ => throw new Exception(s"Name expected but ${subsmap(name.encoded).tpe} found")
    }

  override def reifyList(xs: List[Any]): Tree =
    Select(
      mkList(xs.map { _ match {
        case SubsToTypeDef(typedef) => mkList(List(typedef))
        case SubsToTypeDefList(typedefs) => typedefs
        case Ident(SubsToListTree(listtree)) => listtree
        case x @ _ => mkList(List(reify(x)))
      }}),
      newTermName("flatten"))

}
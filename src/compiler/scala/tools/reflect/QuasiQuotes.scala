package scala.tools
package reflect

import scala.tools.nsc.Global
import scala.tools.nsc.ast.parser.Parsers
import scala.compat.Platform.EOL
import scala.reflect.internal.util.{BatchSourceFile, SourceFile}
import scala.reflect.reify.Reifier

import scala.reflect.api._
import scala.reflect.macros
import scala.collection.mutable


abstract class QuasiQuoteApply {

  val ctx: macros.Context
  import ctx.universe._
  val g = ctx.universe.asInstanceOf[Global]

  val qqprefix = "$quasiquote$"
  val qqdebug = true

  val (universe, args, parts) =
    ctx.macroApplication match {
      case Apply(Select(Select(Apply(Select(universe, _), List(Apply(_, parts0))), _), _), args) =>
        val parts = parts0.map(_ match {
          case Literal(Constant(s: String)) => s
          case _ => throw QuasiQuoteException("Quasi-quotes can only be used with constant string arguments.")
        })
        if(args.length != parts.length - 1)
          throw QuasiQuoteException("Imbalanced amount of arguments.")
        (universe, args, parts)
      case _ => throw QuasiQuoteException("Couldn't parse call prefix tree.")
    }

  val (code, subsmap) = {
    val sb = new StringBuilder(parts.head)
    val subsmap = mutable.Map[String, Tree]()
    for((tree, part) <- args.zip(parts.tail)) {
      val placeholder = ctx.fresh(qqprefix)
      sb.append(placeholder)
      sb.append(part)
      subsmap(placeholder) = tree
    }
    (sb.toString, subsmap.toMap)
  }

  if(qqdebug) println(s"\ncode to parse=\n$code\n")

  val tree = parse(code)

  if(qqdebug) println(s"parsed tree\n=${tree}\n=${showRaw(tree)}\n")

  val reified = reifyTree(tree)

  if(qqdebug) println(s"reified tree\n=${reified}\n=${showRaw(reified)}\n")

  val result = wrap(reified)

  if(qqdebug) println(s"result tree\n=${result}\n=${showRaw(result)}\n")


  def parse(code: String): Tree = {
    val wrappedCode = "object wrapper {" + EOL + code + EOL + "}"
    val file = new BatchSourceFile("<quasiquotes>", wrappedCode)
    val parser = new { val global: g.type = g } with QuasiQuoteParser
    val wrappedTree = parser.parse(file)
    val g.PackageDef(_, List(g.ModuleDef(_, _, g.Template(_, _, _ :: parsed)))) = wrappedTree
    (parsed match {
      case tree :: Nil => tree
      case stats :+ tree => g.Block(stats, tree)
    }).asInstanceOf[ctx.universe.Tree]
  }

  def reifyTree(tree: Tree) = {
    val ctx0 = ctx
    val subsmap0 = subsmap
    val universe0 = universe
    val reifier = new {
      val global: g.type = g
      val typer: g.analyzer.Typer = null
      val universe: g.Tree = universe0.asInstanceOf[g.Tree]
      val mirror: g.Tree = g.EmptyTree
      val reifee: Any = null
      val concrete: Boolean = false
      val subsmap: Map[String, g.Tree] = subsmap0.map(pair => pair._1 -> pair._2.asInstanceOf[g.Tree])
      val ctx: ctx0.type = ctx0
    } with QuasiQuoteReifier
    reifier.reifyTree(tree.asInstanceOf[g.Tree]).asInstanceOf[ctx.universe.Tree]
  }

  def wrap(t: Tree) =
    Block(
      List(ValDef(Modifiers(),
        g.nme.UNIVERSE_SHORT.asInstanceOf[TermName],
        SingletonTypeTree(universe),
        universe)),
      t)
}

abstract class QuasiQuoteParser extends Parsers {

  def parse(file: SourceFile): global.Tree = new QuasiQuoteSourceParser(file).parse()

  class QuasiQuoteSourceParser(source0: SourceFile) extends SourceFileParser(source0)
}

abstract class QuasiQuoteReifier extends Reifier {
  import global._

  val subsmap: Map[String, Tree]
  val ctx: macros.Context

  val universeType = universe.tpe
  val nameType = memberType(universeType, "Name")
  val treeType = memberType(universeType, "Tree")
  val liftableType = ctx.mirror.staticClass("scala.reflect.api.Liftable").toType.asInstanceOf[Type]
  val listType = ctx.mirror.staticClass("scala.collection.immutable.List").toType.asInstanceOf[Type]
  val listTreeType = appliedType(listType, List(treeType))

  def memberType(thistype: Type, name: String): Type = {
    val sym = thistype.typeSymbol.typeSignature.member(newTypeName(name))
    sym.asType.toType.typeConstructor.asSeenFrom(thistype, sym.owner)
  }

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
        else
          None
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

  override def reifyTree(tree: Tree): Tree = tree match {
    case Ident(SubsToTree(tree)) => tree
    case Ident(SubsToLiftable(tree)) => tree
    // case Block(List(), SubsToListTree(listtree)) =>
    //   Block(Select(listtree, newTermName("init")), Select(listtree, newTermName("last")))
    // case emptyValDef =>
    //   mirrorBuildSelect("emptyValDef")
    case EmptyTree =>
      reifyMirrorObject(EmptyTree)
    case Literal(const @ Constant(_)) =>
      mirrorCall("Literal", reifyProduct(const.asInstanceOf[Product]))
    case Import(tree, selectors) =>
      val args = mkList(selectors.map(s => reifyProduct(s.asInstanceOf[Product])))
      mirrorCall("Import", reify(tree), args)
    case _ =>
      reifyProduct(tree.asInstanceOf[Product])
  }

  override def reifyName(name: Name): Tree = {
    if(!subsmap.contains(name.encoded)) {
      val factory =
        if (name.isTypeName)
          "newTypeName"
        else
          "newTermName"
      mirrorCall(factory, Literal(Constant(name.toString)))
    } else
      name match {
        case SubsToNameTree(tree) => tree
        case _ => throw QuasiQuoteException(s"Name expected but ${subsmap(name.encoded).tpe} found")
      }
  }

  override def reifyList(xs: List[Any]): Tree =
    Select(
      mkList(xs.map { _ match {
        case Ident(SubsToListTree(listtree)) => listtree
        case x @ _ => mkList(List(reify(x)))
      }}),
      newTermName("flatten"))

}
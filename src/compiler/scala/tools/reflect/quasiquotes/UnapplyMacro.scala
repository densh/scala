package scala.tools.reflect
package quasiquotes

import java.util.UUID.randomUUID
import scala.tools.nsc.Global
import scala.collection.mutable
import scala.reflect.reify.{Reifier => ReflectReifier}
import scala.reflect.macros




abstract class UnapplyMacro extends Types {
  val ctx: macros.Context
  val g = ctx.universe.asInstanceOf[Global]
  import ctx.universe._
  import ctx.universe.Flag._

  class MyBind(name: Name, tree: Tree) extends g.Bind(name.asInstanceOf[g.Name], tree.asInstanceOf[g.Tree]) {
    override def hashCode = throw new Exception("i hate you")
    override def equals(other: Any) = throw new Exception("i hate you")
  }


  val currentUniverse: ctx.universe.type = ctx.universe
  val currentMirror = ctx.mirror

  var binds = {
    val contextTree = ctx.asInstanceOf[scala.reflect.macros.runtime.Context].callsiteTyper.context.tree.asInstanceOf[Tree]
    val CaseDef(Apply(_, binds0), _, _) = contextTree
    binds0.map(_.asInstanceOf[Bind].duplicate)
  }

  if(Const.debug) println(s"reified tree\n=${binds}\n=${showRaw(binds)}\n")

  val (universe, parts) =
    ctx.macroApplication match {
      case Apply(Select(Select(Apply(Select(universe, _), List(Apply(_, parts0))), _), _), _) =>
        val parts = parts0.map(_ match {
          case Literal(Constant(s: String)) => s
          case _ => throw new Exception("")
        })
        (universe, parts)
      case _ => throw new Exception("")
    }

  if(!(parts.length >= 1 && parts.length <= 23))
    throw new Exception("Inapropriate amount of quasiquote params.")

  val unapplySelector = Ident(newTermName("<unapply-selector>"))
  unapplySelector.setType(treeType)

  val (code, subsmap) = {
    val sb = new StringBuilder(parts.head)
    val subsmap = mutable.Map[String, Tree]()
    for((tree, part) <- binds.zip(parts.tail)) {
      val placeholder = ctx.fresh(Const.prefix)
      sb.append(placeholder)
      sb.append(part)
      subsmap(placeholder) = tree
    }
    (sb.toString, subsmap.toMap)
  }

  if(Const.debug) println(s"subsmap\n=${showRaw(subsmap.toList)}\n")

  val tree = parse(code)
  val reifiedTree = reifyTree(tree, subsmap)

  if(Const.debug) println(s"\ncode to parse=\n$code\n")
  if(Const.debug) println(s"parsed tree\n=${tree}\n=${showRaw(tree)}\n")
  if(Const.debug) println(s"reified tree\n=${reifiedTree}\n=${showRaw(reifiedTree)}\n")

  val tuple = Apply(Ident("Tuple" + (binds.length)), binds.map(b => Ident(b.name)).toList)
  val caseBody = Apply(Ident("Some"), List(tuple))

  val unapplyBody =
    Match(Ident(newTermName("tree")), List(CaseDef(reifiedTree, EmptyTree, caseBody)))

  val localTreeType = Select(Ident(newTermName("$u")), newTypeName("Tree"))

  val unapplyResultType: Tree = {
    val tuple = Ident(newTypeName("Tuple" + binds.length))
    val treetuple = AppliedTypeTree(tuple, binds.map(b => localTreeType))
    val optiontreetuple = AppliedTypeTree(Ident(newTypeName("Option")), List(treetuple))
    optiontreetuple
  }

  //val apiUniverseType = TypeTree(ctx.mirror.staticClass("scala.reflect.api.Universe").toType)
  //TypeTree(g.definitions.ApiUniverseClass.toType.asInstanceOf[Type])

  val apiUniverseType = TypeTree(universeType)//Select(Select(Select(Ident(newTermName("scala")), newTermName("reflect")), newTermName("api")), newTypeName("Universe"))

  val unapplyMethod =
    DefDef(
      Modifiers(), newTermName("unapply"), List(),
      List(
        List(ValDef(Modifiers(PARAM), newTermName("$u"), apiUniverseType, EmptyTree)),
        List(ValDef(Modifiers(PARAM), newTermName("tree"), localTreeType, EmptyTree))),
      unapplyResultType,
      unapplyBody)

  val moduleName = newTermName(Const.prefix + "matcher$" + randomUUID().toString.replace("-", ""))

  val moduleDef =
    ModuleDef(Modifiers(), moduleName, Template(
      List(Select(Ident(newTermName("scala")), newTypeName("AnyRef"))),
      emptyValDef,
      List(
        DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))),
        unapplyMethod)))

  println(s"\n\nMODULEDEF\n${showRaw(moduleDef, printTypes=true, printIds=true)}")

  ctx.introduceTopLevel(moduleDef)

  val result = Apply(Apply(Select(Ident(moduleName), newTermName("unapply")), List(universe)), List(unapplySelector))

  def parse(code: String) = {
    val parser = new { val global: g.type = g } with Parser
    val tree = parser.parse(code)
    tree.asInstanceOf[ctx.universe.Tree]
  }

  def reifyTree(tree: Tree, subsmap0: Map[String, Tree]) = {
    val ctx0 = ctx
    val universe0 = universe
    val reifier = new {
      val global: g.type = g
      val universe: g.Tree = universe0.asInstanceOf[g.Tree]
      val subsmap: Map[String, g.Tree] = subsmap0.map(pair => pair._1 -> pair._2.asInstanceOf[g.Tree])
      val ctx: ctx0.type = ctx0
      val typer: g.analyzer.Typer = null
      val mirror: g.Tree = g.EmptyTree
      val reifee: Any = null
      val concrete: Boolean = false
    } with UnapplyReifier
    reifier.reifyTree(tree.asInstanceOf[g.Tree]).asInstanceOf[ctx.universe.Tree]
  }
}


abstract class UnapplyReifier extends ReflectReifier with Types {
  import global._

  val subsmap: Map[String, Tree]
  val ctx: macros.Context

  val currentUniverse: global.type = global
  val currentMirror = ctx.mirror.asInstanceOf[global.Mirror]

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
      mirrorCall("Literal", reifyProduct(const.asInstanceOf[Product]))
    case Import(tree, selectors) =>
      val args = mkList(selectors.map(s => reifyProduct(s.asInstanceOf[Product])))
      mirrorCall("Import", reify(tree), args)
    case _ =>
      reifyProduct(tree.asInstanceOf[Product])
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
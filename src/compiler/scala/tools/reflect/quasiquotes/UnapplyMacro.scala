package scala.tools.reflect
package quasiquotes

import java.util.UUID.randomUUID
import scala.tools.nsc.Global
import scala.reflect.macros.runtime.Context
import scala.collection.mutable


abstract class UnapplyMacro extends Types {
  val ctx: Context
  val global: ctx.universe.type = ctx.universe
  import ctx.universe._
  import ctx.universe.Flag._

  var binds = {
    val CaseDef(Apply(_, binds0), _, _) = ctx.callsiteTyper.context.tree
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
  val reifiedTree = reifyTree(tree)

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
  //TypeTree(ctx.universe.definitions.ApiUniverseClass.toType.asInstanceOf[Type])

  val apiUniverseType = Select(Select(Select(Ident(newTermName("scala")), newTermName("reflect")), newTermName("api")), newTypeName("Universe"))

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
    val parser = new { val global: ctx.universe.type = ctx.universe } with Parser
    parser.parse(code)
  }

  def reifyTree(tree: Tree) = {
    val ctx0 = ctx
    val subsmap0 = subsmap
    val universe0 = universe
    val reifier = new {
      val ctx: ctx0.type = ctx0
      val global: ctx0.universe.type = ctx0.universe
      val universe = universe0.asInstanceOf[global.Tree]
      val subsmap: Map[String, global.Tree] = subsmap0.map(pair => pair._1 -> pair._2.asInstanceOf[global.Tree])
      val mirror = EmptyTree.asInstanceOf[global.Tree]
      val typer = null
      val reifee = null
      val concrete = false
    } with UnapplyReifier
    reifier.reifyTree(tree.asInstanceOf[reifier.global.Tree]).asInstanceOf[Tree]
  }
}

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

  val unapplySelector = Ident(TermName("<unapply-selector>"))
  unapplySelector.setType(treeType)

  val (code, placeholders) = {
    val sb = new StringBuilder(parts.head)
    val placeholders = mutable.ListBuffer[String]()
    for(part <- parts.tail) {
      val placeholder = ctx.fresh(Const.prefix)
      sb.append(placeholder)
      sb.append(part)
      placeholders += placeholder
    }
    (sb.toString, placeholders.toList)
  }

  val tree = parse(code)
  val (reifiedTree, correspondingTypes) = reifyTree(tree)

  if(Const.debug) println(s"\ncorresponding types=\n$correspondingTypes\n")
  if(Const.debug) println(s"\ncode to parse=\n$code\n")
  if(Const.debug) println(s"parsed tree\n=${tree}\n=${showRaw(tree)}\n")
  if(Const.debug) println(s"reified tree\n=${reifiedTree}\n=${showRaw(reifiedTree)}\n")

  val caseBody: Tree =
    if(placeholders.length == 0)
      Literal(Constant(true))
    else if(placeholders.length == 1)
      Apply(Ident(TermName("Some")), List(Ident(TermName(placeholders(0)))))
    else
      Apply(
        Ident(TermName("Some")),
        List(Apply(
          Ident(TermName("Tuple" + placeholders.length)),
          placeholders.map(p => Ident(TermName(p))))))

  val unapplyBody =
    Block(
      List(Import(Ident(TermName("$u")), List(ImportSelector(nme.WILDCARD, 0, null, 0)))),
      Match(Ident(TermName("tree")), List(CaseDef(reifiedTree, EmptyTree, caseBody))))

  val localTreeType = Select(Ident(TermName("$u")), TypeName("Tree"))

  val unapplyResultType: Tree =
    if(placeholders.length == 0){
      Ident(TypeName("Boolean"))
    } else if(placeholders.length == 1){
      AppliedTypeTree(Ident(TypeName("Option")), List(correspondingTypes(placeholders(0))))
    } else {
      val tuple = Ident(TypeName("Tuple" + placeholders.length))
      val treetuple = AppliedTypeTree(tuple, placeholders.map(p => correspondingTypes(p)))
      val optiontreetuple = AppliedTypeTree(Ident(TypeName("Option")), List(treetuple))
      optiontreetuple
    }

  val apiUniverseType = Select(Select(Select(Ident(TermName("scala")), TermName("reflect")), TermName("api")), TypeName("Universe"))

  val unapplyMethod =
    DefDef(
      Modifiers(), TermName("unapply"), List(),
      List(
        List(ValDef(Modifiers(PARAM), TermName("$u"), apiUniverseType, EmptyTree)),
        List(ValDef(Modifiers(PARAM), TermName("tree"), localTreeType, EmptyTree))),
      unapplyResultType,
      unapplyBody)

  val moduleName = TermName(Const.prefix + "matcher$" + randomUUID().toString.replace("-", ""))

  val moduleDef =
    ModuleDef(Modifiers(), moduleName, Template(
      List(Select(Ident(TermName("scala")), TypeName("AnyRef"))),
      emptyValDef,
      List(
        DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))),
        unapplyMethod)))

  if(Const.debug) println(s"\nmoduledef\n=${showRaw(moduleDef, printTypes=true, printIds=true)}\n=$moduleDef\n")

  ctx.introduceTopLevel(moduleDef)

  val result = Apply(Apply(Select(Ident(moduleName), TermName("unapply")), List(universe)), List(unapplySelector))

  def parse(code: String) = {
    val parser = new { val global: ctx.universe.type = ctx.universe } with Parser
    parser.parse(code)
  }

  def reifyTree(tree: Tree) = {
    val ctx0 = ctx
    val placeholders0 = placeholders
    val universe0 = universe
    val reifier = new {
      val ctx: ctx0.type = ctx0
      val global: ctx0.universe.type = ctx0.universe
      val universe = universe0.asInstanceOf[global.Tree]
      val placeholders: Set[String] = placeholders0.toSet
      val mirror = EmptyTree.asInstanceOf[global.Tree]
      val typer = null
      val reifee = null
      val concrete = false
    } with UnapplyReifier
    val reifiedtree = reifier.reifyTreeCore(tree.asInstanceOf[reifier.global.Tree]).asInstanceOf[Tree]
    val correspondingTypes = reifier.correspondingTypes.map { pair =>
      val tpe = pair._2.asInstanceOf[global.Type]
      if(tpe =:= termNameType)
        (pair._1, Select(Ident(TermName("$u")), TypeName("TermName")))
      else if(tpe =:= typeNameType)
        (pair._1, Select(Ident(TermName("$u")), TypeName("TypeName")))
      else if(tpe =:= treeType)
        (pair._1, Select(Ident(TermName("$u")), TypeName("Tree")))
      else
        throw new Exception("Unexpected reified type.")
    }
    (reifiedtree, correspondingTypes)
  }
}

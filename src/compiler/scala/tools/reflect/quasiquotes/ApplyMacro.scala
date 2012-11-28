package scala.tools.reflect
package quasiquotes

import scala.tools.nsc.Global
import scala.reflect.macros
import scala.collection.mutable


abstract class ApplyMacro {

  val ctx: macros.Context
  import ctx.universe._
  val g = ctx.universe.asInstanceOf[Global]

  val (universe, args, parts) =
    ctx.macroApplication match {
      case Apply(Select(Select(Apply(Select(universe, _), List(Apply(_, parts0))), _), _), args) =>
        val parts = parts0.map(_ match {
          case Literal(Constant(s: String)) => s
          case _ => throw new Exception("Quasi-quotes can only be used with constant string arguments.")
        })
        if(args.length != parts.length - 1)
          throw new Exception("Imbalanced amount of arguments.")
        (universe, args, parts)
      case _ => throw new Exception("Couldn't parse call prefix tree.")
    }

  val (code, subsmap) = {
    val sb = new StringBuilder(parts.head)
    val subsmap = mutable.Map[String, Tree]()
    for((tree, part) <- args.zip(parts.tail)) {
      val placeholder = ctx.fresh(Const.prefix)
      sb.append(placeholder)
      sb.append(part)
      subsmap(placeholder) = tree
    }
    (sb.toString, subsmap.toMap)
  }

  if(Const.debug) println(s"\ncode to parse=\n$code\n")

  val tree = parse(code)

  if(Const.debug) println(s"parsed tree\n=${tree}\n=${showRaw(tree)}\n")

  val reified = reifyTree(tree)

  if(Const.debug) println(s"reified tree\n=${reified}\n=${showRaw(reified)}\n")

  val result = wrap(reified)

  if(Const.debug) println(s"result tree\n=${result}\n=${showRaw(result)}\n")

  def parse(code: String) = {
    val parser = new { val global: g.type = g } with Parser
    val tree = parser.parse(code)
    tree.asInstanceOf[ctx.universe.Tree]
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
    } with Reifier
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
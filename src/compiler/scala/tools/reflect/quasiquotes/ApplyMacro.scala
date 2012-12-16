package scala.tools.reflect
package quasiquotes

import scala.tools.nsc.Global
import scala.reflect.macros.runtime.Context
import scala.collection.mutable
import scala.reflect.internal.util.Collections._


abstract class ApplyMacro { self =>
  val ctx: Context
  import ctx.universe._

  val (universe, args, parts) =
    ctx.macroApplication match {
      case Apply(Select(Select(Apply(Select(universe, _), List(Apply(_, parts0))), _), _), args) =>
        val parts = parts0.map{
          case Literal(Constant(s: String)) => s
          case part => ctx.abort(part.pos, "Quasi-quotes can only be used with constant string arguments.")
        }
        if(args.length != parts.length - 1)
          ctx.abort(ctx.enclosingPosition, "Imbalanced amount of arguments.")
        (universe, args, parts)
      case _ => ctx.abort(ctx.macroApplication.pos, "Couldn't parse call prefix tree ${ctx.macroApplication}.")
    }

  val (code, subsmap) = {
    val sb = new StringBuilder(parts.head)
    val subsmap = mutable.Map[String, Tree]()
    map2(args, parts.tail)((tree, part) => {
      val placeholder = ctx.fresh(nme.QUASIQUOTE_PREFIX)
      sb.append(placeholder)
      sb.append(part)
      subsmap(placeholder) = tree
    })
    (sb.toString, subsmap.toMap)
  }

  if(settings.Yquasiquotedebug.value) println(s"\ncode to parse=\n$code\n")

  val tree = parse(code)

  if(settings.Yquasiquotedebug.value) println(s"parsed tree\n=${tree}\n=${showRaw(tree)}\n")

  val reified = reifyTree(tree)

  if(settings.Yquasiquotedebug.value) println(s"reified tree\n=${reified}\n=${showRaw(reified)}\n")

  val result = wrap(reified)

  if(settings.Yquasiquotedebug.value) println(s"result tree\n=${result}\n=${showRaw(result)}\n")

  def parse(code: String) = {
    val parser = new {
      val global: ctx.universe.type = ctx.universe
      val placeholders = self.subsmap.keys.toSet
    } with Parser
    parser.parse(code)
  }

  def reifyTree(tree: Tree) = {
    val ctx0 = ctx
    val subsmap0 = subsmap
    val universe0 = universe
    val reifier = new {
      val ctx: ctx0.type = ctx0
      val global: ctx0.universe.type = ctx0.universe
      val subsmap: Map[String, global.Tree] = subsmap0.map(pair => pair._1 -> pair._2.asInstanceOf[global.Tree])
      val universe = universe0.asInstanceOf[global.Tree]
      val mirror = global.EmptyTree
      val typer = null
      val reifee = null
      val concrete = false
    } with ApplyReifier
    reifier.reify(tree.asInstanceOf[reifier.global.Tree]).asInstanceOf[Tree]
  }

  def wrap(t: Tree) =
    Block(
      List(ValDef(Modifiers(),
        nme.UNIVERSE_SHORT,
        SingletonTypeTree(universe),
        universe)),
      t)
}
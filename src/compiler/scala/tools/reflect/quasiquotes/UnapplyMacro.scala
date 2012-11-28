package scala.tools.reflect
package quasiquotes

import java.util.UUID.randomUUID
import scala.tools.nsc.Global
import scala.reflect.macros
import scala.collection.mutable


abstract class UnapplyMacro {//with Types {
  val ctx: macros.Context
  import ctx.universe._

  print(ctx.macroApplication)

  // val currentUniverse: ctx.universe.type = ctx.universe
  // val currentMirror = ctx.mirror
  // val universe =

  // val selector = Ident(newTermName("<unapply-selector>"))
  // selector.setType(c.typeOf[treeType])
  // Apply(Apply(Select(Ident(name), newTermName("unapply")), List(universe)), List(selector))

  val result = EmptyTree
}

// abstract class QuasiQuoteQ extends QuasiQuoteConst {
//   val ctx: macros.Context
//   import ctx.universe._
//   import ctx.universe.Flag._

//   val g = ctx.universe.asInstanceOf[Global]
//   import g.definitions.{AnyClass, scalaRepeatedType}

//   val name = newTermName(Const.prefix + "q")

//   val quasiquoteStatic = ctx.mirror.staticClass("scala.tools.reflect.QuasiQuoteStatic").typeSignature

//   val unapplyImplRef = Ident(quasiquoteStatic.member(newTermName("apply")))

//   val applyImplRef = Ident(quasiquoteStatic.member(newTermName("unapply")))

//   val moduledef =
//     ModuleDef(NoMods, name, Template(
//       List(Select(Ident(newTermName("scala")), newTypeName("AnyRef"))),
//       emptyValDef,
//       List(
//         DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))),
//         Import(Select(Ident(newTermName("language")), newTermName("experimental")), List(ImportSelector(newTermName("macros"), 41, newTermName("macros"), 41))),
//         DefDef(Modifiers(MACRO), newTermName("unapply"), List(), List(List(ValDef(Modifiers(PARAM), newTermName("tree"), Ident(newTypeName("Any")), EmptyTree))), Ident(newTypeName("Any")), unapplyImplRef),
//         DefDef(Modifiers(MACRO), newTermName("apply"), List(), List(List(ValDef(Modifiers(PARAM), newTermName("args"), TypeTree(scalaRepeatedType(AnyClass.toType).asInstanceOf[Type]), EmptyTree))), Ident(newTypeName("Any")), applyImplRef)
//       )
//     ))

//   if(!ctx.existsTopLevel(name))
//     ctx.introduceTopLevel(moduledef)

//   val result = Ident(name)
// }

// object QuasiQuoteStatic {
//   def apply(c: macros.Context)(args: c.Expr[Any]*) = c.Expr((new { val ctx: c.type = c } with QuasiQuoteApply).result)
//   def unapply(c: macros.Context)(tree: c.Expr[Any]) = c.Expr((new { val ctx: c.type = c } with QuasiQuoteUnapply).result)
// }

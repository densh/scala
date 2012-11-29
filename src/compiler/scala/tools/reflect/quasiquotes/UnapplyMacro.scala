package scala.tools.reflect
package quasiquotes

import java.util.UUID.randomUUID
import scala.tools.nsc.Global
import scala.reflect.macros
import scala.collection.mutable

abstract class UnapplyMacro extends Types {
  val ctx: macros.Context
  val g = ctx.universe.asInstanceOf[Global]
  import ctx.universe._
  import ctx.universe.Flag._

  val currentUniverse: ctx.universe.type = ctx.universe
  val currentMirror = ctx.mirror

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

  println(showRaw(ctx.macroApplication))

  val unapplySelector = Ident(newTermName("<unapply-selector>"))
  unapplySelector.setType(treeType)

  val pattern = Apply(Ident(newTermName("Ident")), List(Bind(newTermName("name1"), Ident(nme.WILDCARD))))
  val wrapped = Match(Ident(newTermName("EmptyTree")), List(CaseDef(pattern, EmptyTree, Literal(Constant(())))))
  val Match(_, List(CaseDef(typecheckedPattern: Tree, _, _))) = ctx.typeCheck(wrapped)

  val result = typecheckedPattern.asInstanceOf[scala.reflect.api.Trees#Tree]
}


// abstract class UnapplyMacro extends Types {
//   val ctx: macros.Context
//   val g = ctx.universe.asInstanceOf[Global]
//   import ctx.universe._
//   import ctx.universe.Flag._

//   val currentUniverse: ctx.universe.type = ctx.universe
//   val currentMirror = ctx.mirror

//   val (universe, parts) =
//     ctx.macroApplication match {
//       case Apply(Select(Select(Apply(Select(universe, _), List(Apply(_, parts0))), _), _), _) =>
//         val parts = parts0.map(_ match {
//           case Literal(Constant(s: String)) => s
//           case _ => throw new Exception("")
//         })
//         (universe, parts)
//       case _ => throw new Exception("")
//     }

//   if(!(parts.length >= 1 && parts.length <= 23))
//     throw new Exception("Inapropriate amount of quasiquote params.")

//   val name = newTermName(Const.prefix + "matcher$" + randomUUID().toString.replace("-", ""))

//   val unapplyBody =
//     // if(parts.length == 1)
//     //   Apply(Select(Ident("tree"), "equalsStructure"), List(
//     //     reifyTree(tree)
//     //   ))

//     //   Apply(Ident(newTermName("Some")), List(Ident(newTermName("tree"))))
//     EmptyTree

//   val unapplyResultType: Tree =
//     if(parts.length == 1)
//       TypeTree(typeOf[Boolean])
//     else if(parts.length == 2)
//       TypeTree(optionTreeType)
//     else //if(parts.length <= 23)
//       TypeTree(ctx.mirror.staticClass("scala.Tuple" + (parts.length - 1)).toType)

//   val unapply =
//     DefDef(
//       Modifiers(), newTermName("unapply"), List(),
//       List(
//         List(ValDef(Modifiers(PARAM), newTermName("universe"), TypeTree(universeType), EmptyTree)),
//         List(ValDef(Modifiers(PARAM), newTermName("tree"), TypeTree(treeType), EmptyTree))),
//       unapplyResultType,
//       unapplyBody)

//   val moduledef =
//     ModuleDef(Modifiers(), name, Template(
//       List(Select(Ident(newTermName("scala")), newTypeName("AnyRef"))),
//       emptyValDef,
//       List(
//         DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))),
//         unapply)))

//   ctx.introduceTopLevel(moduledef)

//   val selector = Ident(newTermName("<unapply-selector>"))
//   selector.setType(treeType)

//   val result = Apply(Apply(Select(Ident(name), newTermName("unapply")), List(universe)), List(selector))
// }

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

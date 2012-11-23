package scala.reflect
package internal

import scala.reflect.api._
import scala.reflect.macros
import scala.collection.mutable


private[scala] object QuasiQuoteMacro {

  def apply(c: macros.Context)(args0: c.Expr[Any]*): c.Expr[Any] = {
    val impl = new {
      val ctx: c.type = c
      val args = args0.toList
    } with QuasiQuoteApply
    c.Expr(impl.result)
  }
}

private[scala] abstract class QuasiQuoteApply {

  val qqprefix = "$quasiquote$"
  val qquniverse = "$u"
  val qqdebug = false

  val ctx: macros.Context
  val args: List[ctx.Expr[Any]]
  import ctx.universe._

  val (universe, parts) =
    ctx.prefix.tree match {
      case Select(Apply(Select(universe, _), List(Apply(_, args))), _) =>
        val parts = args.map(_ match {
          case Literal(Constant(s: String)) => s
          case _ => throw QuasiQuoteException("Quasi-quotes can only be used with constant string arguments.")
        })
        (universe, parts)
      case _ => throw QuasiQuoteException("Couldn't parse call prefix tree.")
    }

  if(args.length != parts.length - 1)
    throw QuasiQuoteException("Imbalanced amount of arguments.")

  val universeType = universe.tpe
  val nameType = memberType(universeType, "Name")
  val treeType = memberType(universeType, "Tree")
  val liftableType = ctx.mirror.staticClass("scala.reflect.api.Liftable").toType
  val listType = ctx.mirror.staticClass("scala.collection.immutable.List").toType
  val listTreeType = appliedType(listType, List(treeType))

  val (code, subsmap) = {
    val sb = new StringBuilder(parts.head)
    val subsmap = mutable.Map[String, Expr[Any]]()
    for((expr, part) <- args.zip(parts.tail)) {
      val placeholder = ctx.fresh(qqprefix)
      sb.append(placeholder)
      sb.append(part)
      subsmap(placeholder) = expr
    }
    (sb.toString, subsmap)
  }

  if(qqdebug) println(s"\ncode to parse=\n$code\n")

  val tree = ctx.parse(code)

  if(qqdebug) println(s"parsed tree\n=${tree}\n=${showRaw(tree)}\n")

  val reified = reifyTree(tree)

  if(qqdebug) println(s"reified tree\n=${reified}\n=${showRaw(reified)}\n")

  val result = wrap(reified)

  if(qqdebug) println(s"result tree\n=${result}\n=${showRaw(result)}\n")

  def wrap(t: Tree) =
    Block(
      List(ValDef(Modifiers(),
        newTermName(qquniverse),
        SingletonTypeTree(universe),
        universe)),
      t)

  object SubsToLiftable {

    def unapply(name: Name): Option[Tree] =
      subsmap.get(name.encoded) match {
        case Some(expr) =>
          val liftType = appliedType(liftableType, List(expr.actualType))
          val lift = ctx.inferImplicitValue(liftType, silent = true)
          if(lift != EmptyTree) {
            Some(wrapLift(lift, expr.tree))
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
      subsmap.get(name.encoded) match {
        case Some(expr) =>
          if(expr.actualType <:< treeType)
            Some(expr.tree)
          else
            None
        case None => None
      }
  }

  object SubsToNameTree {

    def unapply(name: Name): Option[Tree] =
      subsmap.get(name.encoded) match {
        case Some(expr) =>
          if(expr.actualType <:< nameType)
            Some(expr.tree)
          else
            None
        case None => None
      }
  }

  object SubsToListTree {

    def unapply(name: Name): Option[Tree] =
      subsmap.get(name.encoded).flatMap { expr =>
        if(expr.actualType <:< listTreeType)
          Some(expr.tree)
        else
          None
      }
  }

  def reifyTree(tree: Tree): Tree = tree match {
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
    case Import(expr, selectors) =>
      val args = mkList(selectors.map(s => reifyProduct(s.asInstanceOf[Product])))
      mirrorCall("Import", reifyAny(expr), args)
    case _ =>
      reifyProduct(tree.asInstanceOf[Product])
  }

  def reifyAny(reifee: Any): Tree = reifee match {
    case name: Name               => reifyName(name)
    case tree: Tree               => reifyTree(tree)
    case mods: Modifiers          => reifyModifiers(mods)
    case xs: List[_]              => reifyList(xs)
    case s: String                => Literal(Constant(s))
    case v if isAnyVal(v)         => Literal(Constant(v))
    case null                     => Literal(Constant(null))
    case _                        =>
      throw new QuasiQuoteException(s"Couldn't reify $reifee of type ${reifee.getClass}.")
  }

  def reifyModifiers(m: Modifiers) =
    mirrorFactoryCall("Modifiers", mirrorBuildCall("flagsFromBits", reifyAny(m.flags)), reifyAny(m.privateWithin), reifyAny(m.annotations))

  def reifyName(name: Name): Tree = {
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
        case _ => throw QuasiQuoteException(s"Name expected but ${subsmap(name.encoded).actualType} found")
      }
  }

  def reifyList(xs: List[Any]): Tree =
    Select(
      mkList(xs.map { _ match {
        case Ident(SubsToListTree(listtree)) => listtree
        case x @ _ => mkList(List(reifyAny(x)))
      }}),
      newTermName("flatten"))

  def reifyMirrorObject(name: String): Tree =
    mirrorSelect(name)

  def reifyMirrorObject(x: Product): Tree =
    reifyMirrorObject(x.productPrefix)

  def reifyProduct(x: Product): Tree = {
    val prefix = x.productPrefix
    val elements = x.productIterator.toList
    if (prefix.startsWith("Tuple"))
      scalaFactoryCall(prefix, elements.map(reifyAny).toList: _*)
    else
      mirrorCall(prefix, elements.map(reifyAny): _*)
  }

  def termPath(fullname: String): Tree = {
    val parts = fullname split "\\."
    val prefixParts = parts.init
    val lastName = newTermName(parts.last)
    if (prefixParts.isEmpty) Ident(lastName)
    else {
      val prefixTree = ((Ident(prefixParts.head): Tree) /: prefixParts.tail)(Select(_, _))
      Select(prefixTree, lastName)
    }
  }

  def call(fname: String, args: Tree*): Tree =
    Apply(termPath(fname), args.toList)

  def scalaFactoryCall(name: String, args: Tree*): Tree =
    call(s"scala.$name.apply", args: _*)

  def mirrorCall(name: String, args: Tree*): Tree =
    call(s"$qquniverse.$name", args: _*)

  def mirrorSelect(name: String): Tree =
    termPath(s"$qquniverse.$name")

  def mirrorBuildSelect(name: String): Tree =
    termPath(s"$qquniverse.build.$name")

  def mirrorFactoryCall(value: Product, args: Tree*): Tree =
    mirrorFactoryCall(value.productPrefix, args: _*)

  def mirrorFactoryCall(prefix: String, args: Tree*): Tree =
    mirrorCall(prefix, args: _*)

  def mirrorBuildCall(name: String, args: Tree*): Tree =
    call(s"$qquniverse.build.$name", args: _*)

  def mkList(args: List[Tree]): Tree =
    scalaFactoryCall("collection.immutable.List", args: _*)

  def isAnyVal(x: Any) = x match {
    case _: Byte | _: Short | _: Char | _: Int | _: Long | _: Float | _: Double | _: Boolean | _: Unit => true
    case _                                                                                             => false
  }

  def memberType(thistype: Type, name: String): Type = {
    val sym = thistype.typeSymbol.typeSignature.member(newTypeName(name))
    sym.asType.toType.typeConstructor.asSeenFrom(thistype, sym.owner)
  }
}
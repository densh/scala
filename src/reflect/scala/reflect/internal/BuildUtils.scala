package scala
package reflect
package internal

import Flags._

trait BuildUtils { self: SymbolTable =>
  import definitions.{TupleClass, FunctionClass, MaxTupleArity, MaxFunctionArity, ScalaPackage, UnitClass}

  class BuildImpl extends BuildApi {

    def selectType(owner: Symbol, name: String): TypeSymbol =
      select(owner, newTypeName(name)).asType

    def selectTerm(owner: Symbol, name: String): TermSymbol = {
      val result = select(owner, newTermName(name)).asTerm
      if (result.isOverloaded) result.suchThat(!_.isMethod).asTerm
      else result
    }

    private def select(owner: Symbol, name: Name): Symbol = {
      val result = owner.info decl name
      if (result ne NoSymbol) result
      else
        mirrorThatLoaded(owner).missingHook(owner, name) orElse
        MissingRequirementError.notFound("%s %s in %s".format(if (name.isTermName) "term" else "type", name, owner.fullName))
    }

    def selectOverloadedMethod(owner: Symbol, name: String, index: Int): MethodSymbol = {
      val result = owner.info.decl(newTermName(name)).alternatives(index)
      if (result ne NoSymbol) result.asMethod
      else MissingRequirementError.notFound("overloaded method %s #%d in %s".format(name, index, owner.fullName))
    }

    def newFreeTerm(name: String, value: => Any, flags: Long = 0L, origin: String = null): FreeTermSymbol =
      newFreeTermSymbol(newTermName(name), value, flags, origin)

    def newFreeType(name: String, flags: Long = 0L, origin: String = null): FreeTypeSymbol =
      newFreeTypeSymbol(newTypeName(name), flags, origin)

    def newNestedSymbol(owner: Symbol, name: Name, pos: Position, flags: Long, isClass: Boolean): Symbol =
      owner.newNestedSymbol(name, pos, flags, isClass)

    def setAnnotations[S <: Symbol](sym: S, annots: List[AnnotationInfo]): S =
      sym.setAnnotations(annots)

    def setTypeSignature[S <: Symbol](sym: S, tpe: Type): S =
      sym.setTypeSignature(tpe)

    def This(sym: Symbol): Tree = self.This(sym)

    def Select(qualifier: Tree, sym: Symbol): Select = self.Select(qualifier, sym)

    def Ident(sym: Symbol): Ident = self.Ident(sym)

    def TypeTree(tp: Type): TypeTree = self.TypeTree(tp)

    def thisPrefix(sym: Symbol): Type = sym.thisPrefix

    def setType[T <: Tree](tree: T, tpe: Type): T = { tree.setType(tpe); tree }

    def setSymbol[T <: Tree](tree: T, sym: Symbol): T = { tree.setSymbol(sym); tree }

    def mkAnnotation(tree: Tree, args: List[Tree]): Tree = tree match {
      case ident: Ident => Apply(self.Select(New(ident), nme.CONSTRUCTOR: TermName), args)
      case call @ Apply(Select(New(ident: Ident), nme.CONSTRUCTOR), _) =>
        if (args.nonEmpty)
          throw new IllegalArgumentException("Can't splice annotation that already contains args with extra args, consider merging these lists together")
        call
      case _ => throw new IllegalArgumentException(s"Tree ${showRaw(tree)} isn't a correct representation of annotation, consider passing Ident as a first argument")
    }

    def mkVparamss(argss: List[List[ValDef]]): List[List[ValDef]] =
      argss.map { _.map {
        case vd @ ValDef(mods, _, _, EmptyTree) => copyValDef(vd)(mods = mods | PARAM)
        case vd @ ValDef(mods, _, _, _) => copyValDef(vd)(mods = mods | PARAM | DEFAULTPARAM)
      } }

    def mkTparams(tparams: List[TypeDef]): List[TypeDef] =
      tparams.map { td => copyTypeDef(td)(mods = (td.mods | PARAM) & (~DEFERRED)) }

    object FlagsBits extends FlagsBitsExtractor {
      def apply(bits: Long): FlagSet = bits
      def unapply(flags: Long): Some[Long] = Some(flags)
    }

    object TypeApplied extends TypeAppliedExtractor {
      def apply(tree: Tree, targs: List[Tree]): Tree =
        if (targs.isEmpty) tree
        else if (tree.isTerm) TypeApply(tree, targs)
        else if (tree.isType) AppliedTypeTree(tree, targs)
        else throw new IllegalArgumentException(s"can't apply types to $tree")

      def unapply(tree: Tree): Some[(Tree, List[Tree])] = tree match {
        case TypeApply(fun, targs) => Some((fun, targs))
        case AppliedTypeTree(tpe, targs) => Some((tpe, targs))
        case _ => Some((tree, Nil))
      }
    }

    object Applied extends AppliedExtractor {
      def apply(tree: Tree, argss: List[List[Tree]]): Tree =
        argss.foldLeft(tree) { (tree, args) => Apply(tree, args) }

      def unapply(tree: Tree): Some[(Tree, List[List[Tree]])] = {
        val treeInfo.Applied(fun, targs, argss) = tree
        Some((TypeApplied(fun, targs), argss))
      }
    }

    object SyntacticClassDef extends SyntacticClassDefExtractor {
      def apply(mods: Modifiers, name: TypeName, tparams: List[TypeDef],
                constrMods: Modifiers, vparamss: List[List[ValDef]], parents: List[Tree],
                selfdef: ValDef, body: List[Tree]): ClassDef =
        ClassDef(mods, name, tparams, gen.mkTemplate(parents, selfdef, constrMods, vparamss, body, NoPosition))

      def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef], Modifiers,
                                       List[List[ValDef]], List[Tree], ValDef, List[Tree])] = tree match {
        case ClassDef(mods, name, tparams, Template(parents, selfdef, tbody)) =>
          // extract generated fieldDefs and constructor
          val (defs, (ctor: DefDef) :: body) = tbody.splitAt(tbody.indexWhere {
            case DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => true
            case _ => false
          })
          val (earlyDefs, fieldDefs) = defs.span(treeInfo.isEarlyDef)

          // undo conversion from (implicit ... ) to ()(implicit ... ) when its the only parameter section
          val vparamssRestoredImplicits = ctor.vparamss match {
            case Nil :: rest if !rest.isEmpty && !rest.head.isEmpty && rest.head.head.mods.isImplicit => rest
            case other => other
          }

          // undo flag modifications by mergeing flag info from constructor args and fieldDefs
          val modsMap = fieldDefs.map { case ValDef(mods, name, _, _) => name -> mods }.toMap
          val vparamss = mmap(vparamssRestoredImplicits) { vd =>
            val originalMods = modsMap(vd.name) | (vd.mods.flags & DEFAULTPARAM)
            atPos(vd.pos)(ValDef(originalMods, vd.name, vd.tpt, vd.rhs))
          }

          Some((mods, name, tparams, ctor.mods, vparamss, parents, selfdef, earlyDefs ::: body))
        case _ =>
          None
      }
    }

    object TupleN extends TupleNExtractor {
      def apply(args: List[Tree]): Tree = args match {
        case Nil      => Literal(Constant(()))
        case _        =>
          require(args.length <= MaxTupleArity, s"Tuples with arity bigger than $MaxTupleArity aren't supported")
          self.Apply(TupleClass(args.length).companionModule, args: _*)
      }

      def unapply(tree: Tree): Option[List[Tree]] = tree match {
        case Literal(Constant(())) =>
          Some(Nil)
        case Apply(id: Ident, args)
          if args.length <= MaxTupleArity && id.symbol == TupleClass(args.length).companionModule =>
          Some(args)
        case Apply(Select(Ident(nme.scala_), TermName(tuple)), args)
          if args.length <= MaxTupleArity && tuple == TupleClass(args.length).name =>
          Some(args)
        case _ =>
          None
      }
    }

    object TupleTypeN extends TupleNExtractor {
      def apply(args: List[Tree]): Tree = args match {
        case Nil => self.Select(self.Ident(nme.scala_), tpnme.Unit)
        case _   =>
          require(args.length <= MaxTupleArity, s"Tuples with arity bigger than $MaxTupleArity aren't supported")
          AppliedTypeTree(Ident(TupleClass(args.length)), args)
      }

      def unapply(tree: Tree): Option[List[Tree]] =  tree match {
        case Select(Ident(nme.scala_), tpnme.Unit) =>
          Some(Nil)
        case AppliedTypeTree(id: Ident, args)
          if args.length <= MaxTupleArity && id.symbol == TupleClass(args.length) =>
          Some(args)
        case AppliedTypeTree(Select(id @ Ident(nme.scala_), TermName(tuple)), args)
          if args.length <= MaxTupleArity && id.symbol == ScalaPackage && tuple == TupleClass(args.length).name =>
          Some(args)
        case _ =>
          None
      }
    }

    object FunctionType extends FunctionTypeExtractor {
      def apply(argtpes: List[Tree], restpe: Tree): Tree = {
        require(argtpes.length <= MaxFunctionArity + 1, s"Function types with arity bigger than $MaxFunctionArity aren't supported")
        gen.mkFunctionTypeTree(argtpes, restpe)
      }

      def unapply(tree: Tree): Option[(List[Tree], Tree)] = tree match {
        case AppliedTypeTree(id: Ident, args @ (argtpes :+ restpe))
          if args.length - 1 <= MaxFunctionArity && id.symbol == FunctionClass(args.length - 1) =>
          Some((argtpes, restpe))
        case AppliedTypeTree(Select(pack, fun), args @ (argtpes :+ restpe))
          if args.length - 1 <= MaxFunctionArity && pack.symbol == ScalaPackage && fun == FunctionClass(args.length - 1).name =>
          Some((argtpes, restpe))
        case AppliedTypeTree(Select(Select(Ident(nme.ROOTPKG), nme.scala_), fun), args @ (argtpes :+ restpe))
          if args.length - 1 <= MaxFunctionArity && fun == FunctionClass(args.length - 1).name =>
          Some((argtpes, restpe))
        case _ => None
      }
    }

    object SyntacticBlock extends SyntacticBlockExtractor {
      def apply(stats: List[Tree]): Tree = gen.mkBlock(stats)

      def unapply(tree: Tree): Some[List[Tree]] = tree match {
        case self.Block(stats, expr) => Some(stats :+ expr)
        case _ => Some(tree :: Nil)
      }
    }

    def RefTree(qual: Tree, sym: Symbol) = self.RefTree(qual, sym.name) setSymbol sym

    object SyntacticNew extends SyntacticNewExtractor {
      def apply(parents: List[Tree], selfdef: ValDef, body: List[Tree]): Tree =
        gen.mkNew(parents, selfdef, body, NoPosition, NoPosition)

      def unapply(tree: Tree): Option[(List[Tree], ValDef, List[Tree])] = tree match {
        case Applied(Select(New(TypeApplied(ident, targs)), nme.CONSTRUCTOR), argss) =>
          Some((Applied(TypeApplied(ident, targs), argss) :: Nil, emptyValDef, Nil))
        case self.Block(List(SyntacticClassDef(_, tpnme.ANON_CLASS_NAME, Nil, _, List(Nil), parents, selfdef, body)),
                        Apply(Select(New(Ident(tpnme.ANON_CLASS_NAME)), nme.CONSTRUCTOR), Nil)) =>
          Some((parents, selfdef, body))
        case _ =>
          None
      }
    }
  }

  val build: BuildApi = new BuildImpl
}

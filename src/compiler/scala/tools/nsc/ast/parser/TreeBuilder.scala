/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast.parser

import symtab.Flags._
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.{Position, SourceFile, FreshNameCreator}

/** Methods for building trees, used in the parser.  All the trees
 *  returned by this class must be untyped.
 */
abstract class TreeBuilder {
  val global: Global
  import global._
  import global.build.{SyntacticValFrom => ValFrom, SyntacticValEq => ValEq, SyntacticFilter => Filter, SyntacticYield => Yield}

  def unit: CompilationUnit
  def source: SourceFile

  implicit def fresh: FreshNameCreator              = unit.fresh
  def o2p(offset: Int): Position                    = Position.offset(source, offset)
  def r2p(start: Int, mid: Int, end: Int): Position = rangePos(source, start, mid, end)

  def rootScalaDot(name: Name) = gen.rootScalaDot(name)
  def scalaDot(name: Name)     = gen.scalaDot(name)
  def scalaAnyRefConstr        = scalaDot(tpnme.AnyRef)
  def scalaUnitConstr          = scalaDot(tpnme.Unit)

  def convertToTypeName(t: Tree) = gen.convertToTypeName(t)

  /** Convert all occurrences of (lower-case) variables in a pattern as follows:
   *    x                  becomes      x @ _
   *    x: T               becomes      x @ (_: T)
   */
  object patvarTransformer extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case Ident(name) if (treeInfo.isVarPattern(tree) && name != nme.WILDCARD) =>
        atPos(tree.pos)(Bind(name, atPos(tree.pos.focus) (Ident(nme.WILDCARD))))
      case Typed(id @ Ident(name), tpt) if (treeInfo.isVarPattern(id) && name != nme.WILDCARD) =>
        atPos(tree.pos.withPoint(id.pos.point)) {
          Bind(name, atPos(tree.pos.withStart(tree.pos.point)) {
            Typed(Ident(nme.WILDCARD), tpt)
          })
        }
      case Apply(fn @ Apply(_, _), args) =>
        treeCopy.Apply(tree, transform(fn), transformTrees(args))
      case Apply(fn, args) =>
        treeCopy.Apply(tree, fn, transformTrees(args))
      case Typed(expr, tpt) =>
        treeCopy.Typed(tree, transform(expr), tpt)
      case Bind(name, body) =>
        treeCopy.Bind(tree, name, transform(body))
      case Alternative(_) | Star(_) =>
        super.transform(tree)
      case _ =>
        tree
    }
  }

  /** Traverse pattern and collect all variable names with their types in buffer
   *  The variables keep their positions; whereas the pattern is converted to be
   *  synthetic for all nodes that contain a variable position.
   */
  class GetVarTraverser extends Traverser {
    val buf = new ListBuffer[(Name, Tree, Position)]

    def namePos(tree: Tree, name: Name): Position =
      if (!tree.pos.isRange || name.containsName(nme.raw.DOLLAR)) tree.pos.focus
      else {
        val start = tree.pos.start
        val end = start + name.decode.length
        r2p(start, start, end)
      }

    override def traverse(tree: Tree): Unit = {
      def seenName(name: Name)     = buf exists (_._1 == name)
      def add(name: Name, t: Tree) = if (!seenName(name)) buf += ((name, t, namePos(tree, name)))
      val bl = buf.length

      tree match {
        case Bind(nme.WILDCARD, _)          =>
          super.traverse(tree)

        case Bind(name, Typed(tree1, tpt))  =>
          val newTree = if (treeInfo.mayBeTypePat(tpt)) TypeTree() else tpt.duplicate
          add(name, newTree)
          traverse(tree1)

        case Bind(name, tree1)              =>
          // can assume only name range as position, as otherwise might overlap
          // with binds embedded in pattern tree1
          add(name, TypeTree())
          traverse(tree1)

        case _ =>
          super.traverse(tree)
      }
      if (buf.length > bl)
        tree setPos tree.pos.makeTransparent
    }
    def apply(tree: Tree) = {
      traverse(tree)
      buf.toList
    }
  }

  /** Returns list of all pattern variables, possibly with their types,
   *  without duplicates
   */
  private def getVariables(tree: Tree): List[(Name, Tree, Position)] =
    new GetVarTraverser apply tree

  def byNameApplication(tpe: Tree): Tree =
    AppliedTypeTree(rootScalaDot(tpnme.BYNAME_PARAM_CLASS_NAME), List(tpe))
  def repeatedApplication(tpe: Tree): Tree =
    AppliedTypeTree(rootScalaDot(tpnme.REPEATED_PARAM_CLASS_NAME), List(tpe))

  def makeImportSelector(name: Name, nameOffset: Int): ImportSelector =
    ImportSelector(name, nameOffset, name, nameOffset)

  def makeTupleTerm(elems: List[Tree]) = gen.mkTuple(elems)

  def makeTupleType(elems: List[Tree]) = gen.mkTupleType(elems)

  def stripParens(t: Tree) = t match {
    case Parens(ts) => atPos(t.pos) { makeTupleTerm(ts) }
    case _ => t
  }

  def makeAnnotated(t: Tree, annot: Tree): Tree =
    atPos(annot.pos union t.pos)(Annotated(annot, t))

  def makeSelfDef(name: TermName, tpt: Tree): ValDef =
    ValDef(Modifiers(PRIVATE), name, tpt, EmptyTree)

  /** If tree is a variable pattern, return Some("its name and type").
   *  Otherwise return none */
  private def matchVarPattern(tree: Tree): Option[(Name, Tree)] = {
    def wildType(t: Tree): Option[Tree] = t match {
      case Ident(x) if x.toTermName == nme.WILDCARD             => Some(TypeTree())
      case Typed(Ident(x), tpt) if x.toTermName == nme.WILDCARD => Some(tpt)
      case _                                                    => None
    }
    tree match {
      case Ident(name)             => Some((name, TypeTree()))
      case Bind(name, body)        => wildType(body) map (x => (name, x))
      case Typed(Ident(name), tpt) => Some((name, tpt))
      case _                       => None
    }
  }

  /** Create tree representing (unencoded) binary operation expression or pattern. */
  def makeBinop(isExpr: Boolean, left: Tree, op: TermName, right: Tree, opPos: Position): Tree = {
    def mkNamed(args: List[Tree]) = if (isExpr) args map treeInfo.assignmentToMaybeNamedArg else args
    val arguments = right match {
      case Parens(args) => mkNamed(args)
      case _            => List(right)
    }
    if (isExpr) {
      if (treeInfo.isLeftAssoc(op)) {
        Apply(atPos(opPos union left.pos) { Select(stripParens(left), op.encode) }, arguments)
      } else {
        val x = freshTermName()
        Block(
          List(ValDef(Modifiers(SYNTHETIC | ARTIFACT), x, TypeTree(), stripParens(left))),
          Apply(atPos(opPos union right.pos) { Select(stripParens(right), op.encode) }, List(Ident(x))))
      }
    } else {
      Apply(Ident(op.encode), stripParens(left) :: arguments)
    }
  }

  /** Tree for `od op`, start is start0 if od.pos is borked. */
  def makePostfixSelect(start0: Int, end: Int, od: Tree, op: Name): Tree = {
    val start = if (od.pos.isDefined) od.pos.start else start0
    atPos(r2p(start, end, end + op.length)) { new PostfixSelect(od, op.encode) }
  }

  /** Create tree representing a while loop */
  def makeWhile(startPos: Int, cond: Tree, body: Tree): Tree = {
    val lname = freshTermName(nme.WHILE_PREFIX)
    def default = wrappingPos(List(cond, body)) match {
      case p if p.isDefined => p.end
      case _                => startPos
    }
    val continu = atPos(o2p(body.pos pointOrElse default)) { Apply(Ident(lname), Nil) }
    val rhs = If(cond, Block(List(body), continu), Literal(Constant(())))
    LabelDef(lname, Nil, rhs)
  }

  /** Create tree representing a do-while loop */
  def makeDoWhile(lname: TermName, body: Tree, cond: Tree): Tree = {
    val continu = Apply(Ident(lname), Nil)
    val rhs = Block(List(body), If(cond, continu, Literal(Constant(()))))
    LabelDef(lname, Nil, rhs)
  }

  /** Create block of statements `stats`  */
  def makeBlock(stats: List[Tree]): Tree = gen.mkBlock(stats)

  /** Create tree for for-comprehension generator <val pat0 <- rhs0> */
  def makeGenerator(pos: Position, pat: Tree, valeq: Boolean, rhs: Tree)(implicit fresh: FreshNameCreator): Tree = {
    val pat1 = patvarTransformer.transform(pat)
    if (valeq) ValEq(pat1, rhs).setPos(pos)
    else ValFrom(pat1, rhs).setPos(pos)
  }

  def makeParam(pname: TermName, tpe: Tree) =
    ValDef(Modifiers(PARAM), pname, tpe, EmptyTree)

  def makeSyntheticTypeParam(pname: TypeName, bounds: Tree) =
    TypeDef(Modifiers(DEFERRED | SYNTHETIC), pname, Nil, bounds)

  /** Create tree for for-comprehension <for (enums) do body> or
  *   <for (enums) yield body> where mapName and flatMapName are chosen
  *  corresponding to whether this is a for-do or a for-yield.
  *  The creation performs the following rewrite rules:
  *
  *  1.
  *
  *    for (P <- G) E   ==>   G.foreach (P => E)
  *
  *     Here and in the following (P => E) is interpreted as the function (P => E)
  *     if P is a variable pattern and as the partial function { case P => E } otherwise.
  *
  *  2.
  *
  *    for (P <- G) yield E  ==>  G.map (P => E)
  *
  *  3.
  *
  *    for (P_1 <- G_1; P_2 <- G_2; ...) ...
  *      ==>
  *    G_1.flatMap (P_1 => for (P_2 <- G_2; ...) ...)
  *
  *  4.
  *
  *    for (P <- G; E; ...) ...
  *      =>
  *    for (P <- G.filter (P => E); ...) ...
  *
  *  5. For N < MaxTupleArity:
  *
  *    for (P_1 <- G; P_2 = E_2; val P_N = E_N; ...)
  *      ==>
  *    for (TupleN(P_1, P_2, ... P_N) <-
  *      for (x_1 @ P_1 <- G) yield {
  *        val x_2 @ P_2 = E_2
  *        ...
  *        val x_N & P_N = E_N
  *        TupleN(x_1, ..., x_N)
  *      } ...)
  *
  *    If any of the P_i are variable patterns, the corresponding `x_i @ P_i' is not generated
  *    and the variable constituting P_i is used instead of x_i
  *
  *  @param mapName      The name to be used for maps (either map or foreach)
  *  @param flatMapName  The name to be used for flatMaps (either flatMap or foreach)
  *  @param enums        The enumerators in the for expression
  *  @param body          The body of the for expression
  */
  def makeFor(enums: List[Tree], body: Tree)(implicit fresh: FreshNameCreator): Tree = {
    val (mapName, flatMapName, rawBody) = body match {
      case Yield(tree) => (nme.map, nme.flatMap, tree)
      case _           => (nme.foreach, nme.foreach, body)
    }

    /* make a closure pat => body.
     * The closure is assigned a transparent position with the point at pos.point and
     * the limits given by pat and body.
     */
    def makeClosure(pos: Position, pat: Tree, body: Tree): Tree = {
      def splitpos = wrappingPos(List(pat, body)).withPoint(pos.point).makeTransparent
      matchVarPattern(pat) match {
        case Some((name, tpt)) =>
          Function(
            List(atPos(pat.pos) { ValDef(Modifiers(PARAM), name.toTermName, tpt, EmptyTree) }),
            body) setPos splitpos
        case None =>
          atPos(splitpos) {
            gen.mkVisitor(List(CaseDef(pat, EmptyTree, body)), checkExhaustive = false)
          }
      }
    }

    /* Make an application  qual.meth(pat => body) positioned at `pos`.
     */
    def makeCombination(pos: Position, meth: TermName, qual: Tree, pat: Tree, body: Tree): Tree =
      Apply(Select(qual, meth).setPos(qual.pos).updateAttachment(ForAttachment),
        List(makeClosure(pos, pat, body))) setPos pos

    /* If `pat` is not yet a `Bind` wrap it in one with a fresh name */
    def makeBind(pat: Tree): Tree = pat match {
      case Bind(_, _) => pat
      case _ => Bind(freshTermName(), pat) setPos pat.pos
    }

    /* A reference to the name bound in Bind `pat`. */
    def makeValue(pat: Tree): Tree = pat match {
      case Bind(name, _) => Ident(name) setPos pat.pos.focus
    }

    /* The position of the closure that starts with generator at position `genpos`. */
    def closurePos(genpos: Position) = {
      val end = body.pos match {
        case NoPosition => genpos.point
        case bodypos => bodypos.end
      }
      r2p(genpos.start, genpos.point, end)
    }

    enums match {
      case (t @ ValFrom(pat, rhs)) :: Nil =>
        makeCombination(closurePos(t.pos), mapName, rhs, pat, rawBody)
      case (t @ ValFrom(pat, rhs)) :: (rest @ (ValFrom(_, _) :: _)) =>
        makeCombination(closurePos(t.pos), flatMapName, rhs, pat,
                        makeFor(rest, body))
      case (t @ ValFrom(pat, rhs)) :: Filter(test) :: rest =>
        makeFor(ValFrom(pat, makeCombination(rhs.pos union test.pos, nme.withFilter, rhs, pat.duplicate, test)).setPos(t.pos) :: rest,
                body)
      case (t @ ValFrom(pat, rhs)) :: rest =>
        val valeqs = rest.take(definitions.MaxTupleArity - 1).takeWhile { ValEq.unapply(_).nonEmpty }
        assert(!valeqs.isEmpty)
        val rest1 = rest.drop(valeqs.length)
        val pats = valeqs map { case ValEq(pat, _) => pat }
        val rhss = valeqs map { case ValEq(_, rhs) => rhs }
        val defpat1 = makeBind(pat)
        val defpats = pats map makeBind
        val pdefs = (defpats, rhss).zipped flatMap makePatDef
        val ids = (defpat1 :: defpats) map makeValue
        val rhs1 = makeFor(
          List(ValFrom(defpat1, rhs) setPos t.pos),
          Yield(Block(pdefs, atPos(wrappingPos(ids)) { makeTupleTerm(ids) }) setPos wrappingPos(pdefs)))
        val allpats = (pat :: pats) map (_.duplicate)
        val vfrom1 = ValFrom(atPos(wrappingPos(allpats)) { makeTupleTerm(allpats) }, rhs1) setPos r2p(t.pos.start, t.pos.point, rhs1.pos.end)
        makeFor(vfrom1 :: rest1, body)
      case _ =>
        EmptyTree //may happen for erroneous input
    }
  }

  /** Create tree for a pattern alternative */
  def makeAlternative(ts: List[Tree]): Tree = {
    def alternatives(t: Tree): List[Tree] = t match {
      case Alternative(ts)  => ts
      case _                => List(t)
    }
    Alternative(ts flatMap alternatives)
  }

  /** Create tree for case definition <case pat if guard => rhs> */
  def makeCaseDef(pat: Tree, guard: Tree, rhs: Tree): CaseDef =
    CaseDef(patvarTransformer.transform(pat), guard, rhs)

  /** Creates tree representing:
   *    { case x: Throwable =>
   *        val catchFn = catchExpr
   *        if (catchFn isDefinedAt x) catchFn(x) else throw x
   *    }
   */
  def makeCatchFromExpr(catchExpr: Tree): CaseDef = {
    val binder   = freshTermName()
    val pat      = Bind(binder, Typed(Ident(nme.WILDCARD), Ident(tpnme.Throwable)))
    val catchDef = ValDef(Modifiers(ARTIFACT), freshTermName("catchExpr"), TypeTree(), catchExpr)
    val catchFn  = Ident(catchDef.name)
    val body     = atPos(catchExpr.pos.makeTransparent)(Block(
      List(catchDef),
      If(
        Apply(Select(catchFn, nme.isDefinedAt), List(Ident(binder))),
        Apply(Select(catchFn, nme.apply), List(Ident(binder))),
        Throw(Ident(binder))
      )
    ))
    makeCaseDef(pat, EmptyTree, body)
  }

  /** Create tree for pattern definition <val pat0 = rhs> */
  def makePatDef(pat: Tree, rhs: Tree)(implicit fresh: FreshNameCreator): List[Tree] =
    makePatDef(Modifiers(0), pat, rhs)

  /** Create tree for pattern definition <mods val pat0 = rhs> */
  def makePatDef(mods: Modifiers, pat: Tree, rhs: Tree)(implicit fresh: FreshNameCreator): List[Tree] = matchVarPattern(pat) match {
    case Some((name, tpt)) =>
      List(atPos(pat.pos union rhs.pos) {
        ValDef(mods, name.toTermName, tpt, rhs)
      })

    case None =>
      //  in case there is exactly one variable x_1 in pattern
      //  val/var p = e  ==>  val/var x_1 = e.match (case p => (x_1))
      //
      //  in case there are zero or more than one variables in pattern
      //  val/var p = e  ==>  private synthetic val t$ = e.match (case p => (x_1, ..., x_N))
      //                  val/var x_1 = t$._1
      //                  ...
      //                  val/var x_N = t$._N

      val rhsUnchecked = gen.mkUnchecked(rhs)

      // TODO: clean this up -- there is too much information packked into makePatDef's `pat` argument
      // when it's a simple identifier (case Some((name, tpt)) -- above),
      // pat should have the type ascription that was specified by the user
      // however, in `case None` (here), we must be careful not to generate illegal pattern trees (such as `(a, b): Tuple2[Int, String]`)
      // i.e., this must hold: pat1 match { case Typed(expr, tp) => assert(expr.isInstanceOf[Ident]) case _ => }
      // if we encounter such an erroneous pattern, we strip off the type ascription from pat and propagate the type information to rhs
      val (pat1, rhs1) = patvarTransformer.transform(pat) match {
        // move the Typed ascription to the rhs
        case Typed(expr, tpt) if !expr.isInstanceOf[Ident] =>
          val rhsTypedUnchecked =
            if (tpt.isEmpty) rhsUnchecked
            else Typed(rhsUnchecked, tpt) setPos (rhs.pos union tpt.pos)
          (expr, rhsTypedUnchecked)
        case ok =>
          (ok, rhsUnchecked)
      }
      val vars = getVariables(pat1)
      val matchExpr = atPos((pat1.pos union rhs.pos).makeTransparent) {
        Match(
          rhs1,
          List(
            atPos(pat1.pos) {
              CaseDef(pat1, EmptyTree, gen.mkTuple(vars map (_._1) map Ident.apply))
            }
          ))
      }
      vars match {
        case List((vname, tpt, pos)) =>
          List(atPos(pat.pos union pos union rhs.pos) {
            ValDef(mods, vname.toTermName, tpt, matchExpr)
          })
        case _ =>
          val tmp = freshTermName()
          val firstDef =
            atPos(matchExpr.pos) {
              ValDef(Modifiers(PrivateLocal | SYNTHETIC | ARTIFACT | (mods.flags & LAZY)),
                     tmp, TypeTree(), matchExpr)
            }
          var cnt = 0
          val restDefs = for ((vname, tpt, pos) <- vars) yield atPos(pos) {
            cnt += 1
            ValDef(mods, vname.toTermName, tpt, Select(Ident(tmp), newTermName("_" + cnt)))
          }
          firstDef :: restDefs
      }
  }

  /** Create a tree representing the function type (argtpes) => restpe */
  def makeFunctionTypeTree(argtpes: List[Tree], restpe: Tree): Tree = gen.mkFunctionTypeTree(argtpes, restpe)

  /** Append implicit parameter section if `contextBounds` nonempty */
  def addEvidenceParams(owner: Name, vparamss: List[List[ValDef]], contextBounds: List[Tree]): List[List[ValDef]] = {
    if (contextBounds.isEmpty) vparamss
    else {
      val mods = Modifiers(if (owner.isTypeName) PARAMACCESSOR | LOCAL | PRIVATE else PARAM)
      def makeEvidenceParam(tpt: Tree) = ValDef(mods | IMPLICIT | SYNTHETIC, freshTermName(nme.EVIDENCE_PARAM_PREFIX), tpt, EmptyTree)
      val evidenceParams = contextBounds map makeEvidenceParam

      val vparamssLast = if(vparamss.nonEmpty) vparamss.last else Nil
      if(vparamssLast.nonEmpty && vparamssLast.head.mods.hasFlag(IMPLICIT))
        vparamss.init ::: List(evidenceParams ::: vparamssLast)
      else
        vparamss ::: List(evidenceParams)
    }
  }
}

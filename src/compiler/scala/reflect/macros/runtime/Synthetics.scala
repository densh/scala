/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Gilles Dubochet
 */

package scala.reflect.macros
package runtime

import java.util.UUID._
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.internal.MissingRequirementError
import scala.reflect.io.VirtualFile

trait Synthetics {
  self: Context =>

  import global._

  def existsTopLevel(name: Name): Boolean =
    // getClassIfDefined and getModuleIfDefined cannot be used here
    // because they don't work for stuff declared in the empty package
    // (as specified in SLS, code inside non-empty packages cannot see
    // declarations from the empty package, so compiler internals
    // default to ignoring contents of the empty package)
    // to the contrast, staticModule and staticClass are designed
    // to be a part of the reflection API and, therefore, they
    // correctly resolve all names
    try {
      val sym =
        if (name.isTermName) mirror.staticModule(name.toString)
        else mirror.staticClass(name.toString)
      sym != NoSymbol
    } catch {
      case MissingRequirementError(_) => false
    }

  def introduceTopLevel(code: Tree): Unit = {
    // TODO: provide a way to specify a pretty name for debugging purposes
    val filesystemFile = new VirtualFile("macroSynthetic-" + randomUUID().toString.replace("-", "") + ".scala")
    val sourceFile = new BatchSourceFile(filesystemFile, code.toString)
    val unit = new CompilationUnit(sourceFile)
    def wrap(code: Tree): Tree = code match {
      case PackageDef(_, _) => code
      case ClassDef(_, _, _, _) => wrap(Block(List(code), Literal(Constant(()))))
      case ModuleDef(_, _, _) => wrap(Block(List(code), Literal(Constant(()))))
      case Block(cdefs, _) => PackageDef(Ident(nme.EMPTY_PACKAGE_NAME), cdefs)
    }
    unit.body = wrap(code)
    universe.currentRun.compileLate(unit)
  }
}

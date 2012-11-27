package scala.reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  A slice of [[scala.reflect.macros.Context the Scala macros context]] that
 *  exposes functions to introduce synthetic definitions.
 */
trait Synthetics {
  self: Context =>

  /** Checks whether a definition with a given name (term name for modules, type name for classes)
   *  exists at top level.
   */
  def existsTopLevel(name: Name): Boolean

  /** Adds a top-level definition or a list of top-level definitions
   *  to the compiler's symbol table.
   *
   *  Accepts the following shapes of the `code` parameter:
   *    * `PackageDef`: in this case the package will be created if missing
   *      and all its contents will be added to the global symbol table.
   *    * `ClassDef` and `ModuleDef`: in this case the given class or module
   *      will be added to the empty package.
   *    * `Block`: in this case the `stats` part of the block will be iterated
   *      for classes and modules, which will be added to the empty package.
   */
  def introduceTopLevel(code: Tree): Unit
}

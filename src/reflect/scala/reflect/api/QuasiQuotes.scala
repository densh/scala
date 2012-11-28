package scala.reflect
package api

import scala.language.experimental.macros


trait QuasiQuotes { self: Universe =>

  implicit class QuasiQuote(ctx: StringContext) {
    object q {
      // implementation is hardwired to `scala.tools.reflect.quasiquotes.ApplyMacro`
      // using the mechanism implemented in `scala.tools.reflect.FastTrack`
      def apply(args: Any*): Any = ??? // macro
      // implementation is hardwired to `scala.tools.reflect.quasiquotes.UnapplyMacro`
      // using the mechanism implemented in `scala.tools.reflect.FastTrack`
      def unapply(tree: Any): Option[Any] = ??? // macro
    }
  }
}

case class QuasiQuoteException(msg: String) extends Exception(msg)

package scala.reflect
package api

import scala.language.experimental.macros


trait QuasiQuotes { self: Universe =>

  implicit class QuasiQuote(ctx: StringContext) {
    object q {
      // implementation is hardwired to `scala.tools.reflect.QuasiQuotes`
      // using the mechanism implemented in `scala.tools.reflect.FastTrack`
      def apply(args0: Any*): Any = ??? //macro
    }
  }
}

case class QuasiQuoteException(msg: String) extends Exception(msg)

package scala.reflect
package api

import scala.language.experimental.macros


trait QuasiQuotes { self: Universe =>

  implicit class QuasiQuote(ctx: StringContext) {
    object q {
      def apply(args0: Any*) =
        macro scala.reflect.internal.QuasiQuoteMacro.apply
    }
  }
}

case class QuasiQuoteException(msg: String) extends Exception(msg)

package scala.tools.reflect
package quasiquotes

import scala.tools.nsc.ast.parser.{Parsers => ScalaParser}
import scala.compat.Platform.EOL
import scala.reflect.internal.util.{BatchSourceFile, SourceFile}


abstract class Parser extends ScalaParser {
  import global._

  def parse(code: String): Tree = {
    val wrappedCode = "object wrapper {" + EOL + code + EOL + "}"
    val file = new BatchSourceFile("<quasiquotes>", wrappedCode)
    val wrappedTree = new QuasiQuoteParser(file).parse()
    val PackageDef(_, List(ModuleDef(_, _, Template(_, _, _ :: parsed)))) = wrappedTree
    parsed match {
      case tree :: Nil => tree
      case stats :+ tree => Block(stats, tree)
    }
  }

  class QuasiQuoteParser(source0: SourceFile) extends SourceFileParser(source0) {
    override def allowTypelessParams = true
  }
}

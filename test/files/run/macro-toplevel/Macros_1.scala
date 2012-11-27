import scala.reflect.macros.Context
import language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val msg = "I've been created from " + c.macroApplication
    val Block(List(synthetic: ClassDef), _) = reify{ class SomeUniqueName { def hello = c.literal(msg).splice } }.tree
    if (!c.existsTopLevel(synthetic.name)) c.introduceTopLevel(synthetic)
    c.Expr[String](Select(Apply(Select(New(Ident(synthetic.name)), nme.CONSTRUCTOR), List()), newTermName("hello")))
  }

  def foo = macro impl
  def foo2 = macro impl
}

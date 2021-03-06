import scala.reflect.macros.blackbox.Context
import language.experimental.macros

object MyAttachment

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val ident = Ident(TermName("bar")) updateAttachment MyAttachment
    assert(ident.attachments.get[MyAttachment.type].isDefined, ident.attachments)
    val typed = c.typecheck(ident)
    assert(typed.attachments.get[MyAttachment.type].isDefined, typed.attachments)
    c.Expr[Int](typed)
  }

  def foo = macro impl
}
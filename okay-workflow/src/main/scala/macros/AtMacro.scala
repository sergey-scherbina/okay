package okay
package macros

import scala.quoted.*

/** THE MACRO BEHIND `Wf.At.here`: the call site's `File.scala:line`, a compile-time constant (okay-freer's `At`) */
object AtMacro:

  def here(using Quotes): Expr[Wf.At] =
    import quotes.reflect.*
    val pos = Position.ofMacroExpansion
    val name =
      try pos.sourceFile.name
      catch case _: Throwable => "<unknown>"
    val where = Expr(s"$name:${pos.startLine + 1}")
    '{ Wf.At($where) }
